/*
 * Iterative solver for heat distribution
 */

#include <mpi.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "heat.h"

void usage( char *s )
{
    fprintf(stderr, 
            "Usage: %s <input file> [result file]\n\n", s);
}

int main( int argc, char *argv[] )
{
    unsigned iter;
    FILE *infile, *resfile;
    char *resfilename;
    int myid, numprocs;
    MPI_Status status;
    double global_residual;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &myid);

    if (myid == 0) {
        printf("I am the master (%d) and going to distribute work to %d additional workers ...\n", myid, numprocs-1);

        // algorithmic parameters
        algoparam_t param;
        int np;

        double runtime, flop;
        double residual=0.0;

        // check arguments
        if( argc < 2 )
        {
            usage( argv[0] );
            return 1;
        }

        // check input file
        if( !(infile=fopen(argv[1], "r"))  ) 
        {
            fprintf(stderr, 
                    "\nError: Cannot open \"%s\" for reading.\n\n", argv[1]);

            usage(argv[0]);
            return 1;
        }

        // check result file
        resfilename= (argc>=3) ? argv[2]:"heat.ppm";

        if( !(resfile=fopen(resfilename, "w")) )
        {
            fprintf(stderr, 
                    "\nError: Cannot open \"%s\" for writing.\n\n", 
                    resfilename);
            usage(argv[0]);
            return 1;
        }

        // check input
        if( !read_input(infile, &param) )
        {
            fprintf(stderr, "\nError: Error parsing input file.\n\n");
            usage(argv[0]);
            return 1;
        }
        print_params(&param);

        // set the visualization resolution

        param.u     = 0;
        param.uhelp = 0;
        param.uvis  = 0;
        param.visres = param.resolution;

        if( !initialize(&param) )
        {
            fprintf(stderr, "Error in Solver initialization.\n\n");
            usage(argv[0]);
            return 1;
        }

        // full size (param.resolution are only the inner points)
        np = param.resolution + 2;

        // starting time
        runtime = wtime();

        // send each worker a set of consecutive rows
        int remaining_rows = param.resolution % numprocs; // Not divisible by number of workers
        int rows = (param.resolution - remaining_rows)/numprocs; // Computable rows / Worker
        int columns = param.resolution; // Computable colums / Worker
        if (remaining_rows > 0) {
            fprintf(stdout, "First worker has %d extra row/s\n", remaining_rows);
        }

        int np_col = columns + 2; // Add {first,last} column boundaries
        int np_row = rows + 2; // Add {first,last} row boundaries
        int qtt = np_col * np_row; // Total data to transfer to each worker

        fprintf(stdout, "Splitting matrix: %dx%d to {%d} workers: %d,%d\n",
                param.resolution,
                param.resolution,
                numprocs,
                np_row,
                np_col
               );

        for (int i=1; i<numprocs; i++) {
            // Bcast & Scatter?
            MPI_Send(&param.maxiter, 1, MPI_INT, i, 0, MPI_COMM_WORLD);
            MPI_Send(&np_row, 1, MPI_INT, i, 0, MPI_COMM_WORLD);
            MPI_Send(&np_col, 1, MPI_INT, i, 0, MPI_COMM_WORLD);
            MPI_Send(&param.algorithm, 1, MPI_INT, i, 0, MPI_COMM_WORLD);
            MPI_Send(&param.u[np_col * (i*(np_row - 2) + remaining_rows)], qtt, MPI_DOUBLE, i, 0, MPI_COMM_WORLD);
            MPI_Send(&param.uhelp[np_col *(i*(np_row - 2) + remaining_rows)], qtt, MPI_DOUBLE, i, 0, MPI_COMM_WORLD);
            fprintf(stdout, "Sending rows %d -> %d to %d\n",
                    i*(np_row - 2) + remaining_rows,
                    i*(np_row - 2) + remaining_rows + np_row,
                    i);
        }

        iter = 0;

         // Last computed row position in matrix u
        double *const b_computed = &param.u[(np_row-2)*np_col + remaining_rows*np_col];
         // Bottom boundary position in matrix u
        double *const b_boundary = &param.u[(np_row-1)*np_col + remaining_rows*np_col];

        while(1) {
            switch( param.algorithm ) {
                case 0: // JACOBI
                    residual = relax_jacobi(param.u, param.uhelp, np_row + remaining_rows, np_col);
                    memcpy(param.u, param.uhelp, (np_row + remaining_rows)*np_col*sizeof(double));

                    if (numprocs > 1) {
                        MPI_Send(b_computed, np_col, MPI_DOUBLE, myid+1, 0, MPI_COMM_WORLD);
                        MPI_Recv(b_boundary, np_col, MPI_DOUBLE, myid+1, 0, MPI_COMM_WORLD, &status);
                    }

                    break;
                case 1: // RED-BLACK
                    residual = relax_redblack(param.u, np_row, np_col);
                    break;
                case 2: // GAUSS
                    residual = relax_gauss(param.u, np_row + remaining_rows, np_col);

                    if (numprocs > 1) {
                        MPI_Recv(b_boundary, np_col, MPI_DOUBLE, myid+1, 0, MPI_COMM_WORLD, &status);
                    }

                    break;
            }

            iter++;

            MPI_Allreduce(&residual, &global_residual, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);
            
            // solution good enough ?
            if (global_residual < 0.00005) break;

            // max. iteration reached ? (no limit with maxiter=0)
            if (iter>0 && iter>=param.maxiter) break;
        }

        /* Receive matrix from all workers */
        double *w_position;
        for (int i = 1; i < numprocs; i++) {
            w_position = &param.u[i*np_col*(np_row - 2) + np_col*remaining_rows];
            MPI_Recv(w_position, qtt, MPI_DOUBLE, i, 0, MPI_COMM_WORLD, &status);
            fprintf(stdout, "[%d] Rec the data from [%d]\n", myid, i);
        }

        // Flop count after iter iterations
        flop = iter * 11.0 * param.resolution * param.resolution;
        // stopping time
        runtime = wtime() - runtime;

        fprintf(stdout, "Time: %04.3f ", runtime);
        fprintf(stdout, "(%3.3f GFlop => %6.2f MFlop/s)\n", 
                flop/1000000000.0,
                flop/runtime/1000000);
        fprintf(stdout, "Convergence to residual=%f: %d iterations\n", residual, iter);

        // for plot...
        coarsen( param.u, np, np,
                param.uvis, param.visres+2, param.visres+2 );

        write_image( resfile, param.uvis,  
                param.visres+2, 
                param.visres+2 );

        finalize( &param );

        fprintf(stdout, "Process %d finished computing with residual value = %f\n", myid, residual);

        MPI_Finalize();

        return 0;

    } else {

        printf("I am worker %d and ready to receive work to do ...\n", myid);

        // receive information from master to perform computation locally

        int cols, rows;
        int iter, maxiter;
        int algorithm;
        double residual;

        MPI_Recv(&maxiter, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);
        MPI_Recv(&rows, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);
        MPI_Recv(&cols, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);
        MPI_Recv(&algorithm, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);

        int qtt = rows * cols;

        // allocate memory for worker
        double * u = calloc(qtt, sizeof(double));
        double * uhelp = calloc(qtt, sizeof(double));
        if( (!u) || (!uhelp) )
        {
            fprintf(stderr, "Error: Cannot allocate memory\n");
            return 0;
        }

        // fill initial values for matrix with values received from master
        MPI_Recv(&u[0], qtt, MPI_DOUBLE, 0, 0, MPI_COMM_WORLD, &status);
        MPI_Recv(&uhelp[0], qtt, MPI_DOUBLE, 0, 0, MPI_COMM_WORLD, &status);

        fprintf(stdout, "[%d] Matrix: %dx%d received\n", myid, rows, cols);

        iter = 0;
        while(1) {
            switch( algorithm ) {
                case 0: // JACOBI
                    residual = relax_jacobi(u, uhelp, rows, cols);
                    memcpy(u, uhelp, qtt*sizeof(double));

                    /* Interchange first rows, send computed, receive boundary from previous worker */
                    MPI_Recv(&u[0*cols], cols, MPI_DOUBLE, myid-1, 0, MPI_COMM_WORLD, &status);
                    MPI_Ssend(&u[1*cols], cols, MPI_DOUBLE, myid-1, 0, MPI_COMM_WORLD);

                    if (myid < numprocs - 1) { /* If I'm not the last process [bottom part] */
                        /* Send last computed row, receive bottom boundary from next worker */
                        MPI_Ssend(&u[(rows-2)*cols], cols, MPI_DOUBLE, myid+1, 0, MPI_COMM_WORLD);
                        MPI_Recv(&u[(rows-1)*cols], cols, MPI_DOUBLE, myid+1, 0, MPI_COMM_WORLD, &status);
                    }
                    break;
                case 1: // RED-BLACK
                    residual = relax_redblack(u, rows, cols);
                    break;
                case 2: // GAUSS
                    residual = relax_gauss(u, rows, cols);
                    
                    /* Send/Receive bottom boundary from next process (myid > 0) */
                    MPI_Ssend(&u[cols], cols, MPI_DOUBLE, myid-1, 0, MPI_COMM_WORLD);
                    if (myid < numprocs - 1)
                        MPI_Recv(&u[(rows-1)*cols], cols, MPI_DOUBLE, myid+1, 0, MPI_COMM_WORLD, &status);
                    break;
            }

            iter++;

            /* Update global residual */
            MPI_Allreduce(&residual, &global_residual, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);

            // solution good enough ?
            if (global_residual < 0.00005)
                break;

            // max. iteration reached ? (no limit with maxiter=0)
            if (maxiter>0 && iter>=maxiter)
                break;
        }

        /* Send computed area to master */
        MPI_Send(&u[0], qtt, MPI_DOUBLE, 0, 0, MPI_COMM_WORLD);

        if (u) free(u);
        if (uhelp) free(uhelp);

        fprintf(stdout, "Process %d finished computing %d iterations with residual value = %f\n", myid, iter, global_residual);

        MPI_Finalize();
        exit(0);
    }
}
