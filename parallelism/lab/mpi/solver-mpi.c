#include "heat.h"
#include <mpi.h>

#define min(a,b) ( ((a) < (b)) ? (a) : (b) )
#define NB 1

/*
 * Blocked Jacobi solver: one iteration step
 */
double relax_jacobi (double *u, double *utmp, unsigned sizex, unsigned sizey)
{
    double diff, sum=0.0;
    int nbx, bx, nby, by;

    /* Only one block */
    nbx = 1;
    bx = sizex/nbx;
    nby = 1;
    by = sizey/nby;
    for (int ii=0; ii<nbx; ii++)
        for (int jj=0; jj<nby; jj++)
            for (int i=1+ii*bx; i<=min((ii+1)*bx, sizex-2); i++)
                for (int j=1+jj*by; j<=min((jj+1)*by, sizey-2); j++) {
                    utmp[i*sizey+j]= 0.25 * (
                            u[i*sizey     + (j-1)]+  // left
                            u[i*sizey     + (j+1)]+  // right
                            u[(i-1)*sizey + j    ]+  // top
                            u[(i+1)*sizey + j    ]); // bottom

                    diff = utmp[i*sizey+j] - u[i*sizey + j];
                    sum += diff * diff;
                }

    return sum;
}

/*
 * Blocked Red-Black solver: one iteration step
 */
double relax_redblack (double *u, unsigned sizex, unsigned sizey)
{
    double unew, diff, sum=0.0;
    int nbx, bx, nby, by;
    int lsw;

    nbx = NB;
    bx = sizex/nbx;
    nby = NB;
    by = sizey/nby;
    // Computing "Red" blocks
    for (int ii=0; ii<nbx; ii++) {
        lsw = ii%2;
        for (int jj=lsw; jj<nby; jj=jj+2)
            for (int i=1+ii*bx; i<=min((ii+1)*bx, sizex-2); i++)
                for (int j=1+jj*by; j<=min((jj+1)*by, sizey-2); j++) {
                    unew= 0.25 * (    u[ i*sizey	+ (j-1) ]+  // left
                            u[ i*sizey	+ (j+1) ]+  // right
                            u[ (i-1)*sizey	+ j     ]+  // top
                            u[ (i+1)*sizey	+ j     ]); // bottom
                    diff = unew - u[i*sizey+ j];
                    sum += diff * diff;
                    u[i*sizey+j]=unew;
                }
    }

    // Computing "Black" blocks
    for (int ii=0; ii<nbx; ii++) {
        lsw = (ii+1)%2;
        for (int jj=lsw; jj<nby; jj=jj+2)
            for (int i=1+ii*bx; i<=min((ii+1)*bx, sizex-2); i++)
                for (int j=1+jj*by; j<=min((jj+1)*by, sizey-2); j++) {
                    unew= 0.25 * (    u[ i*sizey	+ (j-1) ]+  // left
                            u[ i*sizey	+ (j+1) ]+  // right
                            u[ (i-1)*sizey	+ j     ]+  // top
                            u[ (i+1)*sizey	+ j     ]); // bottom
                    diff = unew - u[i*sizey+ j];
                    sum += diff * diff;
                    u[i*sizey+j]=unew;
                }
    }

    return sum;
}

/*
 * Gauss-Seidel solver: one iteration step
 */
double relax_gauss (double *u, unsigned sizex, unsigned sizey)
{
    double unew, diff, sum=0.0;
    int nbx, bx, nby, by;

    /* Get MPI params */
    int myid, numprocs;
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &myid);
    MPI_Status status;

    /* Proportional to number of process */
    int NB2 = (numprocs + (numprocs % 2))*16; // Exact division

    /* Split in blocks horizontally */
    nbx = 1; // Only one row (Do not split vert.)
    bx = sizex/nbx;
    nby = NB2; // Split horiz.
    by = sizey/nby;

    //  ---+---+---+---+---+---+---+---
    // | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 |
    // |---+---+---+---+---+---+---+---|
    // | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
    // |---+---+---+---+---+---+---+---|
    // | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
    // |---+---+---+---+---+---+---+---|
    // | 3 | 4 | 5 | 6 | 7 | 8 | 9 |10 |
    //  ---+---+---+---+---+---+---+---

    for (int ii=0; ii<nbx; ii++) {
        for (int jj=0; jj<nby; jj++) {

            /* Receive top boundary from previous process */
            if (numprocs > 0 && ii == 0 && myid > 0)
                MPI_Recv(&u[jj*by], by, MPI_DOUBLE, myid-1, 0, MPI_COMM_WORLD, &status);

            /* Compute block */
            for (int i=1+ii*bx; i<=min((ii+1)*bx, sizex-2); i++)  {
                for (int j=1+jj*by; j<=min((jj+1)*by, sizey-2); j++) {

                    unew= 0.25 * (u[i*sizey + (j-1)]+  // left
                                  u[i*sizey + (j+1)]+  // right
                                  u[(i-1)*sizey	+ j]+  // top
                                  u[(i+1)*sizey	+ j]); // bottom

                    diff = unew - u[i*sizey+ j];
                    sum += diff * diff;
                    u[i*sizey+j]=unew;
                }
            }


            /* Send computed bottom row to next process top boundary */
            if (numprocs > 0 && ii == nbx - 1 && myid < numprocs - 1) {
                MPI_Send(&u[(sizex - 2)*sizey + jj*by], by, MPI_DOUBLE, myid+1, 0, MPI_COMM_WORLD);
            }

        }
    }

    return sum;
}

