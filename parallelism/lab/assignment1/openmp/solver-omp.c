#include "heat.h"

#define NB 8

#define min(a,b) ( ((a) < (b)) ? (a) : (b) )
#define max(a,b) ( ((a) > (b)) ? (a) : (b) )

/*
 * Blocked Jacobi solver: one iteration step
 */
double relax_jacobi (double *u, double *utmp, unsigned sizex, unsigned sizey)
{
    /*
     * Traverse the matrix  of size (sizex, sizey)
     * At each position,
     *    compute the average of the neighbors i.e. left, right, top, bottom.
     *    compute the difference with the average.
     *    sum the diff^2
     *
     * I was expecting good results from  giving to each thread a region of the matrix by
     *    nbx = omp_get_max_threads()/2
     *    nby = omp_get_max_threads()/2
     * so when collapsed the number of regions was omp_get_max_threads().
     *
     * But no, this make it worse.
     */
    double diff, sum=0.0;
    int nbx, bx, nby, by;

    nbx = omp_get_max_threads();
    bx = (sizex + nbx - 1)/nbx;
    nby = omp_get_max_threads();
    by = (sizey + nby - 1)/nby;

    #pragma omp parallel for private(diff) reduction(+:sum) collapse(2)
    for (int ii=0; ii<nbx; ii++)
        for (int jj=0; jj<nby; jj++)
            for (int i=1+ii*bx; i<=min((ii+1)*bx, sizex-2); i++)
                for (int j=1+jj*by; j<=min((jj+1)*by, sizey-2); j++) {
                  utmp[i*sizey+j] = 0.25 * (u[ i*sizey     + (j-1) ]+  // left
                                            u[ i*sizey     + (j+1) ]+  // right
                                            u[ (i-1)*sizey + j     ]+  // top
                                            u[ (i+1)*sizey + j     ]); // bottom
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

    nbx = omp_get_max_threads();
    bx = (sizex + nbx - 1)/nbx;
    nby = omp_get_max_threads();
    by = (sizey + nby - 1)/nby;

    // Computing "Red" blocks
    #pragma omp parallel for private(unew, diff, lsw) reduction(+:sum)
    for (int ii=0; ii<nbx; ii++) {
        lsw = ii%2;
        for (int jj=lsw; jj<nby; jj=jj+2) 
            for (int i=1+ii*bx; i<=min((ii+1)*bx, sizex-2); i++) 
                for (int j=1+jj*by; j<=min((jj+1)*by, sizey-2); j++) {
                    unew= 0.25 * ( u[ i*sizey     + (j-1) ]+  // left
                                   u[ i*sizey     + (j+1) ]+  // right
                                   u[ (i-1)*sizey + j     ]+  // top
                                   u[ (i+1)*sizey + j     ]); // bottom
                    diff = unew - u[i*sizey+ j];
                    sum += diff * diff;
                    u[i*sizey+j] = unew;
                }
    }

    // Computing "Black" blocks
    #pragma omp parallel for private(unew, diff, lsw) reduction(+:sum)
    for (int ii=0; ii<nbx; ii++) {
        lsw = (ii+1)%2;
        for (int jj=lsw; jj<nby; jj=jj+2) 
            for (int i=1+ii*bx; i<=min((ii+1)*bx, sizex-2); i++) 
                for (int j=1+jj*by; j<=min((jj+1)*by, sizey-2); j++) {
                    unew= 0.25 * ( u[ i*sizey     + (j-1) ]+  // left
                                   u[ i*sizey     + (j+1) ]+  // right
                                   u[ (i-1)*sizey + j     ]+  // top
                                   u[ (i+1)*sizey + j     ]); // bottom
                    diff = unew - u[i*sizey+ j];
                    sum += diff * diff;
                    u[i*sizey+j] = unew;
                }
    }

    return sum;
}

/*
 * Blocked Gauss-Seidel solver: one iteration step
 */
double relax_gauss (double *u, unsigned sizex, unsigned sizey)
{
    int n_threads = omp_get_max_threads();
    // The blocks depends on the left and upper block
    char deps[n_threads][n_threads]; // Dependecy matrix

    double unew, diff, sum=0.0;
    int nbx, bx, nby, by;

    nbx = nby = n_threads;
    bx = (sizex + nbx - 1)/nbx;
    by = (sizey + nbx - 1)/nby;

    #pragma omp parallel
    #pragma omp single
    for (int ii=0; ii<nbx; ii++) {
        for (int jj=0; jj<nby; jj++) {
                #pragma omp task shared(sum, u) private(unew, diff)                       \
                depend (in : deps[max(0,ii-1)][jj], deps[ii][max(0,jj-1)]) depend (out : deps[ii][jj])
            {
                double aux_sum=0.0;
                for (int i=1+ii*bx; i<=min((ii+1)*bx, sizex-2); i++) {
                    for (int j=1+jj*by; j<=min((jj+1)*by, sizey-2); j++) {
                        unew= 0.25 * ( u[ i*sizey     + (j-1) ]+  // left
                                       u[ i*sizey     + (j+1) ]+  // right
                                       u[ (i-1)*sizey + j     ]+  // top
                                       u[ (i+1)*sizey + j     ]); // bottom
                        diff = unew - u[i*sizey+ j];
                        aux_sum += diff * diff;
                        u[i*sizey+j]=unew;
                    }
                }
                #pragma omp atomic
                sum += aux_sum;
            }
        }
    }
    return sum;
}
