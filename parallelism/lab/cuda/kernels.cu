#include <math.h>
#include <float.h>
#include <cuda.h>

__global__ void gpu_Heat (float *dev_u, float *dev_uhelp, int N) {

  //extern __shared__ float sdata[]; // lifetime of the block

  //int tid = threadIdx.y*blockDim.x + threadIdx.x;

  int col = (blockIdx.x * blockDim.x) + threadIdx.x;
  int row = (blockIdx.y * blockDim.y) + threadIdx.y;

  //sdata[tid] = dev_u[row*N + col];
  //__syncthreads();

  if (row > 0 && row < (N-1) && col > 0 && col < (N-1)) {
    //dev_uhelp[row*N + col] = 0.25 * ( sdata[(threadIdx.y-1)*blockDim.x + threadIdx.x]
                                    //+ sdata[(threadIdx.y+1)*blockDim.x + threadIdx.x]
                                    //+ sdata[threadIdx.y*blockDim.x + (threadIdx.x - 1)]
                                    //+ sdata[threadIdx.y*blockDim.x + (threadIdx.x + 1)]
                                    //);
    dev_uhelp[row*N + col] = 0.25 * ( dev_u[row*N + (col-1)]
                                    + dev_u[row*N + (col+1)]
                                    + dev_u[(row-1)*N + col]
                                    + dev_u[(row+1)*N + col]
                                    );
  }
}

__global__ void gpu_Heat2 (float *dev_u, float *dev_uhelp, float *dev_r, int N) {

  int col = (blockIdx.x * blockDim.x) + threadIdx.x;
  int row = (blockIdx.y * blockDim.y) + threadIdx.y;

  float diff;

  if (row > 0 && row < (N-1) && col > 0 && col < (N-1)) {
    dev_uhelp[row*N + col] = 0.25 * ( dev_u[row*N + (col-1)]
                                    + dev_u[row*N + (col+1)]
                                    + dev_u[(row-1)*N + col]
                                    + dev_u[(row+1)*N + col]
                                    );
    diff = dev_uhelp[row*N + col] - dev_u[row*N + col];
    dev_r[row*N + col] = diff * diff;
  }
}
