#include <math.h>
#include <float.h>
#include <cuda.h>

__global__ void gpu_Heat0 (float *dev_u, float *dev_uhelp, float *dev_r, int N) {

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

__global__ void gpu_Residuals0 (float *in, float *out, int N) {

  extern __shared__ float sdata[];

  unsigned int tid = threadIdx.x;
  unsigned int i = blockIdx.x*blockDim.x + threadIdx.x;

  if (i < N) {

    sdata[tid] = in[i];
    __syncthreads();

    for(unsigned int s=1; s < blockDim.x && i+s < N; s *= 2) {
      if (tid % (2*s) == 0) {
        sdata[tid] += sdata[tid + s];
      }
      __syncthreads();
    }

    if (tid == 0) out[blockIdx.x] = sdata[0];
  }
}
