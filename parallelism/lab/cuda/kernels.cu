#include <math.h>
#include <float.h>
#include <cuda.h>

__global__ void gpu_Heat0 (float *dev_u, float *dev_uhelp, float *dev_r, int N) {

  int col = (blockIdx.x * blockDim.x) + threadIdx.x;
  int row = (blockIdx.y * blockDim.y) + threadIdx.y;

  float diff = 0.0;

  if (row > 0 && row < N-1 && col > 0 && col < N-1) {
    dev_uhelp[row*N + col] = 0.25 * ( dev_u[row*N + (col-1)]
                                    + dev_u[row*N + (col+1)]
                                    + dev_u[(row-1)*N + col]
                                    + dev_u[(row+1)*N + col]
                                    );
    diff = dev_uhelp[row*N + col] - dev_u[row*N + col];
    dev_r[(row-1)*(N-2) + col - 1] = diff * diff;
  }
}

// =============================================================

// Check bounds: this version allows the last block to be half empty.
__global__ void gpu_Reduce0 (float *in, float *out, int N) {

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

// =============================================================

__global__ void gpu_Reduce1 (float *in, float *out) {

  extern __shared__ float sdata[];

  unsigned int tid = threadIdx.x;
  unsigned int i = blockIdx.x*blockDim.x + threadIdx.x;

  sdata[tid] = in[i];
  __syncthreads();

  for(unsigned int s=1; s < blockDim.x; s *= 2) {
    if (tid % (2*s) == 0) {
      sdata[tid] += sdata[tid + s];
    }
    __syncthreads();
  }

  if (tid == 0) out[blockIdx.x] = sdata[0];
}

// =============================================================

// Divergency in warps removed
// Remove % (slow)
__global__ void gpu_Reduce2 (float *in, float *out) {

  extern __shared__ float sdata[];

  unsigned int tid = threadIdx.x;
  unsigned int i = blockIdx.x*blockDim.x + threadIdx.x;

  sdata[tid] = in[i];
  __syncthreads();

  for(unsigned int s=1; s < blockDim.x; s *= 2) {
    int index = 2*s*tid;
    if (index < blockDim.x) {
      sdata[index] += sdata[index + s];
    }
    __syncthreads();
  }

  if (tid == 0) out[blockIdx.x] = sdata[0];
}

// =============================================================

// 32 banks of 64-bits (or 32-bits mode).
//
// if two addresses of a memory request fall in the same memory bank, there is a bank conflict and the access has to be serialized.
//
// A shared memory request for a warp does not generate a bank conflict between two threads that access any sub-word within the same 64-bit word (even though the addresses of the two sub-words fall in the same bank).
__global__ void gpu_Reduce3 (float *in, float *out) {

  extern __shared__ float sdata[];

  unsigned int tid = threadIdx.x;
  unsigned int i = blockIdx.x*blockDim.x + threadIdx.x;

  sdata[tid] = in[i];
  __syncthreads();

  for(unsigned int s=blockDim.x*0.5; s>0; s>>=1) {
    if (tid < s) {
      sdata[tid] += sdata[tid+s];
    }
    __syncthreads();
  }

  if (tid == 0) out[blockIdx.x] = sdata[0];
}

// =============================================================

// First add, half of the threads where idle.
// This version is the first one to match the cpu output.
// NOTE: you need to halve the #blocks on the call.
__global__ void gpu_Reduce4 (float *in, float *out) {

  extern __shared__ float sdata[];

  unsigned int tid = threadIdx.x;
  unsigned int i = blockIdx.x*(blockDim.x*2) + threadIdx.x;

  // Each block processes two blocks
  sdata[tid] = in[i] + in[i+blockDim.x];
  __syncthreads();

  // Now we have half the blocks.
  for(unsigned int s=blockDim.x*0.5; s>0; s>>=1) {
    if (tid < s) {
      sdata[tid] += sdata[tid+s];
    }
    __syncthreads();
  }

  if (tid == 0) out[blockIdx.x] = sdata[0];
}

// =============================================================

// These optimizations can be disabled using the volatile keyword: If a variable located in global or shared memory is declared as volatile, the compiler assumes that its value can be changed or used at any time by another thread and therefore any reference to this variable compiles to an actual memory read or write instruction.
__device__ void warpReduce0(volatile float* sdata, int tid) {
  // Warps: between instructions there is an implicit barrier
  sdata[tid] += sdata[tid + 32];
  sdata[tid] += sdata[tid + 16];
  sdata[tid] += sdata[tid +  8];
  sdata[tid] += sdata[tid +  4];
  sdata[tid] += sdata[tid +  2];
  sdata[tid] += sdata[tid +  1];
}

// Warps are synchronized 32 threads.
// Unroll last iterations where threads < 32
__global__ void gpu_Reduce5 (float *in, float *out) {

  extern __shared__ float sdata[];

  unsigned int tid = threadIdx.x;
  unsigned int i = blockIdx.x*(blockDim.x*2) + threadIdx.x;

  sdata[tid] = in[i] + in[i+blockDim.x];
  __syncthreads();

  // Saves work:
  //   All warps execute every iteration of the loop
  //    and the if statement.
  for(unsigned int s=blockDim.x*0.5; s>32; s>>=1) {
    if (tid < s) {
      sdata[tid] += sdata[tid+s];
    }
    __syncthreads();
  }

  if (tid < 32) warpReduce0(sdata, tid);

  if (tid == 0) out[blockIdx.x] = sdata[0];
}

// =============================================================

// Loop unrolling: templates are evaluated at compile time.

template <unsigned int blockSize>
    __device__ void warpReduce1(volatile float* sdata, int tid) {
  if (blockSize >= 64) sdata[tid] += sdata[tid + 32];
  if (blockSize >= 32) sdata[tid] += sdata[tid + 16];
  if (blockSize >= 16) sdata[tid] += sdata[tid +  8];
  if (blockSize >= 8)  sdata[tid] += sdata[tid +  4];
  if (blockSize >= 4)  sdata[tid] += sdata[tid +  2];
  if (blockSize >= 2)  sdata[tid] += sdata[tid +  1];
}

template <unsigned int blockSize>
    __global__ void gpu_Reduce6 (float *in, float *out) {

  extern __shared__ float sdata[];

  unsigned int tid = threadIdx.x;
  unsigned int i = blockIdx.x*(blockDim.x*2) + threadIdx.x;

  sdata[tid] = in[i] + in[i+blockDim.x];
  __syncthreads();

  if (blockSize >= 512) {
    if (tid < 256) {sdata[tid] += sdata[tid + 256];} __syncthreads(); }
  if (blockSize >= 256) {
    if (tid < 128) {sdata[tid] += sdata[tid + 128];} __syncthreads(); }
  if (blockSize >= 128) {
    if (tid < 64) {sdata[tid] += sdata[tid + 64];} __syncthreads(); }

  if (tid < 32) warpReduce1<blockSize>(sdata, tid);

  if (tid == 0) out[blockIdx.x] = sdata[0];
}

// =============================================================

// Multiple adds per thread

// NOTE: you need to reduce the number of blocks
template <unsigned int blockSize>
    __global__ void gpu_Reduce7 (float *in, float *out, unsigned int N) {

  extern __shared__ float sdata[];

  unsigned int tid = threadIdx.x;
  unsigned int i = blockIdx.x*(blockDim.x*2) + threadIdx.x;
  unsigned int gridSize = blockSize*2*gridDim.x; // grid = #blocks
  sdata[tid] = 0;

  while (i < N) {
    sdata[tid] += in[i] + in[i+blockSize];
    i += gridSize;
  }

  __syncthreads();

  if (blockSize >= 512) {
    if (tid < 256) {sdata[tid] += sdata[tid + 256];} __syncthreads(); }
  if (blockSize >= 256) {
    if (tid < 128) {sdata[tid] += sdata[tid + 128];} __syncthreads(); }
  if (blockSize >= 128) {
    if (tid < 64) {sdata[tid] += sdata[tid + 64];} __syncthreads(); }

  if (tid < 32) warpReduce1<blockSize>(sdata, tid);

  if (tid == 0) out[blockIdx.x] = sdata[0];
}
