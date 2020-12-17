# OpenMP Notes

## schedules

### schedule(static)

p threads
n iterations
q = ceiling(n/p) such that n = p*q-r

Impl 1. chunk_size = q
Impl 2. chunk_size = q (first p - r threads)
        chunk_size = q - 1 (remaining r threads)

### schedule(static, chunk_size)

n/chunk_size and assigned to threads in round-robin fashion.

### schedule(dynamic, chunk_size = 1)

First come first serve. 
A thread is given chunk_size each time.

### schedule(guided, chunk_size = 1)

First come first serve. 

Chunk size = rem_iterations/threads, decreasing to chunk_size.
