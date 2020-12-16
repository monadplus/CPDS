# CUDA Assignment

To get who is using the cards in the node:

```bash
nvidia-smi
```

## Performance

Parameters:

```
25000    # iterations
256      # resolution (should be a power of 2)
16       # block size
```

### Reduce1

```
Time on GPU in ms. = 1487.472778 (11.464 GFlop => 7706.82 MFlop/s)
Convergence to residual=0.000050: 15901 iterations
```

### Reduce2

```
Time on GPU in ms. = 1331.161865 (11.464 GFlop => 8611.79 MFlop/s)
Convergence to residual=0.000050: 15901 iterations
```

### Reduce3

```
Time on GPU in ms. = 1278.862793 (11.464 GFlop => 8963.97 MFlop/s)
Convergence to residual=0.000050: 15901 iterations
```

### Reduce4

```
Time on GPU in ms. = 1150.255127 (11.464 GFlop => 9966.21 MFlop/s)
Convergence to residual=0.000050: 15901 iterations
```

### Reduce5

```
Time on GPU in ms. = 1094.976074 (11.464 GFlop => 10469.35 MFlop/s)
Convergence to residual=0.000050: 15901 iterations
```

### Reduce6

```
Time on GPU in ms. = 1105.981201 (11.464 GFlop => 10365.17 MFlop/s)
Convergence to residual=0.000050: 15901 iterations
```

### Reduce7

```
Time on GPU in ms. = 1118.528687 (11.464 GFlop => 10248.90 MFlop/s)
Convergence to residual=0.000050: 15901 iterations
```
