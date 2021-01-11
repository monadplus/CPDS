#!/bin/bash
	# @ job_name		= heatmpi
	# @ partition		= debug
	# @ initialdir		= .
	# @ output		= heatmpi.%j.out
	# @ error		= heatmpi.%j.err
	# @ total_tasks		= 8
	# @ cpus_per_task	= 1
	# @ tasks_per_node	= 8
	# @ wall_clock_limit	= 00:02:00

prog=heatmpi

srun -n 1 ./$prog test.dat
srun -n 2 ./$prog test.dat
srun -n 3 ./$prog test.dat
srun -n 4 ./$prog test.dat
srun -n 5 ./$prog test.dat
srun -n 6 ./$prog test.dat
srun -n 7 ./$prog test.dat
srun -n 8 ./$prog test.dat
