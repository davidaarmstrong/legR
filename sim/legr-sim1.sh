#!/bin/bash
#SBATCH --account=def-darmst46
#SBATCH --time=0:30:00
#SBATCH --mem-per-cpu=4096MB
module load StdEnv/2020 gcc/9.3.0 r/4.0.0
R CMD BATCH "sim1.r"
