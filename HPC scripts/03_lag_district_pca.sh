#!/bin/bash
#SBATCH --time=2:00:00
#SBATCH --mail-type=ALL
#SBATCH --partition day,scavenge
#SBATCH --requeue
#SBATCH --mail-user=wa223@yale.edu
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=10G
#SBATCH -o ./Report/output03/output03.%a.out # STDOUT
#SBATCH -e ./Report/error03/error03.%a.out 
#SBATCH --array=1-4572# If k models and J hold out time points this is 1- j*k  J=36 times, k=127 districts

#Define the number of models being tested

N_models=1
K=127
J=36


#Load R
module load  R/4.2.3-foss-2022b

# J:1-36 time periods
# K 1:N_districts 

# Use modulos to iterate through all task IDs
# Use modulos to calculate i, j, and k
task_id=$SLURM_ARRAY_TASK_ID
i=$(( (task_id - 1) / (J * K) + 1 )) # Calculate i based on the number of models, J, and K
remainder=$(( (task_id - 1) % (J * K) )) # Calculate the remainder to find j and k
j=$(( remainder / K + 1 )) # Calculate j based on the remainder and K
k=$(( remainder % K + 1 )) # Calculate k based on the remainder

# Run your R script with the task-specific J and K
Rscript ./Model/R/03_call_lag_district_pca.R "$j" "$k" "$i"