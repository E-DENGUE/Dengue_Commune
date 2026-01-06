#!/bin/bash
#SBATCH --time=01:04:00
#SBATCH --mail-type=ALL
#SBATCH --partition day,scavenge
#SBATCH --requeue
#SBATCH --mail-user=wa223@yale.edu
#SBATCH --cpus-per-task=4
#SBATCH --mem-per-cpu=1G
#SBATCH -o ./Report/output02/output02.%a.out # STDOUT
#SBATCH -e ./Report/error02/error02.%a.out 
#SBATCH --array=1-36  # If k models and J hold out time points this is 1- j*k  J=84, K=15


#Define the number of models being tested

N_models=1

#Load R
module load  R/4.2.3-foss-2022b

# J:1-36 time periods
# K 1:N_models models

# Use modulos to iterate through all task IDs
task_id=$SLURM_ARRAY_TASK_ID
j=$(( (task_id-1)  / N_models + 1 )) # $(( )) does arithmetic evaluation; Bash performs integer division so floor() is default
k=$(( task_id  % N_models  + 1 )) # $(( )) does arithmetic evaluation


# Run the R script with the task-specific J and K
Rscript ./Model/R/02_call_hhh4.R "$j" "$k"

