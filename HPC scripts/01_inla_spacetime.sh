#!/bin/bash
#SBATCH --time=24:00:00
#SBATCH --mail-type=ALL
#SBATCH --partition day,scavenge
#SBATCH --requeue
#SBATCH --mail-user=wa223@yale.edu
#SBATCH --cpus-per-task=30
#SBATCH --mem-per-cpu=10G
#SBATCH -o ./Report/output01/output01.%a.out # STDOUT
#SBATCH -e ./Report/error01/error01.%a.out 
#SBATCH --array=1-1000   # If k models and J hold out time points, this is 1- j*k  

#Define the number of models being tested


N_models=13

#Load R
module load  R/4.2.3-foss-2022b

# J:1-72 time periods
# K 1:N_models models

# Use modules to iterate through all task IDs
task_id=$SLURM_ARRAY_TASK_ID
j=$(( (task_id-1)  / N_models + 1 )) # $(( )) does arithmetic evaluation; Bash performs integer division so floor() is default
k=$(( task_id  % N_models  + 1 )) # $(( )) does arithmetic evaluation



# Run the R script with the task-specific J and K
Rscript ./Model/R/01_call_inla_spacetime.R   "$j" "$k"







