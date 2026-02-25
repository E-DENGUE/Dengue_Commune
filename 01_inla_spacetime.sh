#!/bin/bash
#SBATCH --time=24:00:00
#SBATCH --mail-type=ALL
#SBATCH --partition day,scavenge
#SBATCH --requeue
#SBATCH --mail-user=wong@mpiib-berlin.mpg.de
#SBATCH --cpus-per-task=30
#SBATCH --mem-per-cpu=10G
#SBATCH -o ./Report/output01/output01.%a.out # STDOUT
#SBATCH -e ./Report/error01/error01.%a.out 
#SBATCH --array=1-288   # If k models and J hold out time points this is 1- j*k  J=72, K=4


#Define the number of models being tested

N_models=4

#Load R
# module load R/4.2.0-foss-2020b
module load R/4.4.1-foss-2022b
# module load R-INLA/24.01.18-foss-2022b

# # Force INLA to use the working launcher (bypasses missing inla.mkl)
# export INLA_CALL=/vast/palmer/apps/avx2/software/R-INLA/24.01.18-foss-2022b/INLA/bin/linux/64bit/inla.run


# J:1-72 time periods
# K 1:N_models models

# Use modulos to iterate through all task IDs
task_id=$SLURM_ARRAY_TASK_ID
j=$(( (task_id-1)  / N_models + 1 )) # $(( )) does arithmetic evaluation; Bash performs integer division so floor() is default
k=$(( task_id  % N_models  + 1 )) # $(( )) does arithmetic evaluation

# Start time
start_time=$(date +"%Y-%m-%d %H:%M:%S")

# Run your R script with the task-specific J and K
Rscript ./R/01_call_inla_spacetime.R "$j" "$k"

# End time
end_time=$(date +"%Y-%m-%d %H:%M:%S")

# Calculate running time
start_seconds=$(date -d "$start_time" +%s)
end_seconds=$(date -d "$end_time" +%s)
running_time=$((end_seconds - start_seconds))

# Output j, k, model number, and running time to a log file
echo "j=$j, k=$k, model_number=$task_id, running_time=$running_time seconds" >> ./Report/log1.txt
# done