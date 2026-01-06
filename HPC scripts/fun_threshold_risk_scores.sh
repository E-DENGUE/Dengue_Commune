#!/bin/bash
#SBATCH --time=24:00:00
#SBATCH --mail-type=ALL
#SBATCH --partition day,scavenge
#SBATCH --requeue
#SBATCH --mail-user=wa223@yale.edu
#SBATCH --cpus-per-task=30
#SBATCH --mem-per-cpu=10G
#SBATCH -o ./Report/fun_threshold_risk_scores.out # STDOUT
#SBATCH -e ./Report/fun_threshold_risk_scores.out # STDOUT

#Define the number of models being tested


#Load R
module load  R/4.2.3-foss-2022b



Rscript ./Output/fun_threshold_risk_scores.R 
