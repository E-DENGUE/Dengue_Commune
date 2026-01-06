#!/bin/bash
#SBATCH --time=10:00:00
#SBATCH --mail-type=ALL
#SBATCH --partition day,scavenge
#SBATCH --requeue
#SBATCH --mail-user=wa223@yale.edu
#SBATCH --cpus-per-task=4
#SBATCH --mem-per-cpu=1G
#SBATCH -o ./Report/combine_forecast.out # STDOUT
#SBATCH -e ./Report/combine_forecast.out # STDOUT

#Define the number of models being tested


#Load R
module load  R/4.2.3-foss-2022b



Rscript ./Model/R/04_combine_forecast.R 