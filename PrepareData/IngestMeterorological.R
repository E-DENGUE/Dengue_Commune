#This script reads in 

# Load required library
library(tidyverse)
library(pbapply)
library(arrow)

#Read in custom functions
all_functions <- list.files('../R/functions', full.names=T) 
lapply(all_functions, source)  #Read in all functions

#List ll files in the staging folder
filenames_full_path <-
  list.files("./Data/Staging_meteorological/", full.names = TRUE)

#Read in and clean the data into a tidy format
month_ave <- pbapply::pblapply(filenames_full_path, FUN=read_meterological)

month_ave2 <- combine_meteorological(month_ave)

#Save as compressed csv file, which compress ~3-fold
vroom::vroom_write(month_ave2, './Data/meteorological.csv.gz' )

#Move the staging files to an archive folder
lapply(filenames_full_path, FUN=move.file)
