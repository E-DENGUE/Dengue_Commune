# How to use

## Meterological data

### Step 1
Save raw meterological files (daily data, by commune) in Data/Staging_meterological

## Step 2
Run file IngestMeterological.R
This file reads in any files in Data/Staging_meterological, puts them in a tidy format, combines them together and appends them to already formatted data in Data/meterological.csv.gz . It then moves the files from the staging folder to the archive folder

