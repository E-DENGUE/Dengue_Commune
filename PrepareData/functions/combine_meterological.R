combine_meteorological <- function(ds.list=month_ave){
    ds <- bind_rows(ds.list) %>% 
      ungroup() %>%
      pivot_wider(. , id_cols=c(monthdate, commune_id), values_from=mean_value, names_from=variable_name) %>%
      mutate(commune_id = as.numeric(commune_id))
    
    #append to existing file as new data is added
    data_files <- list.files('./Data/', full.names=F) 
    
    if(grep("meteorological.csv.gz" ,data_files)) {
      existing_data <- vroom::vroom( './Data/meteorological.csv.gz' ) 
      
      ds <- bind_rows(ds,existing_data) %>%
        distinct() #remove duplicates
      
    }
    
    return(ds)

}