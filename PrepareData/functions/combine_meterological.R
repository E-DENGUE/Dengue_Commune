combine_meteorological <- function(ds.list=month_ave){
    ds_mean <- bind_rows(ds.list) %>% 
      ungroup() %>%
      mutate(variable_name = paste0('mean_',variable_name)) %>%
      pivot_wider(. , id_cols=c(monthdate, commune_id), values_from=mean_value, names_from=variable_name) %>%
      mutate(commune_id = as.numeric(commune_id))
    
    ds_cum <- bind_rows(ds.list) %>% 
      ungroup() %>%
      mutate(variable_name = paste0('cum_',variable_name)) %>%
      pivot_wider(. , id_cols=c(monthdate, commune_id), values_from=cum_value, names_from=variable_name) %>%
      mutate(commune_id = as.numeric(commune_id))
    
    ds <- ds_cum %>%
      full_join(ds_mean, by=c('monthdate','commune_id'))
    
    #append to existing file as new data is added
    data_files <- list.files('./Data/', full.names=F) 
    
    if(grep("meteorological.csv.gz" ,data_files)) {
      existing_data <- vroom::vroom( './Data/meteorological.csv.gz' ) 
      
      ds <- bind_rows(ds,existing_data) %>%
        distinct() #remove duplicates
      
    }
    
    return(ds)

}