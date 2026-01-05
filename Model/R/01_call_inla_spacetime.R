source('./Model/R/99_load.R')


args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1])  
k <- as.numeric(args[2])


###These were run with logit_thermal_suitability_lag3
# all.mods <- list('mod1'=form1,'mod2'=form2,'mod3'=form3,'mod4'=form4,'mod5'=form5,'mod6'=form6,'mod7'=form7,'mod8'=form8,'mod9'=form9,'mod10'=form10,
#                  'mod11'=form11,'mod12'=form12,'mod13'=form13)



all.mods <- list( 'mod14'=mod7,'mod15'=mod8,'mod16'=mod9,'mod17'=mod10,'mod18'=mod11,'mod19'=mod12,
                  'mod20'=mod13,'mod21'=mod14,'mod22'=mod15,'mod23'=mod16,'mod24'=mod4,'mod25'=mod5,'mod26'=mod6,
                  'mod27'=form1,'mod28'=form2,'mod29'=form3,'mod30'=form4,'mod31'=form5,'mod32'=form6,'mod33'=form7,'mod34'=form8,'mod35'=form9,'mod36'=form10,
                                    'mod37'=form11,'mod38'=form12,'mod39'=form13,
                  'mod40'=form14,'mod41'=form15,'mod42'=form16,'mod43'=form17,'mod44'=form18
)
#j=1

#k=23


 modN_extract <- as.numeric(str_match(names(all.mods)[k], "mod(\\d+)")[1, 2])
 
  mod1 <- inla_spacetime_mod(vintage_date = date.test2[j], formula1 = all.mods[[k]], modN=modN_extract ) 


# #Or you can use for loop to run in your computer
# for (j in 1:length(date.test2)) {
#  for (k in 1:3) {
#     tryCatch({
#       modN_extract <- as.numeric(str_match(names(all.mods)[k], "mod(\\d+)")[1, 2])
#       mod1 <- inla_spacetime_mod(vintage_date = date.test2[j], formula1 = all.mods[[k]], modN = modN_extract)
#     }, error = function(e) {
#       message(paste("Skipping iteration j =", j, "k =", k, "due to error:", e$message))
#     })
#   }
# }


