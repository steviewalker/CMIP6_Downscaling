#' @title Calculate climatology based on mean
#' @description  Take regridded nc file from ScenarioMIP CMIP6 regridded using regrid_ncfiles.R
#' @details INPUT: 1) regridded nc file
#' @details OUTPUT: 1) Climatology for 2 periods 2020-2050, 2070-2100
#' @details Used code from climate_change_statistics.R
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


anomaly_nc <- function(this.model, models.cimp6, historical.files, model.vars){
 
  print(this.model) 
  
  this.model.files <- models.cimp6 %>% 
       grep(this.model,., value = TRUE)
  
  this.historical.files <- historical.files %>% 
  grep(this.model,., value = TRUE)
  
  lapply(model.vars, calc_anomaly, this.model.files, this.historical.files)

  }

calc_anomaly <- function(this.model.var, this.model.files,this.historical.files){
  
  print(this.model.var)
  
  model.var.historical <- this.historical.files %>%
    grep(this.model.var,., value = TRUE)
  
  model.var.scenarios <- this.model.files %>%
    grep(this.model.var,., value = TRUE)
 
  for(eachfile in model.var.scenarios){
 
    new.name <- gsub("mean","anomaly",eachfile)
    
    setwd(data.path)
    
    test.file<-file.exists(new.name)
    
    if(test.file==FALSE)
    {
      
      system(paste("ncbo -y sbt -O ",eachfile," ", model.var.historical," ",new.name,sep=""),wait = TRUE)  
      
      #move delta anomaly file to separate directory
      file.rename(from = file.path(data.path, new.name), to = file.path(save.path, new.name))
      
    } # close test.file == FALSE conditional loop
    
    
  }
  
}
  
  



