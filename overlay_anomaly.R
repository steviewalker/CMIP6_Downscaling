#' @title Overlay interpolated anomalies to ROMS time-series
#' @description  Take regridded nc file from ScenarioMIP CMIP6 regridded using regrid_ncfiles.R
#' @details INPUT: 1) .asc file of anomalies interpolated to 0.1km
#' @details OUTPUT: 1) Climatology for 2 periods 2020-2050, 2070-2100
#' @details Used code from climate_change_statistics.R
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com


overlay_roms <- function(this.file, PugetSoundAtlantisPolygons, AtlantisBoxInfo, 
                         atlantis.temp.roms, atlantis.salt.roms, data.path, save.path){
  print(this.file)
  out.file <- gsub("PugetSound.asc","roms.nc",this.file)
  print(out.file)
  setwd(save.path)
  test.file <- file.exists(out.file)
  
  if(test.file==FALSE){
    
  setwd(data.path)
  
  #this.file <- "sos_Omon_CNRM-CM6-1-HR_ssp245_r1i1p1f2_anomaly_207001-210012PugetSound.asc"
  this.var <- this.file %>% 
    str_split(.,"_") %>% 
    unlist %>% 
    .[1]
    
  interpolatedRaster <- raster(this.file)
  crs(interpolatedRaster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  #plot(interpolatedRaster)
  print("masking raster")
  #ps.buffer <- gBuffer(PugetSoundAtlantisPolygons,width=c(0.1))
  mask.raster <- mask(interpolatedRaster,PugetSoundAtlantisPolygons)
  #plot(mask.raster)
  #plot(PugetSoundAtlantisPolygons, add=TRUE)
  
  print("extracting anomalies")
  extractedAnomaliesOnPolygons <- extract(mask.raster,PugetSoundAtlantisPolygons,fun=max,na.rm=TRUE,df=TRUE, sp=TRUE)  #df says return as dataframe
  
  anomaly.data.table <- extractedAnomaliesOnPolygons %>% 
    as.data.frame() %>% 
    mutate(scenario=this.file) %>% 
    separate(scenario,
             into=c("variable",
                    "realm",
                    "model",
                    "experiment",
                    "realisation",
                    "stat",
                    "date"),
             sep = "_") %>% 
    mutate(date=gsub("PugetSound.asc","",date)) %>%
    mutate(BOX_ID=as.numeric(as.character(BOX_ID))) %>% arrange(BOX_ID)
  
  names.table <- names(anomaly.data.table)
  
  names.table[11] <- "raster_value"
  
  anomaly.data.table.name <- setNames(anomaly.data.table, names.table)
  
  # Note anomaly.table.name is nicely sorted by BOX_ID
  
  numAtlantisBoxes <- length(unique(AtlantisBoxInfo$box_id))
  
  if(this.var=="tos")
    {
    #----------------------------
    
    # extract the actual temperature variable. 
    # It is depth layers X plygons X timesteps. 
    this.temperatureArray <- ncvar_get(temp.roms.nc,"temperature")
    temp.roms.nc <-nc_open(paste0("~/Puget_Sound_Downscaling/roms_forcing/",atlantis.temp.roms))

        print("temperature file")
        # Loop over Atlantis boxes
    for (boxIndex in 1:numAtlantisBoxes)
    {
      print('boxIndex')
      print(boxIndex)
      # AtlantisBoxInfo$num_layers[boxIndex]
      #temperatureArray  has dims of     7   89 1460
      surfaceLayerIndex <- AtlantisBoxInfo$num_layers[boxIndex]
      # ACTUALLY REALLY ADDING THE ANOMALY TO THE OLD TEMPERATURE ARRAY . DELTA METHOD ! 
      this.temperatureArray[surfaceLayerIndex,boxIndex, ]<-this.temperatureArray[surfaceLayerIndex,boxIndex, ] +anomaly.data.table.name$raster_value[boxIndex]
      print(anomaly.data.table.name$raster_value[boxIndex] )  
      
    }   # end loop over Atlantis boxes
    
    #-----------
    
    #temperatureArray
    
    # NOW WILL NEED TO TAKE temperatureArray and RESAVE INTO nc FORMAT. 
    
    setwd(forcing.path)
    system(paste("nccopy", atlantis.temp.roms,out.file),wait=TRUE)
    
    roms.anomaly.nc <-nc_open(out.file, write=TRUE)
    
    ncvar_put(roms.anomaly.nc, "temperature", this.temperatureArray) #  start=NA, count=NA, verbose=FALSE ) 
    
    #open and save again with updated variable
    nc_close(roms.anomaly.nc)
    
    roms.anomaly.nc <-nc_open(out.file, write=TRUE)
    
    temperatureArrayInitial <- ncvar_get(temp.roms.nc,"temperature")
    temperatureArrayAnomalyAdded <- ncvar_get(roms.anomaly.nc,"temperature")
    
    
    #---------------------------------
    # Still checknig things... loop over all boxes and extract top layer only: 
    
    # So this is just checking and plotting the output here, 
    
    #-------------------------
    
    CompareTopLayers <- NULL # array(NA,dim=c(89,2,1460))
    
    num_Active_AtlantisBoxes <- 60
    
    for (boxIndex in 1:num_Active_AtlantisBoxes)
    {
      print('boxIndex')
      print(boxIndex)
      
      for (timestep in 1:1460)
      {
        print('timestep')
        print(timestep)
        # AtlantisBoxInfo$num_layers[boxIndex]
        #temperatureArray  has dims of     7   89 1460
        surfaceLayerIndex <- AtlantisBoxInfo$num_layers[boxIndex]
        
        #CompareTopLayers[boxIndex, 1,timestep]<-temperatureArrayInitial[surfaceLayerIndex,boxIndex,timestep ] 
        #CompareTopLayers[boxIndex, 2,timestep]<-temperatureArrayAnomalyAdded[surfaceLayerIndex,boxIndex,timestep ] 
        CompareTopLayers <-rbind(CompareTopLayers,cbind(temperatureArrayInitial[surfaceLayerIndex,boxIndex,timestep ], temperatureArrayAnomalyAdded[surfaceLayerIndex,boxIndex,timestep ], boxIndex,timestep))
        
      }
      
    } 
    
    print("compare anomalies")
    #plot(CompareTopLayers[,1], CompareTopLayers[,2])
    #View(CompareTopLayers)
    mean(CompareTopLayers[,1])
    #[1] 11.20254
    mean(CompareTopLayers[,2])
    #[1] 11.40549
    mean((CompareTopLayers[,2])/CompareTopLayers[,1])
    
    #-------------------------------------------------
    
    
    nc_close(temp.roms.nc)
    nc_close(roms.anomaly.nc)
    
    file.rename(from = file.path(forcing.path, out.file), to = file.path(save.path, out.file))
    
}  # END OF THE TEMPERATURE cONDITIONAL LOOP. 

  #-------------------------------
  
  # IF SALINITY , THEN .... 
  
  if(this.var=="sos"){

    salt.roms.nc <-nc_open(paste0("~/Puget_Sound_Downscaling/roms_forcing/",atlantis.salt.roms))
    #----------------------------
    # extract the actual temperature variable. 
    # It is depth layers X plygons X timesteps. 
    this.salinityArray <- ncvar_get(salt.roms.nc,"salinity")
    
    print("salinity file")    
    # Loop over Atlantis boxes
    for (boxIndex in 1:numAtlantisBoxes)
    {
      print('boxIndex')
      print(boxIndex)
      # AtlantisBoxInfo$num_layers[boxIndex]
      #temperatureArray  has dims of     7   89 1460
      surfaceLayerIndex <- AtlantisBoxInfo$num_layers[boxIndex]
      # ACTUALLY REALLY ADDING THE ANOMALY TO THE OLD TEMPERATURE ARRAY . DELTA METHOD ! 
      this.salinityArray[surfaceLayerIndex,boxIndex, ] <- this.salinityArray[surfaceLayerIndex,boxIndex, ] + anomaly.data.table.name$raster_value[boxIndex]
      print(anomaly.data.table.name$raster_value[boxIndex] )  
      
    }   # end loop over Atlantis boxes
    
    #-----------
    
    #salinityArray
    
    # NOW WILL NEED TO TAKE temperatureArray and RESAVE INTO nc FORMAT. 
    
    out.file <- gsub("PugetSound.asc","roms.nc",this.file)
    setwd(forcing.path)
    system(paste("nccopy", atlantis.salt.roms,out.file),wait=TRUE)
    
    roms.anomaly.nc <-nc_open(out.file, write=TRUE)
    
    ncvar_put(roms.anomaly.nc, "salinity", this.salinityArray) #  start=NA, count=NA, verbose=FALSE ) 
    
    #open and save again with updated variable
    nc_close(roms.anomaly.nc)
    
    roms.anomaly.nc <-nc_open(out.file, write=TRUE)
    
    salinityArrayInitial <- ncvar_get(salt.roms.nc,"salinity")
    salinityArrayAnomalyAdded <- ncvar_get(roms.anomaly.nc,"salinity")
    
    
    #---------------------------------
    # Still checking things... loop over all boxes and extract top layer only: 
    
    # So this is just checking and plotting the output here, 
    
    #-------------------------
    
    CompareTopLayers <- NULL # array(NA,dim=c(89,2,1460))
    
    num_Active_AtlantisBoxes <- 60
    
    for (boxIndex in 1:num_Active_AtlantisBoxes)
    {
      print('boxIndex')
      print(boxIndex)
      
      for (timestep in 1:1460)
      {
        print('timestep')
        print(timestep)
        # AtlantisBoxInfo$num_layers[boxIndex]
        #temperatureArray  has dims of     7   89 1460
        surfaceLayerIndex <- AtlantisBoxInfo$num_layers[boxIndex]
        
        #CompareTopLayers[boxIndex, 1,timestep]<-temperatureArrayInitial[surfaceLayerIndex,boxIndex,timestep ] 
        #CompareTopLayers[boxIndex, 2,timestep]<-temperatureArrayAnomalyAdded[surfaceLayerIndex,boxIndex,timestep ] 
        CompareTopLayers <-rbind(CompareTopLayers,cbind(salinityArrayInitial[surfaceLayerIndex,boxIndex,timestep ], salinityArrayAnomalyAdded[surfaceLayerIndex,boxIndex,timestep ],boxIndex,timestep   ))
        
      }
      
    } 
    
    print("compare anomalies")
    
    #plot(CompareTopLayers[,1], CompareTopLayers[,2])
    #View(CompareTopLayers)
    print(mean(CompareTopLayers[,1]))
    #[1] 11.20254
    print(mean(CompareTopLayers[,2]))
    #[1] 11.40549
    mean((CompareTopLayers[,2])/CompareTopLayers[,1])
    
    #-------------------------------------------------
    
    
    nc_close(salt.roms.nc)
    nc_close(roms.anomaly.nc)
    
    file.rename(from = file.path(forcing.path, out.file), to = file.path(save.path, out.file))
    
     }  # end if condition over salinity. 
  # 

  print("save data")
  temp.table <- CompareTopLayers %>% 
    as_tibble(.name_repair = "unique") %>% 
    setNames(c("roms_original","roms_cimp6","boxIndex","timestep")) 
  
  if(mean(temp.table$roms_original) == mean(temp.table$roms_cimp6)) stop("Anomaly not properly added")
  
  setwd(save.path)
  write_csv(temp.table,gsub("roms.nc","roms.csv",out.file))
  
  }  #close if file does not exist
}