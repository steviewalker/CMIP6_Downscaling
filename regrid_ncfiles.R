#' @title Regrid files
#' @description  Functions to plot shapefile
#' @details INPUT: 1) An Atlantis model polygon shape file
#' @details OUTPUT: 1) Map of Puget Sound
#' @details Used code from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com modified 6/19/20 by Stevie Walker steviewalker99@gmail.com



regrid_nc <- function(this.file, save.path, data.path, wgts.dir, resolution) {
  
  this.nc <- paste0(data.path,"/",this.file)
  
  this.name <- gsub(".nc","",this.file)
 
  new.name <- paste0(this.name,"_rg.nc")
  
  # open netCDF file
  ncin <- nc_open(this.nc)
  print(this.file) #lists basic info about file
  
  setwd(save.path)
  test.file <- file.exists(new.name)
  
  if(test.file==FALSE){
    
    if(grepl("GFDL",new.name)) # GFDL is on the base grid already, so no need to regrid
    {  
        file.rename(from = file.path(data.path, this.file), to = file.path(save.path, new.name))
      
    }
    else  # for cases when not GFDL, such as CNRM
    {
    setwd(data.path)
    
    # this outputs file information
    system(paste("cdo sinfov ",this.file,sep=""), wait=TRUE)
    
    # Generate bilinear interpolation weights
    system(paste("cdo genbil,r",resolution," ",this.file," ",this.name,"_wgts.nc",sep=""),wait = TRUE) 
   
    weigthFile <- paste0(this.name,"_wgts.nc")
   
    # Grid remapping
    system(paste("cdo remap,r",resolution,",",weigthFile," ", this.file," ",new.name,sep=""),wait = TRUE)
  
    #move regrid files to separate directory
    rgrid.files <- list.files(data.path, pattern = "rg.nc$")
    
    file.rename(from = file.path(data.path, rgrid.files), to = file.path(save.path, rgrid.files))
    
    #move weights files to separate directory
    wgts.files <- list.files(data.path, pattern = "wgts.nc$")
    
    file.rename(from = file.path(data.path, wgts.files), to = file.path(wgts.dir, wgts.files))
    
    print(paste(this.file, "done regrid:", new.name))
    
    } # end of else statement (for cases when not GFDL) 
  }
  
}

