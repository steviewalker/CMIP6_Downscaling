#' @title Calculate climatology based on mean
#' @description  Take nc file from NEC Data Access, National Oceanographic Data Center
#' @details INPUT: 1) NEC nc file
#' @details OUTPUT: 1) Climatology for 1982-2010
#' @details Used code from climate_change_statistics.R
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com




climatology_nc <- function(this.file, stat.clim, data.path, save.path){
  
  
  # open netCDF file
  ncin <- nc_open(this.file)
  print(ncin) #lists basic info about file
  
  date.table <- this.file %>% 
    str_split(.,"_") %>% 
    unlist %>% 
    .[7] %>% 
    tibble(date=.) %>% 
    separate(date,c("start_date","end_date"),"-",remove=FALSE) %>%
    separate(start_date,c("start_yr","start_mo"),4,remove=FALSE) %>%
    separate(end_date,c("end_yr","end_mo"),4,remove=FALSE)
  
  this.filename <- this.file %>% 
    str_split(.,"_gn") %>% 
    unlist %>% 
    .[1]
  
  test.file <- grepl("historical",this.file)
  
  if(test.file==TRUE){
    print("historical file")
    ini.mo="01"
    ini.yr="1984"
    ini.day = "01"
    fin.mo="11"
    fin.yr="2014"
    fin.day = "28" 
    
    calc_climatology(this.file, ini.mo, ini.yr, ini.day, fin.mo, fin.yr, fin.day, this.filename,stat.clim, data.path, save.path, date.table)
    
  } else {
    
    ini.mo="01"
    ini.yr="2020"
    ini.day = "01"
    fin.mo="12"
    fin.yr="2050"
    fin.day = "28" 
    
    calc_climatology(this.file, ini.mo, ini.yr, ini.day, fin.mo, fin.yr, fin.day, this.filename,stat.clim, data.path, save.path, date.table)
    
    ini.mo="01"
    ini.yr="2070"
    ini.day = "01"
    fin.mo="12"
    fin.yr="2100"
    fin.day = "28" 
    
    calc_climatology(this.file, ini.mo, ini.yr, ini.day, fin.mo, fin.yr, fin.day, this.filename,stat.clim, data.path, save.path, date.table)
    
  }
  
  
}

calc_climatology <- function(this.file, ini.mo, ini.yr, ini.day, fin.mo, fin.yr, fin.day, this.filename,stat.clim, data.path, save.path, date.table){
  
  
  ini.req.date <- paste0(ini.yr, ini.mo, ini.day) %>% as.Date("%Y%m%d")
  
  fin.req.date <- paste0(fin.yr, fin.mo, fin.day) %>% as.Date("%Y%m%d")
  
  ini.mo.date <- paste(date.table$start_yr, date.table$start_mo, ini.day,sep="") %>% as.Date("%Y%m%d")
  
  fin.mo.date <- paste(date.table$end_yr, date.table$end_mo, fin.day,sep="") %>% as.Date("%Y%m%d")
  
  ini.layer <- length(seq(from=ini.mo.date, to=ini.req.date, by='month'))
  
  end.layer <- ini.layer+length(seq(from=ini.req.date, to=fin.req.date, by='month')) 
  
  new.file <- paste0(this.filename,"_",stat.clim,"_",ini.yr,ini.mo,"-",fin.yr,fin.mo,".nc")
  
  print(paste("Generating",new.file))
  
  setwd(save.path)
  test.file <- file.exists(new.file)
  
  if(test.file==FALSE){
    
    setwd(data.path)
    #getmean
    system(paste0("ncra -F -d time,",ini.layer,",",end.layer," ",this.file," ",new.file), wait=TRUE)
    #move to save folder
    file.rename(from = file.path(data.path, new.file), to = file.path(save.path, new.file))
    
    
  }
  
}
