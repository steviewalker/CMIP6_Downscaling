#' @title Combine files
#' @description  Combine multiple nc files into one
#' @details INPUT: 1) nc files
#' @details OUTPUT: 1) a single nc file
#' @details uses external library NCO to combine files http://nco.sourceforge.net/
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com modified 7/7/20 by Stevie Walker steviewalker99@gmail.com


combine_files <- function(this.pattern, model.files, save.path, data.path){
  
  print(this.pattern)
  these.files <-  grep(this.pattern,model.files, value=TRUE)
  
  if(length(these.files) > 1){
    
    #extracts start and end date 
    
    file.table <- these.files %>% 
      str_split(.,"_gr") %>% 
      unlist %>% 
      grep(".nc",., value = TRUE) %>% 
      gsub("_","",.) %>% 
      gsub(".nc","",.) %>% 
      tibble(date=.) %>% 
      separate(col=date,into=c("start_date","end_date"),"-") %>% 
      separate(col=start_date,into=c("start_yr","start_mo"),4) %>% 
      separate(col=end_date,into=c("end_yr","end_mo"),4)
    
    #start year
    start.yr <- file.table %>% 
      pull(start_yr) %>% 
      as.numeric %>% 
      min()
    
    #end year
    end.yr <- file.table %>% 
      pull(end_yr) %>% 
      as.numeric %>% 
      max()
    
    #start month
    start.mo <- file.table %>% 
      filter(start_yr == start.yr) %>% 
      pull(start_mo) 
    
    #end month
    end.mo <- file.table %>% 
      filter(end_yr == end.yr) %>% 
      pull(end_mo) 
    
    #creates a new name with the start and end date of the series  
    new.name <- paste(this.pattern,"_gr_",start.yr,start.mo,"-",end.yr,end.mo,".nc",sep="")
    
    setwd(save.path)
    
    #checks if the combined file already exists
    test.file <- file.exists(new.name)
    
    if(test.file==FALSE){
      
      #calls library for netcdf files NCO, to combine all files into one
      #moves combined data file to combined data directory
      setwd(data.path)
      
      #-------------------------
      # GFDL did not specify time dimension as the delimiting dimension. 
      # Here we force each file to know that time dimension is important. 
      
      combinedNamesAllFilesWithTime <- "" # empty character vector to hold file names later. 
      
      for (fileNum in 1:length(these.files))  # Loop over five 20 year blocks of output, to combine into a 100 year block
           {
             print("fileNum is")
             print(fileNum)
             
             # # Just adding WithTime to the file name.  Note this is just a text string filename, not a file itself. 
             newFileNameWithTime <- gsub( ".nc", "WithTime.nc",these.files[fileNum])  
             
             # Here we are just adding to a long list of filenames , so we can glue them together later
             combinedNamesAllFilesWithTime<- paste(combinedNamesAllFilesWithTime, newFileNameWithTime,sep=" ")
             
             # Check to make sure file doesn't already exist
             if (!file.exists(newFileNameWithTime ))  
             {
               # If file doesn't exist, then we need to create it with ncks 
             system(paste("ncks -O --mk_rec_dmn time ", these.files[fileNum], newFileNameWithTime,sep=" "))  # this is the ncks magic to make time  the special delimiter
             }
             else
             {  
               print("file already existed so not redimensioned by time")
               }
             
         }
      
           
      #----------------     
           # Create text string which is list of files  created above, which conviently had "WithTime" added to file name. 
            # Use list.files to find these files from the directory. 
      
      #  theseFilesWithTime<-toString(list.files(pattern="WithTime.nc"))
      
      # NOW WE USE ncrcat TO COMBINE OR CONCATENATE ALL FIVE FILES INTO 1, DIMENSIONING BY TIME. 
      
      print(combinedNamesAllFilesWithTime)  # this is just a long list of filenames. 
      
      system(paste("ncrcat",combinedNamesAllFilesWithTime ,new.name,sep=" "),wait = TRUE) 
      # was like this, worked before: system(paste("ncrcat -O",these.files,new.name,sep=" "),wait = TRUE)  
      
      file.rename(from=paste(data.path,new.name,sep="/"),to=(paste(save.path,new.name,sep="/")))
      
      list.files(pattern="*.*WithTime.nc$",full.names = TRUE) %>% 
      file.remove()
    }
  } else {
    
    #if file already is a full set of years moves to combined data directory
    file.rename(from=paste(data.path,these.files,sep="/"),to=(paste(save.path,these.files,sep="/")))
  }
  
  # THIS WAS JUST SOME TESTING: 
  #file1.nc <- "sos_Omon_GFDL-CM4_historical_r1i1p1f1_gn_185001-186912.nc"
  #file2.nc<-  "sos_Omon_GFDL-CM4_historical_r1i1p1f1_gn_187001-188912.nc"
  #fileout1.nc<- "sos_Omon_GFDL-CM4_historical_r1i1p1f1_gn_185001-186912WithTIME.nc"
  #fileout2.nc <- "sos_Omon_GFDL-CM4_historical_r1i1p1f1_gn_187001-188912WithTIME.nc"
  #filenew.nc <-  "sos_Omon_GFDL-CM4_historical_r1i1p1f1_gn_185001-188912.nc"
  #system(paste("ncks -O --mk_rec_dmn time ", file1.nc, fileout1.nc,sep=" "))
  #system(paste("ncks -O --mk_rec_dmn time ", file2.nc, fileout2.nc, sep=" "))
  #system(paste("ncrcat",fileout1.nc, fileout2.nc, filenew.nc,sep=" "))  
  
}
