#' @title Get CIMP6 files from ESGF repository
#' @description  Search ESGF server using RESTFUL API
#' @details INPUT: 1) model name to search for
#' @details OUTPUT: 1) CIMP6 for historical scenarios
#' @details https://esgf-node.llnl.gov/search/cmip6/
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com



get_historical_nc <- function(model.name, model.var) {
  
#this.url <- paste("http://esgf-node.llnl.gov/esg-search/search?mip_era=CMIP6&realm=ocean&experiment_id=ssp245&frequency=mon&variable=tos&source_id=", model.name, sep = "")

#get all scenarios in ScenarioMIP
this.url <- paste0("http://esgf-node.llnl.gov/esg-search/search?mip_era=CMIP6&realm=ocean&experiment_id=historical&frequency=mon&variable=",model.var,"&source_id=", model.name)

print(this.url)
#read XML with search results for each variable
data.set <- xmlParse(this.url) %>% xmlToList() %>% unlist

#grep("esgf-data1.llnl.gov", data.set, value= TRUE)

#get all instances of data and data nodes
vector.names <- names(data.set)=="result.doc.str.text"


output.var <- grep("[|]",data.set[vector.names], value = TRUE)

length.var <- length(output.var)

for(eachdata in 1:length.var){
  
  this.output <- output.var[[eachdata]]
  
  file.source <- str_split(this.output,"[|]") %>% 
    unlist %>% 
    .[1]
  
  data.source <- str_split(this.output,"[|]") %>% 
    unlist %>% 
    .[2]
  
  data.set.cimp6 <- tibble(dataset = this.output,file_source = file.source, data_source = data.source) %>% 
    separate(
      col=file_source,
      c(
        "mip_era",
        "experiment",
        "lab",
        "model",
        "experiment_id",
        "realisation",
        "realm",
        "variable",
        "gn",
        "ver"
      ),
      sep = "\\.",
      remove = FALSE
    ) 
  
  this.url.wget <- paste("https://esgf-node.llnl.gov/esg-search/wget/?distrib=false&dataset_id=",data.set.cimp6$dataset,sep="")
  
  
  this.filename <- paste(data.set.cimp6$model,data.set.cimp6$experiment,data.set.cimp6$variable,".sh",sep="") %>% 
    gsub("-","",.) 
  
  withRestarts(
    tryCatch(
      download.file(
        url=this.url.wget,
        destfile=this.filename,
        method = "wget",
        quiet = FALSE,
        mode = "w",
        cacheOK = TRUE,
        extra = getOption("download.file.extra")
      ),
      finally = print(paste("DONE", this.filename, sep = "  "))
    ),
    abort = function() {
    },
    error = function(e) {
      print("error")
    }
  )
   #--------------------
  
  # 07/17/2020: There was a problem with two of the  WGET files that Stevie discovered. 
  # This line here is the hack to deal with new WGET files that we had to get manually
  this.filename<-"wget-20200717204131_ssp370.sh"
  # The filenames are: wget-20200717202811_historical.sh
  #    and wget-20200717204131_ssp370.sh
  #--------------------------
  # 07/17/2020: Then continue to run these two lines (no need to run these in a loop, just run these two lines) 
  system(paste("sudo chmod +x", this.filename))
  try(system(paste("sudo ./", this.filename, " -H https://esgf-data.dkrz.de/esgf-idp/openid/hmorzaria", sep = ""), wait=TRUE))
  #If you get asked for password Juncus#7
  
    
}


}
