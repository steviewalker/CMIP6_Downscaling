#' @title Plotting Interpolated Salinity Anomalies for Salish Sea
#' @author Stevie Walker, steviewalker99@gmail.com
#' July 2020
#' @details INPUT: A .asc file of salinity from different models, scenarios, and long-term or short-term projections
#' @details OUTPUT: Plots of interpolated temperature or salinity conditions
#' @details Modified using code from https://www.nrel.colostate.edu/this-is-how-i-did-it-mapping-in-r-with-ggplot2/
#' @details required packages: "raster" "tidyverse" "RColorBrewer"
#' @description shows interpolated anomalies of salinity for the entire Salish Sea


# Loading Libraries -------------------------------------------------------


# load packages
library(raster)
library(RColorBrewer)
library(tidyverse)

setwd("~/Puget_Sound_Downscaling/3km_interpolated_files/")


# Creating Plot -----------------------------------------------------------

plot_salinity_anomaly <- function(this.file, data.path, save.path) {
  
  print(this.file)
  setwd(data.path)
  
  #this.file <- "sos_Omon_CNRM-CM6-1-HR_ssp126_r1i1p1f2_anomaly_207001-210012PugetSound.asc"
  save.name <- gsub(pattern="PugetSound.asc", "_plot.png", this.file)
  
  #model name
  model.name <- this.file %>%
    str_split(., pattern="_") %>%
    unlist %>%
    .[3]
  
  #scenario
  scenario <- this.file %>%
    str_split(., pattern="_") %>%
    unlist %>%
    .[4]
  
  #short or long term
  time.period <- this.file %>%
    str_split(., pattern="_") %>%
    unlist %>%
    .[7] %>%
    gsub("PugetSound.asc", "", .) %>%
    as_tibble() %>%
    separate(col = value, into = c("int.yr","end.yr"), sep = "-") %>%
    separate(col = int.yr, into = c("in.yr", "in.mon"), sep = 4) %>%
    separate(col = end.yr, into = c("en.yr", "en.mon"), sep = 4)
  
  if(time.period$in.yr == "2020") term.period <- "Short-term (2020-2050)"
  if(time.period$in.yr == "2070") term.period <- "Long-term (2070-2100)"
  
  
  #creating title
  plot.title <- paste(term.period, model.name, scenario)
  #labs(title = "Long-term (2070-2100) ssp585 SST Anomaly"
  
  #open ASC file, convert to raster object
  map <- raster(this.file)
  
  #determining min and max values to set scale
  maxvalue <- cellStats(map, stat='max', na.rm=TRUE)
  minvalue <- cellStats(map, stat='min', na.rm=TRUE)
  
  print(this.file)
  print(paste("raster max value", maxvalue))
  print(paste("raster min value", minvalue))
  
  #convert the raster to points for plotting
  map.p <- rasterToPoints(map)
  
  #Make the points a dataframe for ggplot
  df <- data.frame(map.p)
  #Make column headings
  colnames(df) <- c("Longitude", "Latitude", "salinity")
  
  #preparing axis labels
  ewbrks <- c(-128,-126,-124,-122,-120)
  nsbrks <- c(46,48,50,52)
  ewlbls <- paste0(abs(ewbrks),ifelse(ewbrks < 0, "°E","°W"))
  nslbls <- paste0(abs(nsbrks),ifelse(nsbrks < 0, "°S","°N"))
  
  if(model.name=="CNRM-CM6-1-HR") {
    ggplot(data=df, aes(y=Latitude, x=Longitude, color="red")) +
      geom_raster(aes(fill=salinity)) +
      theme_bw() +
      labs(title = plot.title,
           fill = "Salinity anomaly") +
      scale_fill_distiller(palette = "YlGnBu", direction = -1, limits=c(-2.16,0.17)) +
      #scale_fill_gradient(low = "#225ea8", high = "#ffeda0") +
      scale_x_continuous(breaks = ewbrks, labels = ewlbls) +
      scale_y_continuous(breaks = nsbrks, labels = nslbls) +
      coord_cartesian(xlim = range(df$Longitude), ylim = range(df$Latitude), expand = TRUE) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key = element_blank()
      )
  }
  
  if(model.name=="GFDL-CM4") {
    ggplot(data=df, aes(y=Latitude, x=Longitude, color="red")) +
      geom_raster(aes(fill=salinity)) +
      theme_bw() +
      labs(title = plot.title,
           fill = "Salinity anomaly") +
      scale_fill_distiller(palette = "YlGnBu", direction = -1, limits=c(-0.6,0.83)) +
      #scale_fill_gradient(low = "#225ea8", high = "#ffeda0") +
      scale_x_continuous(breaks = ewbrks, labels = ewlbls) +
      scale_y_continuous(breaks = nsbrks, labels = nslbls) +
      coord_cartesian(xlim = range(df$Longitude), ylim = range(df$Latitude), expand = TRUE) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key = element_blank()
      )
  }
  
  ggsave(filename = save.name, device = "png", path = save.path, 
         dpi = 150, width = 15, height = 11, units = "cm")
  
}
