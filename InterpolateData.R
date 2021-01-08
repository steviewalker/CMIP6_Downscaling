
InterpolateData <- function(this.file,PugetSoundRaster,data.path,save.path)
{

  print(this.file)
  setwd(data.path)
  
#this.file<-"sos_Omon_GFDL-CM4_ssp245_r1i1p1f1_anomaly_202001-205012.nc"

this.var <- this.file %>%
  str_split(., pattern="_") %>%
  unlist %>%
  .[1]

out.file <-gsub(".nc","PugetSound.asc",this.file)
print(out.file)

# Bounding box is:  -130.0,46.0,-121.5,51.5
#nc_open(this.file)

#----------
# Create Raster map of this variable: 
raster.nc <- raster(this.file,  varname = this.var, stopIfNotEqualSpaced = FALSE)
proj4string(raster.nc) = "+proj=longlat +datum=WGS84"

# Deal with fact that we want West Longitude as negative longitude, but CNRM uses continuous degrees East longitude only
raster.nc.rotate <- rotate(raster.nc) # https://stackoverflow.com/questions/25730625/how-to-convert-longitude-from-0-360-to-180-180
plot(raster.nc.rotate)

#-----------------
# We want to crop map to just area around WA and Oregon and British Columbia
#extent.study <- extent(-130, -121.5, 46, 51.5) # (first row: xmin, xmax; second row: ymin, ymax)
# filter(lat < 40 & lat > (-10) & lon > (-130) & lon < (-50))
raster.nc.rotate.ext <- crop(raster.nc.rotate, PugetSoundRaster)
plot(raster.nc.rotate.ext)


lat.lon.raster <- data.frame(xyFromCell(raster.nc.rotate.ext, 1:ncell(raster.nc.rotate.ext)))
vals.raster <- getValues(raster.nc.rotate.ext)

#---------------------------
# Here's the main part of the work: fitting thin plate spline model to interpolate
#https://www.rdocumentation.org/packages/raster/versions/2.6-7/topics/interpolate
#### Thin plate spline model
tps.model <- Tps(lat.lon.raster, vals.raster)


#----------------------
# Ok now have a model fit, but need to apply that model to make a new map by predicting across a raster map. 

# going to set resolution at 3km (trying for now)
pred.raster <- raster(res=0.03)
#extent(pred.raster)<-extent.study
pred.raster.ext <- setExtent(pred.raster,PugetSoundRaster,keepres = TRUE)

# use model to predict values at all locations
pred.raster.tps <- interpolate(pred.raster.ext, tps.model)

ps.tps.raster <- mask(pred.raster.tps,PugetSoundRaster)
plot(ps.tps.raster)
setwd(save.path)

writeRaster(ps.tps.raster,
            filename = out.file,
            format="ascii",
            overwrite=TRUE,
            NAflag= -9999)

}

