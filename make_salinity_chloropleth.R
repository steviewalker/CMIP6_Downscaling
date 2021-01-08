#' @title Function for plotting salinity chloropleth maps for Puget Sound
#' @author Stevie Walker, steviewalker99@gmail.com
#' July 2020
#' @details INPUT: .csv file of final downscaled temperature and salinity projections
#' @details OUTPUT: Chloropleth map of salinity for long-term and short-term projections
#' @details used code from https://github.com/hmorzaria/gommesopelagic/blob/master/make_map.R
#' @description Shows salinity projections for 2020-2050 and 2070-2100 under multiple models and scenarios


#make_salinity_chloropleth(data.downscaled.table, model.type = "GFDL-CM4", experiment.type ="ssp585", 
                    # plot.filename1="salinity_long-term_choropleth.png", plot.filename2 = "salinity_short-term_choropleth.png",
                    # combined.filename="salinity_combined_choropleth.png", save.path = "~/Puget_Sound_Downscaling/final_figures/")

make_salinity_chloropleth <- function(data.downscaled.table, model.type, experiment.type, plot.filename1, plot.filename2, combined.filename, save.path){
  

#grouping data including Atlantis polygon boxes
box.salinity.grouping <- data.downscaled.table %>%
  group_by(variable, model, experiment, boxIndex, date) %>%
  filter(variable=="sos") %>%
  summarize(mean.roms_original = mean(roms_original), 
            mean.roms_cimp6 = mean(roms_cimp6)) %>%
  ungroup() %>%
  mutate(term=if_else(date =="202001-205012roms.csv", "Short-term",
                      if_else(date =="207001-210012roms.csv", "Long-term", "NA")))

#subsetting boxes for one scenario, model, and variable
box.salinity.grouping.subset <- box.salinity.grouping %>%
  filter(model == model.type & experiment == experiment.type & term == "Long-term") %>%
  dplyr::select(boxIndex,mean.roms_cimp6) %>%
  dplyr::rename(id=boxIndex)


#reading in shapefile
setwd("~/Puget_Sound_Downscaling/Atlantis_shapefile/")

model.shape <- readOGR(".","bgm_Puget_Sound_89b_0p0001_WGS84")

# Reformat shape for mapping purposes
model.shape.df <- broom::tidy(model.shape, region="BOX_ID")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

#setting map bounds
min.long <- -125
max.long <- -121.4
min.lat <- 46.7
max.lat <- 49.14

#preparing axis labels
ewbrks <- c(-125,-124,-123,-122)
nsbrks <- c(47,47.5,48,48.5,49)
#followed example from https://stackoverflow.com/questions/59610405/ggplot-specify-longitude-latitude-axis-breaks

#correcting Antlantis polygon ids
polygon1 <-  box.salinity.grouping.subset %>%
  mutate(id=as.character(id)) %>% 
  right_join(model.shape.df, by="id")

setwd("~/Puget_Sound_Downscaling/final_figures/")
      

SALmodel.map1 <- ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5,
                   height = unit(0.095, "in"), line_width = 0.6) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         height = unit(0.4, "in"), width = unit(0.4, "in"),
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "darkgrey", check_overlap = FALSE) +
  coord_sf(xlim = c(min.long, max.long), ylim = c(min.lat, max.lat), expand = FALSE)+
  scale_x_continuous(breaks = ewbrks) +
  scale_y_continuous(breaks = nsbrks) +
  geom_polygon(data = polygon1, aes(fill=mean.roms_cimp6, x = long, y = lat, group = group))+
  labs(title = "Long-term (2070-2100)",
       fill = "Salinity") +
theme_bw()+
  theme(legend.position = "right") +
  #scale_fill_gradient(low="white", high="gray55")
  scale_fill_distiller(palette = "Greens", direction = 1, limits = c(22.33,33)) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )

#saving long term figure
ggsave(SALmodel.map1, filename = plot.filename1, device = "png", path = save.path, 
       dpi = 150, width = 13, height = 11, units = "cm")


# Short Term Plot ---------------------------------------------------------


#subsetting boxes for one scenario, model, and variable
box.salinity.grouping.subset2 <- box.salinity.grouping %>%
  filter(model == model.type & experiment == experiment.type & term == "Short-term") %>%
  dplyr::select(boxIndex,mean.roms_cimp6) %>%
  dplyr::rename(id=boxIndex)


polygon2 <-  box.salinity.grouping.subset2 %>%
  mutate(id=as.character(id)) %>% 
  right_join(model.shape.df, by="id")


SALmodel.map2 <- ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5,
                   height = unit(0.095, "in"), line_width = 0.6) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         height = unit(0.4, "in"), width = unit(0.4, "in"),
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "darkgrey", check_overlap = FALSE) +
  coord_sf(xlim = c(min.long, max.long), ylim = c(min.lat, max.lat), expand = FALSE)+
  scale_x_continuous(breaks = ewbrks) +
  scale_y_continuous(breaks = nsbrks) +
  geom_polygon(data = polygon2, aes(fill=mean.roms_cimp6, x = long, y = lat, group = group))+
  labs(title = "Short-term (2020-2050)",
       fill = "Salinity") +
  theme_bw()+
  theme(legend.position = "right") +
  #scale_fill_gradient(low="white", high="gray55")
  scale_fill_distiller(palette = "Greens", direction = 1, limits = c(22.33,33)) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank() ,
    legend.position = "none" #uncomment this line before creating facet grid
  )

#saving short term figure
ggsave(SALmodel.map2, filename = plot.filename2, device = "png", path = save.path, 
       dpi = 150, width = 13, height = 11, units = "cm")

#combining short-term and long-term into one grid
grid.combine <- grid.arrange(SALmodel.map2,SALmodel.map1, ncol =2, nrow=1)

#saving combined grids
ggsave(grid.combine, filename = combined.filename, device = "png", path = save.path, 
       dpi = 100, width = 24, height = 10, units = "cm")
}

