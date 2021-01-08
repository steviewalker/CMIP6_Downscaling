#' @title Mapping Puget Sound
#' @description  Functions to plot shapefile
#' @details INPUT: 1) An Atlantis model polygon shape file
#' @details OUTPUT: 1) Map of Puget Sound
#' @details Used code from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com modified 6/19/20 by Stevie Walker steviewalker99@gmail.com

make_map <- function(shape.file, basin.shapefile, file.name, scale.factor, bar.position, arrow.position, min.long, max.long, min.lat, max.lat) {

  model.shape <- readOGR(shape.file)
  
  # Reformat shape for mapping purposes
  model.shape.df <- broom::tidy(model.shape)
  
  basin.shape <- readOGR(basin.shapefile)
  
  basin.shape.crs <- spTransform(basin.shape,
              crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
   
  basin.names <- basin.shape.crs@data %>% 
    mutate(id = as.character(0:8)) %>% 
    dplyr::select(-WEB_GEOMET)
  
  # Reformat shape for mapping purposes
  basin.shape.df <- broom::tidy(basin.shape.crs) %>% 
    left_join(basin.names, by="id")
  
  my_box <- rgeos::bbox2SP(n = 50, s = 47, w = -125, e = -121.5,
                           proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

  box.df <- broom::tidy(my_box) 
  
  world <- ne_countries(scale = "large", returnclass = "sf")
  class(world)
  
  world_points<- st_centroid(world)
  world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))
  
  #create Puget Sound map
  # main.map <- ggplot(data = world) +
  #   geom_sf() +
  #   annotation_north_arrow(location = arrow.position, which_north = "true", 
  #                          pad_x = unit(0.08, "in"), pad_y = unit(0.3, "in"),
  #                          style = north_arrow_fancy_orienteering) +
  #   #geom_text(data= world_points,aes(x=X, y=Y, label=name),
  #   #          color = "darkgrey", check_overlap = FALSE) +
  #   coord_sf(xlim = c(min.long, max.long), ylim = c(min.lat, max.lat), expand = FALSE)+
  #   geom_path(data = basin.shape.df, aes(x = long, y = lat, group=group),
  #             colour = "lightgrey") +
  #   annotation_scale(location = "tr", width_hint = 0.2) +
  #   xlab(NULL) +
  #   ylab(NULL) +
  #   theme_bw()+
  #   theme(panel.grid = element_blank()) +
  #   annotate(geom="text", x=-122.3, y=47.05, label="Puget Sound",
  #            color="black")
  
  main.map <- ggplot() +
    geom_polygon(data = box.df, aes(x = long, y = lat, group=group), fill = "gray86")+
    geom_polygon(data = basin.shape.df, aes(x = long, y = lat, group=group, fill = NAME))+
    annotation_north_arrow(location = arrow.position, which_north = "true", 
                           pad_x = unit(0.08, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering)+
    annotation_scale(location = "tr", width_hint = 0.2)+
    #scale_fill_manual(pal2) +
    xlab("Lon") +
    ylab("Lat") +
    labs(fill = "Basin") +
    theme_bw()+
    theme(panel.grid = element_blank())
    #annotate(geom="text", x=-122.6, y=49.65, label="Puget Sound",
          #   color="black")
    
  
  #create inset map
  states.shape <- readOGR("~/Puget_Sound_Downscaling/USstates_shapefile/eua.shp")
  crs(states.shape) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  states.shape.df <- broom::tidy(states.shape)
  
  inset.map <- ggplot(data = world) +
    geom_path(data = states.shape.df, aes(x = long, y = lat, group=group),
               colour = "black") +
    geom_sf() + 
    coord_sf(xlim = c(-167.6,-112), ylim = c(28.5, 62.8), expand = FALSE) +
    geom_rect(aes(xmin = -124.555, xmax = -121.9, ymin = 46.9936, ymax = 48.957),
              alpha = 0, fill = "grey80", color = "black", size = 0.5) +
    xlab(NULL) +
    ylab(NULL) +
    theme_bw() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.line = element_blank(), panel.grid = element_blank())
  
  #combine maps
  model.map <- ggdraw() +
    draw_plot(main.map) +
    draw_plot(inset.map, x = 0.06, y = 0.07, width = 0.26, height = 0.26)
    
   
  ggsave(file.name, model.map,path = "~/Puget_Sound_Downscaling/final_figures/", 
         scale = scale.factor, width = 20, height = 17, units = "cm", dpi = 400)
  
  return(model.map)
  
}
