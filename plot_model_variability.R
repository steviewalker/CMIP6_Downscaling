#' @title Plotting Variability between models for Puget Sound
#' @author Stevie Walker, steviewalker99@gmail.com
#' July 2020
#' @details INPUT: A .csv file of downscaled data from different models, scenarios, and long-term or short-term projections
#' @details OUTPUT: Plot of average SST for one scenario, two terms, and two models
#' @details Modified using code from https://www.nrel.colostate.edu/this-is-how-i-did-it-mapping-in-r-with-ggplot2/
#' @details required packages: "tidyverse" "RColorBrewer"
#' @description line plot showing the variability between the GFDL-CM4 and CNRM-CM6-1-HR models


# Setting directory ------------------------------------

setwd("~/Puget_Sound_Downscaling/")
data.downscaled.grouped <- fread("data_downscaled_table.csv")
save.path <- "~/Puget_Sound_Downscaling/final_figures"


# Plot SST variability over time ------------------------------------------
#use library lubridate to create a time sequence
month.seq <- seq(ymd_hms('2005-01-01 12:00:00'), ymd_hms('2006-12-31 24:00:00'), by = "12 hours")

#grouping data
mean.SST.model.experiments <- data.downscaled.grouped %>%
  filter(variable=="tos") %>% 
  group_by(variable, model, experiment, timestep, date) %>%
  summarize(mean.roms_original = mean(roms_original), 
            mean.roms_cimp6 = mean(roms_cimp6)) %>%
  ungroup() %>%
  mutate(timeInDays=timestep/2) %>%
  mutate(term=if_else(date =="202001-205012roms.csv", "Short-term",
                      if_else(date =="207001-210012roms.csv", "Long-term", "NA"))) %>% 
  arrange(model,experiment,term,timestep) %>% 
  mutate(d_m_h=rep(month.seq,12))#repeat time sequence created above, for each experiment,model, term combination


#mean.SST.model.experiments %>%  group_by(model,experiment) %>% 
# summarise(mean=mean(mean.roms_cimp6))

#creating temperature variability plot
SST_variability <- mean.SST.model.experiments %>% 
  filter(experiment=="ssp585") %>%
  ggplot(aes(x=d_m_h, y=mean.roms_cimp6)) +
  geom_line(aes(group=experiment, color=model)) +
  labs(x="Date", y="Average SST (°C)", color="Model") +
  theme_bw() +
  theme(aspect.ratio = 0.5, 
        axis.text.x = element_text(angle = -45, hjust = 0.1, vjust = 0.2)) +
  facet_grid(term~.)+
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"))


# Plot salinity variability over time ----------------------------------------------------

#grouping data
mean.salinity.model.experiments <- data.downscaled.grouped %>%
  filter(variable=="sos") %>% 
  group_by(variable, model, experiment, timestep, date) %>%
  summarize(mean.roms_original = mean(roms_original), 
            mean.roms_cimp6 = mean(roms_cimp6)) %>%
  ungroup() %>%
  mutate(timeInDays=timestep/2) %>%
  mutate(term=if_else(date =="202001-205012roms.csv", "Short-term",
                      if_else(date =="207001-210012roms.csv", "Long-term", "NA"))) %>% 
  arrange(model,experiment,term,timestep) %>% 
  mutate(d_m_h=rep(month.seq,12))#repeat time sequence created above, for each experiment,model, term combination

#creating variability plot
salinity_variability <- mean.salinity.model.experiments %>% 
  filter(experiment=="ssp585") %>%
  ggplot(aes(x=d_m_h, y=mean.roms_cimp6)) +
  geom_line(aes(group=experiment, color=model)) +
  labs(x="Date", y="Average Salinity", color="Model") +
  theme_bw() +
  theme(aspect.ratio = 0.5, 
        axis.text.x = element_text(angle = -45, hjust = 0.1, vjust = 0.2)) +
  facet_grid(term~.)+
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"))


# Faceting Figures -----------------------------------------------------------

grid.combine <- grid_arrange_shared_legend(SST_variability, salinity_variability, ncol = 2, nrow = 1, position='top')

ggsave(grid.combine, filename = "Fig6_model_variability.png", device = "png", path = save.path, 
       dpi = 100, width = 22, height = 12, units = "cm")



# Repeating Process with ssp245 -------------------------------------------



#creating temperature variability plot
SST_variability2 <- mean.SST.model.experiments %>% 
  filter(experiment=="ssp245") %>%
  ggplot(aes(x=d_m_h, y=mean.roms_cimp6)) +
  geom_line(aes(group=experiment, color=model)) +
  labs(x="Date", y="Average SST (°C)", color="Model") +
  theme_bw() +
  theme(aspect.ratio = 0.5, 
        axis.text.x = element_text(angle = -45, hjust = 0.1, vjust = 0.2)) +
  facet_grid(term~.)+
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"))

#creating variability plot
salinity_variability2 <- mean.salinity.model.experiments %>% 
  filter(experiment=="ssp245") %>%
  ggplot(aes(x=d_m_h, y=mean.roms_cimp6)) +
  geom_line(aes(group=experiment, color=model)) +
  labs(x="Date", y="Average Salinity", color="Model") +
  theme_bw() +
  theme(aspect.ratio = 0.5, 
        axis.text.x = element_text(angle = -45, hjust = 0.1, vjust = 0.2)) +
  facet_grid(term~.)+
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"))

#combining and saving ssp245 plot
grid.combine2 <- grid_arrange_shared_legend(SST_variability2, salinity_variability2, ncol = 2, nrow = 1, position='top')

ggsave(grid.combine2, filename = "ssp245_model_variability.png", device = "png", path = save.path, 
       dpi = 100, width = 22, height = 12, units = "cm")

