#' @title Plotting Downscaled Salinity for Puget Sound
#' @author Stevie Walker, steviewalker99@gmail.com
#' July 2020
#' @details INPUT: A .csv file of downscaled data from different models, scenarios, and long-term or short-term projections
#' @details OUTPUT: Plot of average downscaled salinity scenarios for different terms and models
#' @details Modified using code from https://www.nrel.colostate.edu/this-is-how-i-did-it-mapping-in-r-with-ggplot2/
#' @details required packages: "tidyverse" "RColorBrewer"
#' @description line plot showing the average temporal change in salinity in Puget Sound under different scenarios, for different models and terms

# Setting directory ------------------------------------

setwd("~/Puget_Sound_Downscaling/")
data.downscaled.grouped <- fread("data_downscaled_table.csv")
save.path <- "~/Puget_Sound_Downscaling/final_figures"



# Plot Salinity variability over time -------------------------------------

#use library lubridate to create a time sequence
month.seq <- seq(ymd_hms('2005-01-01 12:00:00'), ymd_hms('2006-12-31 24:00:00'), by = "12 hours")

#grouping data
mean.salinity.model.experiments <- data.downscaled.grouped %>%
  group_by(variable, model, experiment, timestep, date) %>%
  filter(variable=="sos") %>%
  summarize(mean.roms_original = mean(roms_original), 
            mean.roms_cimp6 = mean(roms_cimp6)) %>%
  ungroup() %>%
  mutate(timeInDays=timestep/2) %>%
  mutate(term=if_else(date =="202001-205012roms.csv", "Short-term",
                      if_else(date =="207001-210012roms.csv", "Long-term", "NA"))) %>% 
  arrange(model,experiment,term,timestep) %>% 
  mutate(d_m_h=rep(month.seq,12))#repeat time sequence created above, for each experiment,model, term combination

#obtaining ROMS baseline
base.salinity <- mean.salinity.model.experiments %>% 
  filter(experiment=="ssp245") %>% 
  dplyr::select(variable,model,experiment,timestep,date,d_m_h,mean.roms_original,timeInDays,term) %>%
  mutate(experiment="base ROMS", mean.roms_cimp6 = mean.roms_original) 

#adding in baseline to scenarios
mean.salinity.base <- mean.salinity.model.experiments %>% 
  bind_rows(base.salinity)

#summarizing data for table
base.salinity %>% 
  summarise(mean_salinity=mean(mean.roms_original), max_salinity=max(mean.roms_original), min_salinity=min(mean.roms_original))

mean.salinity.model.experiments %>% 
  group_by(model,experiment,term) %>% 
  summarise(mean_salinity=mean(mean.roms_cimp6), max_salinity=max(mean.roms_cimp6), min_salinity=min(mean.roms_cimp6), sd=sd(mean.roms_cimp6), se=sd(mean.roms_cimp6)/sqrt(n()))


#plotting figure
ggplot(data = mean.salinity.model.experiments , aes(x=d_m_h, y=mean.roms_cimp6)) +
  geom_line(aes(group=model, color=experiment), size=0.5) +
  geom_line(data=base.salinity,aes(group=variable,x=d_m_h, y=mean.roms_original), size=0.5)+ #this line adds the base ROMS
  labs(x="Date", y="Average Salinity", color="Scenario") +
  theme_bw() +
  theme(aspect.ratio = 0.5, 
        axis.text.x = element_text(angle = -45, hjust = 0.1, vjust = 0.2)) +
  facet_grid(term~model)+
  scale_color_manual(values = c("#1b9e77","#7570b3","#d95f02","#e7298a", "#000000"))

ggsave(filename = "Fig5_salinity_model_term_scenario.png", device = "png", path = save.path, 
       dpi = 150, width = 20, height = 10, units = "cm")

