## -- load libraries -- ##
library(here)
library(ncdf4)
library(chron) # allows user to manage time and date data
library(lubridate) # allows user to manage time and date data
library(dplyr)
library(ggplot2)
library(patchwork)


## -- read in data -- ##
# Data was retrieved from: http://www.cesm.ucar.edu/projects/community-projects/LENS/data-sets.html

data_paths <- list.files(here("_data"), full.names = TRUE, recursive = TRUE) 

en_01 <- nc_open(data_paths[1])
en_02 <- nc_open(data_paths[2])
en_03 <- nc_open(data_paths[3])



## -- set spatio-temporal coverage -- ##
# Set the latitude and longitude for Colorado
lat <- ncvar_get(en_01, "lat")
lon <- ncvar_get(en_01, "lon")

# Zoom in on Colorado
lats = which(lat >= 37 & lat <= 41)
lons = which(lon >= 251 & lon <= 256)
# the longitudes have already been subtracted from 360, per the requirements of the model
# Used Denver as the Eastern cut off for the state


# set the time frame
begin_time="1/15/2006"
end_time="12/15/2080"


# Combine dates into a sequence
all_time <- seq.dates(begin_time, end_time, by="months")


## -- math time -- ##
# Convert snowfall from mm/s to in/month
snow_01 <- ncvar_get(en_01, "SNOW") * (1/25.4) * (2.628e+6)
snow_02 <- ncvar_get(en_02, "SNOW") * (1/25.4) * (2.628e+6)
snow_03 <- ncvar_get(en_03, "SNOW") * (1/25.4) * (2.628e+6)

# Calculate Regional Average
snow1_regavg <- apply(snow_01[lons,lats,],3,sum)
snow2_regavg <- apply(snow_02[lons,lats,],3,sum)
snow3_regavg <- apply(snow_03[lons,lats,],3,sum)



# Combine Time and Regional Average
snow1_data <- data.frame(all_time, snow = round(snow1_regavg, 2))
snow2_data <- data.frame(all_time, snow = round(snow2_regavg, 2))
snow3_data <- data.frame(all_time, snow = round(snow3_regavg, 2))

# Add a month column for all climate ensembles
snow1_data$month = month(all_time)
snow2_data$month = month(all_time)
snow3_data$month = month(all_time)


## -- create ski seasons -- ##
# Create data frame that identifies months within a ski season, 
# taking into account that January of one year is part of the same ski season 
# for December of the previous year. Repeat this for all three climate ensembles.
snow_season_01 <- mutate(snow1_data, 
                         season_year = ifelse(month(all_time) == 1, year(all_time)-1, 
                                              ifelse(month(all_time) == 2, year(all_time)-1, 
                                                     ifelse(month(all_time) == 3, year(all_time)-1,
                                                            ifelse(month(all_time) == 4, year(all_time)-1,
                                                                   ifelse(month(all_time) == 5, year(all_time)-1,
                                                                          year(all_time)))))),
                         season = case_when(
                           month(all_time) %in% c(11, 12, 1, 2, 3, 4, 5) ~ "Ski_On",
                           month(all_time) %in% c(6, 7, 8, 9, 10) ~ "Ski_Off",
                           T ~ NA_character_
                         ))

snow_season_02 <- mutate(snow2_data, 
                         season_year = ifelse(month(all_time) == 1, year(all_time)-1, 
                                              ifelse(month(all_time) == 2, year(all_time)-1, 
                                                     ifelse(month(all_time) == 3, year(all_time)-1,
                                                            ifelse(month(all_time) == 4, year(all_time)-1,
                                                                   ifelse(month(all_time) == 5, year(all_time)-1,
                                                                          year(all_time)))))),
                         season = case_when(
                           month(all_time) %in% c(11, 12, 1, 2, 3, 4, 5) ~ "Ski_On",
                           month(all_time) %in% c(6, 7, 8, 9, 10) ~ "Ski_Off",
                           T ~ NA_character_
                         ))

snow_season_03 <- mutate(snow3_data, 
                         season_year = ifelse(month(all_time) == 1, year(all_time)-1, 
                                              ifelse(month(all_time) == 2, year(all_time)-1, 
                                                     ifelse(month(all_time) == 3, year(all_time)-1,
                                                            ifelse(month(all_time) == 4, year(all_time)-1,
                                                                   ifelse(month(all_time) == 5, year(all_time)-1,
                                                                          year(all_time)))))),
                         season = case_when(
                           month(all_time) %in% c(11, 12, 1, 2, 3, 4, 5) ~ "Ski_On",
                           month(all_time) %in% c(6, 7, 8, 9, 10) ~ "Ski_Off",
                           T ~ NA_character_
                         ))


## -- clean up ski season data -- ##
# Sum the total ski season snowfall from 2020 to 2080 for each climate ensemble.
snow_season_01_summary <- snow_season_01 %>%
  filter(season == "Ski_On") %>% 
  filter(season_year > 2019) %>% 
  filter(season_year< 2080) %>% 
  group_by(season_year) %>% 
  summarize(total_snow = sum(snow))

snow_season_02_summary <- snow_season_02 %>%
  filter(season == "Ski_On") %>%
  filter(season_year > 2019) %>% 
  filter(season_year< 2080) %>% 
  group_by(season_year) %>% 
  summarize(total_snow = sum(snow))

snow_season_03_summary <- snow_season_03 %>%
  filter(season == "Ski_On") %>% 
  filter(season_year > 2019) %>% 
  filter(season_year< 2080) %>% 
  group_by(season_year) %>% 
  summarize(total_snow = sum(snow))


## -- create ensemble time-series graphs -- ##
# Graph the time series of total predicted snowfall per ski season from 2020-2080, for each climate ensemble. Also, add in the predicted average high and low snowfall of 212.51 in/month and 133.21 in/month respectively
en_01_graph <- ggplot(snow_season_01_summary, aes(x = season_year, y = total_snow)) +
  geom_line(color="goldenrod") +
  geom_hline(yintercept = 212, color="#C70000", linetype="dashed")  +
  geom_hline(yintercept = 133, color="#009BB0", linetype="dashed") +
  scale_y_continuous(limits=c(0,300), expand=c(0,0)) +
  xlab("Year \n") + 
  ylab("Total Snow \n(in)") +
  ggtitle("Predicted Snowfall per Season (2020-2080) Under RCP 8.5 Ensemble 1") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman"))+
  theme(plot.margin = unit(c(1,1,2,1), "points"))

en_02_graph <- ggplot(snow_season_02_summary, aes(x = season_year, y = total_snow)) +
  geom_line(color="#7A5028") + 
  geom_hline(yintercept = 212, color="#C70000", linetype="dashed")  +
  geom_hline(yintercept = 133, color="#009BB0", linetype="dashed") +
  scale_y_continuous(limits=c(0,300), expand=c(0,0)) +
  xlab("Year \n") + 
  ylab("Total Snow \n(in)") +
  ggtitle("\nPredicted Snowfall per Season (2020-2080) Under RCP 8.5 Ensemble 2") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman"))+
  theme(plot.margin = unit(c(1,1,2,1), "points"))


en_03_graph <- ggplot(snow_season_03_summary, aes(x = season_year, y = total_snow)) +
  geom_line(color="#114C54") +
  geom_hline(yintercept = 212, color="#C70000", linetype="dashed")  +
  geom_hline(yintercept = 133, color="#009BB0", linetype="dashed") +
  scale_y_continuous(limits=c(0,300), expand=c(0,0)) +
  xlab("Year \n") + 
  ylab("Total Snow \n(in)") +
  ggtitle("\nPredicted Snowfall per Season (2020-2080) Under RCP 8.5 Ensemble 3") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text=element_text(family="Times New Roman"))


## -- consolidate graphs -- ##
wrap_plots(en_01_graph / en_02_graph / en_03_graph)