#UPPH '24 class manuscript
#Fig 3: empirical air pollution measurements from stations near Ithaca

#set up work environment
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(cowplot)



#### download air quality data from EPA for sensitivity analyses 
#install.packages(pkgs="RAQSAPI", dependencies = TRUE )
library(RAQSAPI)
aqs_key <- read_file("dk_epa_raqsapi_key.txt")
aqs_credentials(username = "dankatz@cornell.edu", key = aqs_key)


### get codes for counties whose AQ data appear in i-Tree
#note: it isn't possible to manually enter data from other sources into i-Tree

#aqs_states()
NY_counties_to_include <- c("Steuben", #Corning, pop density = 26/km2
                            "Monroe", #Rochester, pop density = 210/km2
                            "Tioga", #Owego, pop density = 36/km2
                            "Onondaga", #Syracuse, pop density = 200/km2
                            "Tompkins") #Ithaca, pop density 86/km2
PA_counties_to_include <- c("Lackawanna", #Scranton, pop density: 177/km2
                            "Bradford", #Sayre, pop density: 20/km2
                            "Luzerne") #Wilkes-Barre, pop density: 140/km2
NY_counties <- aqs_counties_by_state(stateFIPS = 36) %>% #NY = 36, PA = 42
    filter(county_name %in% NY_counties_to_include) %>% 
    mutate(state_code = "36")
PA_counties <- aqs_counties_by_state(stateFIPS = 42) %>% #NY = 36, PA = 42
  filter(county_name %in% PA_counties_to_include) %>% 
  mutate(state_code = "42")

all_counties <- bind_rows(NY_counties, PA_counties)

### air quality parameter codes
#https://aqs.epa.gov/data/api/list/parametersByClass?email=test@aqs.api&key=test&pc=CRITERIA
o3_code <- "44201"
so2_code <- "42401"
no2_code <- "42602"
pm25_code <- "88101"
co_code <- "42101"
all_aq_codes <- c(o3_code, so2_code, no2_code, pm25_code, co_code)

#download the data for a single site and year
# test <- aqs_annualsummary_by_site(parameter = "44201", bdate = as.Date("20170618", format="%Y%m%d"), 
#                           edate = as.Date("20190618", format="%Y%m%d"), stateFIPS = "37", countycode = "183", sitenum = "0014" )
# 
# test2 <- aqs_annualsummary_by_county( parameter = o3_code, 
#                                       bdate = as.Date("20010101", format="%Y%m%d"), 
#                                       edate = as.Date("20071231", format="%Y%m%d"),
#                                       stateFIPS = 36, countycode = NY_counties$county_code[NY_counties$county_name == "Monroe"], return_header = FALSE )

#note: I already saved csv created in this section, so re-load that later in the script instead of re-running the download section

#go through all counties for all variables for 2000-2023 using a for loop
aq_df <- list()
for(i in 1:nrow(all_counties)){ #takes ~95 min for 2000-2023
  for(j in 1:5){
  focal_county_parameter <- aqs_annualsummary_by_county( parameter = all_aq_codes[j], 
                                        bdate = as.Date("19700101", format="%Y%m%d"), 
                                        edate = as.Date("19991231", format="%Y%m%d"),
                                        stateFIPS = all_counties$state_code[i], countycode = all_counties$county_code[i], return_header = FALSE )
  aq_df <- bind_rows(aq_df, focal_county_parameter)
  print(paste(j, i, all_counties$state_code[i], all_counties$county_code[i], all_aq_codes[j], sep = " - "))
}}

#save data
#write_csv(aq_df, file.path("epa_aq_data","aq_annual_download_1970_2000.csv"))
aq_df_1970_2000 <- read_csv(file.path("epa_aq_data","aq_annual_download_1970_2000.csv")) %>% 
          mutate(state_code = as.character(state_code))
aq_df_2000_2023 <- read_csv(file.path("epa_aq_data","aq_annual_download.csv")) %>% 
  mutate(state_code = as.character(state_code))
aq_df <- rbind(aq_df_1970_2000, aq_df_2000_2023)

aq_df_viz <- left_join(aq_df, all_counties)
  
# str(aq_df)
# str(all_counties)


### load in national average data for air pollutants ##############################################
#manually download csv's from EPA's website
#https://www.epa.gov/air-trends/nitrogen-dioxide-trends #click on: "view chart data in Excel" link

nat_o3 <- read_csv("C:/Users/dsk273/Box/classes/plants and public health fall 2024/Ithaca class manuscript/air pollution data/OzoneNational.csv") %>% 
                     janitor::clean_names()
nat_pm <- read_csv("C:/Users/dsk273/Box/classes/plants and public health fall 2024/Ithaca class manuscript/air pollution data/PM25National.csv") %>% 
  janitor::clean_names()
nat_so2 <- read_csv("C:/Users/dsk273/Box/classes/plants and public health fall 2024/Ithaca class manuscript/air pollution data/Sulfur_DioxideNational.csv") %>% 
  janitor::clean_names()
nat_no2 <- read_csv("C:/Users/dsk273/Box/classes/plants and public health fall 2024/Ithaca class manuscript/air pollution data/Nitrogen_DioxideNational.csv") %>% 
  janitor::clean_names()
nat_co <- read_csv("C:/Users/dsk273/Box/classes/plants and public health fall 2024/Ithaca class manuscript/air pollution data/Carbon_MonoxideNational.csv") %>% 
  janitor::clean_names()


### visualize data ################################################################################
#panel for o3
# focal_var <- "Ozone" 
# sites_most_years_per_county_o3 <- aq_df_viz %>% 
#   filter(parameter == focal_var) %>% 
#   group_by(county_name, site_number) %>% 
#   summarize(min_year = min(year),
#             max_year = max(year)) %>% 
#   mutate(most_years = max_year - min_year) %>% 
#   group_by(county_name) %>% 
#   top_n(., 1) %>% 
#   filter(site_number != "2006")  #two sites had equal number of years of data

panel_O3 <-
  aq_df_viz %>% 
  filter(parameter == "Ozone") %>% 
  filter(year > 1979) %>%
  filter(sample_duration == "1 HOUR") %>% 
  filter(observation_percent > 90 & validity_indicator == "Y") %>% 
  #filter(site_number %in% sites_most_years_per_county_o3$site_number) %>% 
  group_by(county_name, year) %>% arrange(county_name, year)  %>%  
  slice_max(order_by = observation_count, n = 1, with_ties = FALSE) %>% 
  mutate(county_fig = case_when(county_name == "Monroe" ~ "selected source",
                                county_name != "Monroe" ~ "nearby source"))  %>%  
  left_join(., nat_o3) %>% 
ggplot(aes(x = year, y = fourth_max_value, 
           color = county_fig, 
           fill = "national average",
           ymax = x90th_percentile, ymin = x10th_percentile))  + 
  ggthemes::theme_few() + 
  xlab("year") + ylab("ozone (ppb)") + 
   geom_ribbon(alpha = 0.2, color = NA) +
  scale_fill_manual(name = "", values = "cornflowerblue") +
  geom_point() +
  geom_line(aes(x= year, y = fourth_max_value, group = county_name, color = county_fig), inherit.aes = FALSE, alpha = 0.3) +
  scale_color_manual(values = c("gray40", "chocolate"), name = "data source") +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  theme(legend.position="none")

   




#panel for PM
panel_PM <-
  aq_df_viz %>% 
  filter(parameter == "PM2.5 - Local Conditions") %>%  #aq_df_viz$parameter
  filter(metric_used == "Daily Mean") %>% 
  filter(observation_percent > 90 & validity_indicator == "Y") %>% 
  #filter(year < 2023) %>% 
  filter(pollutant_standard == "PM25 24-hour 2024") %>% 
  filter(event_type != "Events Excluded" & event_type != "Concurred Events Excluded") %>% 
  group_by(county_name, year) %>% arrange(county_name, year)  %>%  
  slice_max(order_by = observation_count, n = 1, with_ties = FALSE) %>% 
  mutate(county_fig = case_when(county_name == "Monroe" ~ "selected source",
                                county_name != "Monroe" ~ "nearby source"))  %>%  
  left_join(., nat_pm) %>% 
  ggplot(aes(x = year, y = arithmetic_mean, 
             color = county_fig, 
             fill = "national average",
             ymax = x90th_percentile, ymin = x10th_percentile))  + 
  ggthemes::theme_few() +  xlab("year") + ylab("PM2.5 (ug/M3)") +
  geom_ribbon(alpha = 0.2, color = NA) +
  scale_fill_manual(name = "", values = "cornflowerblue") +
  geom_point() +
  geom_line(aes(x= year, y = arithmetic_mean, group = county_name, color = county_fig), inherit.aes = FALSE, alpha = 0.3) +
  scale_color_manual(values = c("gray40", "chocolate"), name = "data source") +
  guides(fill = guide_legend(override.aes = list(shape = NA)))+
  theme(legend.position="none")



#panel for CO
panel_CO <- 
  aq_df_viz %>% 
  filter(parameter == "Carbon monoxide") %>% 
  #filter(metric_used == "Obseved hourly values") %>% 
  filter(sample_duration == "8-HR RUN AVG END HOUR") %>% 
  filter(observation_percent > 90 & validity_indicator == "Y") %>% 
  filter(year > 1979) %>% 
  group_by(county_name, year) %>% arrange(county_name, year)  %>%  
  slice_max(order_by = observation_count, n = 1, with_ties = FALSE) %>% 
  mutate(county_fig = case_when(county_name == "Monroe" ~ "selected source",
                                county_name != "Monroe" ~ "nearby source"))  %>%  
  left_join(., nat_co) %>% 
    ggplot(aes(x = year, y = second_max_value, 
               color = county_fig, 
               fill = "national average",
               ymax = x90th_percentile, ymin = x10th_percentile))  + 
    ggthemes::theme_few() +  xlab("year") + ylab("CO (ppm)") +
    geom_ribbon(alpha = 0.2, color = NA) +
    scale_fill_manual(name = "", values = "cornflowerblue") +
    geom_point() +
    geom_line(aes(x= year, y = second_max_value, group = county_name, color = county_fig), inherit.aes = FALSE, alpha = 0.3) +
    scale_color_manual(values = c("gray40", "chocolate"), name = "data source") +
    guides(fill = guide_legend(override.aes = list(shape = NA)))+
  theme(legend.position="none")
  
  



#panel for SO2
panel_SO2 <-
  aq_df_viz %>% 
    filter(parameter == "Sulfur dioxide") %>% 
    filter(sample_duration == "1 HOUR")  %>% 
  filter(pollutant_standard == "SO2 1-hour 2010") %>% 
    filter(observation_percent > 90 & validity_indicator == "Y") %>% 
  mutate(county_fig = case_when(site_number == "1007" ~ "selected source",
                                site_number != "1007" ~ "nearby source"))  %>%  
    filter(site_address != "MILTON AND HALL AVENUES") %>% 
  left_join(., nat_so2) %>% 
  ggplot(aes(x = year, y = ninety_ninth_percentile, 
             color = county_fig,
             fill = "national average",
             ymax = x90th_percentile, ymin = x10th_percentile))  + 
  ggthemes::theme_few() +  xlab("year") + ylab("SO2 (ppm)") +
  geom_ribbon(alpha = 0.2, color = NA) +
  scale_fill_manual(name = "", values = "cornflowerblue") +
  geom_point() +
  geom_line(aes(x= year, y = ninety_ninth_percentile, group = site_number, color = county_fig), inherit.aes = FALSE, alpha = 0.3) +
  scale_color_manual(values = c("gray40", "chocolate"), name = "data source") +
  guides(fill = guide_legend(override.aes = list(shape = NA)))+
  theme(legend.position="none")

  
#panel for NO2
panel_NO2_leg <-
  aq_df_viz %>% 
    filter(parameter == "Nitrogen dioxide (NO2)") %>% 
    filter(pollutant_standard == "NO2 1-hour 2010") %>% 
    filter(observation_percent > 90 & validity_indicator == "Y") %>% 
    filter(year > 1985) %>% 
  mutate(county_fig = case_when(county_name == "Lackawanna" ~ "selected source",
                                county_name != "Lackawanna" ~ "nearby source"))  %>%  
  left_join(., nat_no2) %>% 
  ggplot(aes(x = year, y = ninety_eighth_percentile, 
             color = county_fig, 
             fill = "national average",
             ymax = x90th_percentile, ymin = x10th_percentile))  + 
  ggthemes::theme_few() +  xlab("year") + ylab("NO2 (ppb)") +
  geom_ribbon(alpha = 0.2, color = NA) +
  scale_fill_manual(name = "", values = "cornflowerblue") +
  geom_point() +
  geom_line(aes(x= year, y = ninety_eighth_percentile, group = county_name, color = county_fig), inherit.aes = FALSE, alpha = 0.3) +
  scale_color_manual(values = c("gray40", "chocolate"), name = "data source") +
  guides(fill = guide_legend(override.aes = list(shape = NA)))
  
legend <- get_legend(panel_NO2_leg)
panel_NO2 <- panel_NO2_leg +  theme(legend.position="none")


#combine AQ figs
plot_grid(panel_O3, panel_PM, panel_CO, panel_NO2, panel_SO2, legend, ncol = 2,
          labels = c("A","B","C","D","E", ""))

ggsave("AQ_fig_3.jpeg", width = 7, height = 9, dpi = 300)


