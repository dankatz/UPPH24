#R script for data visualization and analysis

#set up work environment
library(googledrive)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(readr)
library(stringi)
library(ggthemes)
library(forcats)
library(scales)
library(janitor)
library(cowplot)
library(lubridate)


here()
getwd()


species_lookup_table <- read_csv(file.path("Data for Final Manuscript","species_lookup_table.csv")) %>% 
  mutate(common_name = tolower(common_name))

species_lookup_table2 <- read_csv(file.path("Create_iTree_spp_codes_from_inventory_list.csv")) %>% clean_names() %>% 
  select(genus_name, species_name, common_name)
  
city_trees <- read_csv(file.path("Ithaca_city_trees.csv")) %>% clean_names() %>% 
  mutate(genus_name = gsub( " .*$", "", spp_bot ),
         genus_name = stringr::str_to_title(genus_name),
         ba_in = (pi * (dbh/2)^2)/144) %>% 
  filter(genus_name != "Stump")


### download output from i-Tree Eco analyses on Google Drive ###################
#manually download the folder from google drive and place it in the folder


### changes in environmental conditions over time ##############################
met <- read_csv("C:/Users/dsk273/Box/classes/plants and public health fall 2024/Ithaca class manuscript/CU_daily_met_USC00304174.csv") %>% 
  janitor::clean_names() %>% 
  mutate(met_date = lubridate::mdy(date), #first decades are in ymd
         met_year = year(met_date),
         met_month = month(met_date),
         temp = tmax*0.1, #(tmax + tmin)/2 * 0.1, #in tenths of deg C
         prcp = prcp*0.1) #in 10ths of mm

future_clim_temp <- read_csv("C:/Users/dsk273/Box/classes/plants and public health fall 2024/Ithaca class manuscript/Tompkins_County-annual-proj_mod-tmax.csv") %>% 
  janitor::clean_names() %>% 
  rename_with( ~ paste0("temp_", .x)) %>% 
  mutate(met_year = temp_year)

future_clim_pcp <- read_csv("C:/Users/dsk273/Box/classes/plants and public health fall 2024/Ithaca class manuscript/Tompkins_County-annual-proj_mod-pcpn.csv") %>% 
  janitor::clean_names() %>% 
  rename_with( ~ paste0("pcp_", .x)) %>% 
  mutate(met_year = pcp_year)

future_clim <- left_join(future_clim_temp, future_clim_pcp) %>% 
  mutate(temp_rcp45_weighted_mean_c = 5/9 * (temp_rcp45_weighted_mean - 32),
         temp_rcp45_min_c = 5/9 * (temp_rcp45_min - 32),
         temp_rcp45_max_c = 5/9 * (temp_rcp45_max - 32)) %>% 
  mutate(pcp_rcp45_weighted_mean_cm = 2.54 * pcp_rcp45_weighted_mean,
         pcp_rcp45_min_cm = 2.54 * pcp_rcp45_min,
         pcp_rcp45_max_cm = 2.54 * pcp_rcp45_max) %>% 
  filter(met_year > 2023,
         met_year < 2060)
         
  
fig_temp <- 
  met %>% 
  filter(met_year > 1982) %>% 
  filter(met_year != 1994) %>% 
  filter(met_year != 2024) %>% 
  group_by(met_year) %>% 
  summarize(met_temp_mon = mean(temp),
            met_prcp_mon = mean(prcp)) %>% 
  ggplot(aes(x= met_year, y = met_temp_mon)) + geom_line() + geom_point() + theme_few() +
  ylab("mean annual temperature (°C)") + xlab("") +
    geom_ribbon(data = future_clim, aes(x = met_year, y = temp_rcp45_weighted_mean_c,
                                        ymin = temp_rcp45_min_c, ymax = temp_rcp45_max_c), fill = "red", alpha = 0.2) +
    geom_line(data = future_clim, aes(x = met_year, y = temp_rcp45_weighted_mean_c), color = "red4", alpha = 0.6, lwd = 1.1) 
  

fig_precip <- 
  met %>% 
  filter(met_year > 1982) %>% 
  filter(met_year != 2024) %>% 
  #filter(met_year != 1994) %>% 
  group_by(met_year) %>% 
  summarize(met_temp_mon = mean(temp, na.rm = TRUE),
            met_prcp_mon = sum(prcp, na.rm = TRUE)) %>% 
  ggplot(aes(x= met_year, y = met_prcp_mon/10)) + geom_line() + geom_point() + theme_few() +
  ylab("annual precipitation (cm)") + xlab("") + 
    geom_ribbon(data = future_clim, aes(x = met_year, y = pcp_rcp45_weighted_mean_cm,
                                        ymin = pcp_rcp45_min_cm, ymax = pcp_rcp45_max_cm), fill = "red", alpha = 0.2) +
    geom_line(data = future_clim, aes(x = met_year, y = pcp_rcp45_weighted_mean_cm), color = "red4", alpha = 0.6, lwd = 1.1) 
  

plot_grid(fig_temp, fig_precip, ncol = 1)





### pollen ############################################
pollen_annual <- read_csv(file.path("Data for Final Manuscript","annual_pollen_prod.csv")) %>% 
  mutate(common_name = trimws(gsub("[^A-Za-z[:space:]]","",species_pollenprod)),
         pollen_prod = as.numeric(gsub("[^0-9.-]", "", gsub("\\..*","", species_pollenprod))),
         common_name = tolower(common_name)) %>% 
  select(-species_pollenprod)

pollen_annual <- left_join(pollen_annual, species_lookup_table) %>% 
  mutate(genus_species = paste(genus, species, sep = " ")) %>% 
  mutate(genus_species = fct_reorder(.f = genus_species, .x = -pollen_prod))

ggplot(pollen_annual, aes(x = census, y = pollen_prod, color = genus_species)) + geom_point() + geom_line() + 
   theme_few() + scale_y_log10(label=comma) + ylab("pollen production (billions of grains/yr)") + xlab("year") +
  scale_color_discrete(name = "Species") +  theme(legend.text = element_text(face="italic")) +
  annotation_logticks()
#facet_wrap(~common_name) +


### air pollution ##############################################################
#which genera to focus data viz on?
top_10_gen_ba_tib <- city_trees %>% group_by(genus_name) %>% summarize(total_ba_in = sum(ba_in)) %>% 
  arrange(-total_ba_in) %>%  top_n(10) %>% 
  mutate(top_10_gen_ba = total_ba_in) #including for a later merge

top_10_gen_ba <- top_10_gen_ba_tib %>% pull(genus_name)#%>% #print(n = 20) #left_join(., species_lookup_table2)

#assemble data
aq_tree_05 <- read_csv(file.path("Data for Final Manuscript", "2005", "Data by Tree","pollutionRemovalByTree.csv")) %>% 
  mutate(census = 2005)
aq_tree_13 <- read_csv(file.path("Data for Final Manuscript", "2013", "Data by Tree","pollutionRemovalByTree_2013.csv"))%>% 
  mutate(census = 2013)
aq_tree_19 <- read_csv(file.path("Data for Final Manuscript", "2019", "Data by Tree","pollutionRemovalByTree_2019.csv"))%>% 
  mutate(census = 2019)
aq_tree_21 <- read_csv(file.path("Data for Final Manuscript", "2021", "Data by Tree","pollutionRemovalByTree_2021.csv"))%>% 
  mutate(census = 2021)

aq <- bind_rows(aq_tree_05, aq_tree_13, aq_tree_19, aq_tree_21) %>% 
  clean_names() %>% 
  rename(common_name = species_name) %>% 
  left_join(., species_lookup_table2) %>% 
  mutate(genus_species = paste(genus_name, species_name, sep = " "))


aq_gen10_yr <-  aq %>% 
  group_by(census, genus_name) %>% 
  summarize(co_removed_kg_yr = sum(co_removed_oz_yr) / 35.274,
            o3_removed_kg_yr  = sum(o3_removed_oz_yr)  / 35.274,
            no2_removed_kg_yr  = sum(no2_removed_oz_yr ) / 35.274,
            so2_removed_kg_yr  = sum(so2_removed_oz_yr ) / 35.274,
            pm2_5_removed_kg_yr  = sum(pm2_5_removed_oz_yr ) / 35.274) %>% 
  mutate(genus_nametop10 = case_when(genus_name %in% top_10_gen_ba ~ genus_name,
                              .default = "other")) %>% 
  group_by(census, genus_nametop10) %>% 
  summarize(co_removed_kg_yr = sum(co_removed_kg_yr),
            o3_removed_kg_yr = sum(o3_removed_kg_yr),
            no2_removed_kg_yr = sum(no2_removed_kg_yr),
            so2_removed_kg_yr = sum(so2_removed_kg_yr),
            pm2_5_removed_kg_yr = sum(pm2_5_removed_kg_yr)) 
  # left_join(., top_10_gen_ba_tib) %>% #to reorder the fig by basal area
  # mutate(genus_nametop10 = fct_reorder(.f = genus_nametop10, .x = -total_ba_in ))

  


#Carbon Monoxide removal
fig_co <- aq_gen10_yr %>% 
  ggplot(aes(x = census, y = co_removed_kg_yr, fill = genus_nametop10)) + 
  #geom_area(alpha = 0.2) + 
  geom_bar(position="stack", stat = "identity")+
  theme_few() + ylab("CO removed (kg/yr)") + scale_fill_discrete(name = "") +  
  theme(legend.position="none")

#ozone removal
fig_o3 <- aq_gen10_yr %>% 
  ggplot(aes(x = census, y = o3_removed_kg_yr, fill = genus_nametop10)) + 
  geom_bar(position="stack", stat = "identity")+
  theme_few() + ylab("ozone removed (kg/yr)") + scale_fill_discrete(name = "") +  
  theme(legend.position="none")

#NO2 removal
fig_no2 <- aq_sp_yr %>% 
  mutate(top10_ba = case_when(genus_name %in% top_10_gen_ba ~ genus_name,
                              .default = "other")) %>% 
  ggplot(aes(x = census, y = no2_removed_kg_yr , fill = top10_ba)) + 
  # geom_area() + 
  geom_bar(position="stack", stat = "identity")+
  theme_few() + ylab("nitrogen dioxide removed (kg/yr)") + scale_fill_discrete(name = "") +  
  theme(legend.position="none")

#SO2 removal
fig_so2 <- aq_sp_yr %>% 
  mutate(top10_ba = case_when(genus_name %in% top_10_gen_ba ~ genus_name,
                              .default = "other")) %>% 
  ggplot(aes(x = census, y = so2_removed_kg_yr , fill = top10_ba)) + 
  # geom_area() + 
  geom_bar(position="stack", stat = "identity")+
  theme_few() + ylab("sulfur dioxide removed (kg/yr)") + scale_fill_discrete(name = "") +  
  theme(legend.position="none")

#PM2.5 removal
fig_pm <- aq_sp_yr %>% 
  mutate(top10_ba = case_when(genus_name %in% top_10_gen_ba ~ genus_name,
                              .default = "other")) %>% 
  ggplot(aes(x = census, y = pm2_5_removed_kg_yr, fill = top10_ba)) + 
 # geom_area() + 
  geom_bar(position="stack", stat = "identity")+
  theme_few() + ylab("PM2.5 removed (kg/yr)") + scale_fill_discrete(name = "") +  
  theme(legend.position="none")
#get a shared legend
#fig to get legend from
fig_leg <- aq_gen10_yr %>% 
  ggplot(aes(x = census, y = co_removed_kg_yr, fill = genus_nametop10)) + 
  #geom_area(alpha = 0.2) + 
  geom_bar(position="stack", stat = "identity")+
  theme_few() + ylab("CO removed (kg/yr)") + scale_fill_discrete(name = "tree genera") +
  guides(fill=guide_legend(ncol=2)) +  theme(legend.text = element_text(face="italic")) 
legend <- get_legend(fig_leg)


#combine AQ figs
plot_grid(fig_o3, fig_pm, fig_co, fig_so2, fig_no2, legend, ncol = 2,
         labels = c("A","B","C","D","E", ""))


### hydrology ##############################################################
#which genera to focus data viz on?
top_10_gen_ba_tib <- city_trees %>% group_by(genus_name) %>% summarize(total_ba_in = sum(ba_in)) %>% 
  arrange(-total_ba_in) %>%  top_n(10) %>% 
  mutate(top_10_gen_ba = total_ba_in) #including for a later merge

top_10_gen_ba <- top_10_gen_ba_tib %>% pull(genus_name)#%>% #print(n = 20) #left_join(., species_lookup_table2)

#assemble data
h_tree_05 <- read_csv(file.path("Data for Final Manuscript", "2005", "Data by Tree","hydroEffectsByTree.csv")) %>% 
  mutate(census = 2005)
h_tree_13 <- read_csv(file.path("Data for Final Manuscript", "2013", "Data by Tree","hydroEffectsByTree_2013.csv"))%>% 
  mutate(census = 2013)
h_tree_19 <- read_csv(file.path("Data for Final Manuscript", "2019", "Data by Tree","hydroEffectsByTree_2019.csv"))%>% 
  mutate(census = 2019)
h_tree_21 <- read_csv(file.path("Data for Final Manuscript", "2021", "Data by Tree","hydroEffectsByTree_2021.csv"))%>% 
  mutate(census = 2021)

hy <- bind_rows(h_tree_05, h_tree_13, h_tree_19, h_tree_21) %>% 
  clean_names() %>% 
  rename(common_name = species_name) %>% 
  left_join(., species_lookup_table2) %>% 
  mutate(genus_species = paste(genus_name, species_name, sep = " "))


hy_gen10_yr <-  hy %>% 
  group_by(census, genus_name) %>% 
  summarize(leaf_area_m2 = sum(leaf_area_ft_2 ) / 10.764,
            h20_intercepted_m3  = sum(water_intercepted_gal_yr )  / 264.172,
            avoided_runoff_m3  = sum(avoided_runoff_gal_yr  ) / 264.172) %>% 
  mutate(genus_nametop10 = case_when(genus_name %in% top_10_gen_ba ~ genus_name,
                                     .default = "other")) %>% 
  group_by(census, genus_nametop10) %>% 
  summarize(leaf_area_m2 = sum(leaf_area_m2),
            h20_intercepted_m3 = sum(h20_intercepted_m3),
            avoided_runoff_m3 = sum(avoided_runoff_m3)) 
# left_join(., top_10_gen_ba_tib) %>% #to reorder the fig by basal area
# mutate(genus_nametop10 = fct_reorder(.f = genus_nametop10, .x = -total_ba_in ))


#runoff avoided
fig_runoff <- hy_gen10_yr %>% 
  ggplot(aes(x = census, y = avoided_runoff_m3 , fill = genus_nametop10)) + 
  #geom_area(alpha = 0.2) + 
  geom_bar(position="stack", stat = "identity")+ scale_y_continuous(label=comma) +
  theme_few() + ylab(expression(paste(runoff~avoided~m^3~"/yr"))) + scale_fill_discrete(name = "") +  
  theme(legend.position="none") 

#water intercepted
fig_h20intercept <- hy_gen10_yr %>% 
  ggplot(aes(x = census, y = h20_intercepted_m3 , fill = genus_nametop10)) + 
  #geom_area(alpha = 0.2) + 
  geom_bar(position="stack", stat = "identity")+ scale_y_continuous(label=comma) +
  theme_few() + ylab(expression(paste(water~intercepted~m^3~"/yr"))) + scale_fill_discrete(name = "") +  
  theme(legend.position="none")


#get a shared legend
#fig to get legend from
fig_leg <- hy_gen10_yr %>% 
  ggplot(aes(x = census, y = h20_intercepted_m3, fill = genus_nametop10)) + 
  geom_bar(position="stack", stat = "identity")+
  scale_fill_discrete(name = "tree genera") +
  guides(fill=guide_legend(ncol=1)) +  theme(legend.text = element_text(face="italic")) 
legend <- get_legend(fig_leg)


#combine AQ figs
plot_grid(fig_runoff, fig_h20intercept, legend, ncol = 3, rel_widths = c(3,3,1),
          labels = c("A","B",""))



### figure: human vulnerability over time ######################################



### figure: predictions in changes of ES/D in near future ######################