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

here()
getwd()


species_lookup_table <- read_csv(file.path("Data for Final Manuscript","species_lookup_table.csv")) %>% 
  mutate(common_name = tolower(common_name))

species_lookup_table2 <- read_csv(file.path("Create_iTree_spp_codes_from_inventory_list.csv")) %>% clean_names() %>% 
  select(genus_name, species_name, common_name)
  
city_trees <- read_csv(file.path("Ithaca_city_trees.csv")) %>% clean_names() %>% 
  mutate(genus_name = gsub( " .*$", "", spp_bot ),
         ba = )


### download output from i-Tree Eco analyses on Google Drive ###################
#manually download the folder from google drive and place it in the folder

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

names(aq)

aq_sp_yr <-  aq %>% 
  group_by(census, genus_name) %>% 
  summarize(co_removed_oz_yr = sum(co_removed_oz_yr),
            o3_removed_oz_yr  = sum(o3_removed_oz_yr ),
            no2_removed_oz_yr  = sum(no2_removed_oz_yr ),
            so2_removed_oz_yr  = sum(so2_removed_oz_yr ),
            pm2_5_removed_oz_yr  = sum(pm2_5_removed_oz_yr )) 

#which genera to focus data viz on?
city_trees %>% 

aq_sp_yr %>% 
  group_by(genus_name) %>% 
  filter(!is.na(genus_name)) %>% 
  summarize(total_removed_co = sum(co_removed_oz_yr)) %>% 
  arrange(-total_removed_co) %>% 
  mutate(gen_order = rank(-total_removed_co))

aq_sp_yr %>% 
  group_by(genus_name) %>% 
  filter(!is.na(genus_name)) %>% 
  summarize(total_removed_o3 = sum(o3_removed_oz_yr)) %>% 
  arrange(-total_removed_o3) %>% 
  mutate(gen_order = rank(-total_removed_o3))

aq_sp_yr %>% 
  group_by(genus_name) %>% 
  filter(!is.na(genus_name)) %>% 
  summarize(total_removed_o3 = sum(o3_removed_oz_yr)) %>% 
  arrange(-total_removed_o3) %>% 
  mutate(gen_order = rank(-total_removed_o3))

### figure: effects of trees over time for each exposure #######################



### figure: visualize changes in the number and abundance of each species ######



### figure: human vulnerability over time ######################################



### figure: predictions in changes of ES/D in near future ######################