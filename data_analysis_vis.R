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

here()
getwd()

### download output from i-Tree Eco analyses on Google Drive ###################
#manually download the folder from google drive and place it in the folder

### pollen ############################################
pollen_annual <- read_csv(file.path("Data for Final Manuscript","annual_pollen_prod.csv")) %>% 
  mutate(common_name = trimws(gsub("[^A-Za-z[:space:]]","",species_pollenprod)),
         pollen_prod = as.numeric(gsub("[^0-9.-]", "", gsub("\\..*","", species_pollenprod))),
         common_name = tolower(common_name)) %>% 
  select(-species_pollenprod)

species_lookup_table <- read_csv(file.path("Data for Final Manuscript","species_lookup_table.csv")) %>% 
  mutate(common_name = tolower(common_name))

pollen_annual <- left_join(pollen_annual, species_lookup_table) %>% 
  mutate(genus_species = paste(genus, species, sep = " ")) %>% 
  mutate(genus_species = fct_reorder(.f = genus_species, .x = -pollen_prod))

ggplot(pollen_annual, aes(x = census, y = pollen_prod, color = genus_species)) + geom_point() + geom_line() + 
   theme_few() + scale_y_log10(label=comma) + ylab("pollen production (billions of grains/yr)") + xlab("year") +
  scale_color_discrete(name = "Species") +  theme(legend.text = element_text(face="italic")) +
  annotation_logticks()
#facet_wrap(~common_name) +


### air pollution ##############################################################



### figure: effects of trees over time for each exposure #######################



### figure: visualize changes in the number and abundance of each species ######



### figure: human vulnerability over time ######################################



### figure: predictions in changes of ES/D in near future ######################