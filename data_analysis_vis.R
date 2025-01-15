#R script for data visualization and analysis

#set up work environment
library(googledrive)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(readr)
library(stringi)
library(stringr)
library(ggthemes)
library(forcats)
library(scales)
library(janitor)
library(cowplot)
library(lubridate)
library(purrr)

here()
getwd()

#rm(list = ls())

### add tree census data ###############################################################################
species_lookup_table <- read_csv(file.path("Data for Final Manuscript","species_lookup_table.csv")) %>% 
  mutate(common_name = tolower(common_name))

species_lookup_table2 <- read_csv(file.path("Create_iTree_spp_codes_from_inventory_list.csv")) %>% clean_names() %>% 
  select(genus_name, species_name, common_name)
  
#load in version that Russell created to separate out street and park trees
st_pk_tree_dir <- "C:/Users/dsk273/Box/classes/plants and public health fall 2024/Ithaca class manuscript/UPPH Manuscript Files-20250115/UPPH Manuscript Files/Tree Information Spreadsheets/Park and Street Trees/"

#load in censuses based on i-Tree export (to include data cleaning done by Russell and Tommy)
Ithaca_Trees_2021 <- read.csv(file.path(paste0(st_pk_tree_dir, "2021_treesClass.csv"))) %>% 
  mutate(year_s = 2021,
         User.Tree.ID = as.character(User.Tree.ID))
Ithaca_Trees_2019 <- read.csv(file.path(paste0(st_pk_tree_dir, "2019_treesClass.csv"))) %>% 
  mutate(year_s = 2019,
         User.Tree.ID = as.character(User.Tree.ID))
Ithaca_Trees_2013 <- read.csv(file.path(paste0(st_pk_tree_dir, "2013_treesClass.csv"))) %>% 
  mutate(year_s = 2013,
         User.Tree.ID = as.character(User.Tree.ID)) %>% 
  rename(TreeClass = TREECLASS)
Ithaca_Trees_2002 <- read.csv(file.path(paste0(st_pk_tree_dir, "2002_treesClass.csv")))%>% 
  mutate(year_s = 2002,
         User.Tree.ID = as.character(User.Tree.ID)) %>% 
  rename(TreeClass = TREECLASS)
Ithaca_Trees_1997 <- read.csv(file.path(paste0(st_pk_tree_dir, "1997_treesClass.csv")))%>% 
  mutate(year_s = 1997,
         User.Tree.ID = as.character(User.Tree.ID))
Ithaca_Trees_1947Stratum <- read.csv(file.path("Data for Final Manuscript","1928-1947","ithacatrees_stratum1928-1947.csv")) %>% 
  mutate(User.Tree.ID = as.character(User.Tree.ID),
         year_s = 1947,
         TreeClass = "Street")

itree_dfs <- bind_rows(Ithaca_Trees_2021, Ithaca_Trees_2019, Ithaca_Trees_2013, Ithaca_Trees_2002, Ithaca_Trees_1997, 
                       Ithaca_Trees_1947Stratum) %>% 
  select(year_s, Species, DBH.1..in., TreeClass ) %>% 
  rowwise() %>% 
  mutate(Species_old = Species,
         common_name = sub("\\s*\\(.*", "", Species_old),
         species = str_extract_all(Species_old, "\\([^()]+\\)")[[1]],
         species = substring(species, 2, nchar(species)-1), #remove parentheses
         genus = gsub(" .*$", "", species)) %>%   #sub("\\s*\\(.*", "", Species_old))
  rename(DBH = DBH.1..in.) %>% 
  select(-Species, -Species_old) %>% 
  ungroup() 

Ithaca_Trees_1987 <- read.csv(file.path("Data for Final Manuscript","1987","1987Ithaca_trees.csv")) %>% 
  select(-tree_id, -code) %>% 
  rowwise() %>% 
  mutate(genus = gsub(" .*$", "", species),
         year_s = 1987,
         TreeClass = "Street") %>% 
  ungroup()

all_trees <- bind_rows(itree_dfs, Ithaca_Trees_1987) %>% 
  mutate( dbh_cm = 2.54 * DBH,
          ba_m2 = 0.00007854 * dbh_cm^2) %>% 
  filter(TreeClass != "Natural Area") %>% 
  filter(TreeClass != "Unassigned")

# all_trees %>% 
#   group_by(year_s, TreeClass) %>% 
#   summarize(n = n())
# unique(all_trees$TreeClass)

### download output from i-Tree Eco analyses on Google Drive ###################
#manually download the folder from google drive and place it in the folder
#https://drive.google.com/drive/folders/1iJu5UMsAL5N6PPd-kmbKz9KGCabi6WuH

### figures of number of trees and basal area at each census ###################
all_trees_top_10_gen_ba <- all_trees %>% group_by(genus) %>% summarize(total_ba_m2 = sum(ba_m2)) %>% 
  arrange(-total_ba_m2) %>%  top_n(10) %>% 
  mutate(top_10_gen = "top10")

#basal area figure
BA_fig_st <- all_trees %>% 
  filter(TreeClass == "Street") %>% 
  left_join(., all_trees_top_10_gen_ba) %>% 
  mutate(genus_cat = case_when(top_10_gen == "top10" ~ genus,
                               .default = "other")) %>% 
  group_by(genus_cat, year_s) %>% 
  summarize(total_ba = sum(ba_m2)) %>% 
  ggplot(aes(x = as.factor(year_s), y = total_ba, fill = genus_cat)) + geom_col() + ggthemes::theme_few() +
  ylab(expression("total basal area (" ~m^2~")"))+ xlab("year")+
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(face = "italic"))+
  scale_y_continuous(labels = scales::comma)+ 
  grafify::scale_fill_grafify(palette = "fishy", guide="none")

BA_fig_Park <- all_trees %>% 
  filter(TreeClass == "Park") %>% 
  left_join(., all_trees_top_10_gen_ba) %>% 
  mutate(genus_cat = case_when(top_10_gen == "top10" ~ genus,
                               .default = "other")) %>% 
  group_by(genus_cat, year_s) %>% 
  summarize(total_ba = sum(ba_m2)) %>% 
  ggplot(aes(x = as.factor(year_s), y = total_ba, fill = genus_cat)) + geom_col() + ggthemes::theme_few() +
  ylab(expression("total basal area (" ~m^2~")"))+ xlab("year")+
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(face = "italic"))+
  scale_y_continuous(labels = scales::comma)+ 
  grafify::scale_fill_grafify(palette = "fishy", guide="none")

#number of individuals
num_fig_st <- all_trees %>% 
  filter(TreeClass == "Street") %>% 
  left_join(., all_trees_top_10_gen_ba) %>% 
  mutate(genus_cat = case_when(top_10_gen == "top10" ~ genus,
                               .default = "other")) %>% 
  group_by(genus_cat, year_s) %>% 
  summarize(n_indiv = n()) %>% 
  ggplot(aes(x = as.factor(year_s), y = n_indiv, fill = genus_cat)) + geom_col() + ggthemes::theme_few() +
  ylab(expression("number of stems"))+ xlab("year")+
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(face = "italic"))+
  scale_y_continuous(labels = scales::comma) + 
  grafify::scale_fill_grafify(palette = "fishy", guide="none")

num_fig_Park <- all_trees %>% 
  filter(TreeClass == "Park") %>% 
  left_join(., all_trees_top_10_gen_ba) %>% 
  mutate(genus_cat = case_when(top_10_gen == "top10" ~ genus,
                               .default = "other")) %>% 
  group_by(genus_cat, year_s) %>% 
  summarize(n_indiv = n()) %>% 
  ggplot(aes(x = as.factor(year_s), y = n_indiv, fill = genus_cat)) + geom_col() + ggthemes::theme_few() +
  ylab(expression("number of stems"))+ xlab("year")+
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(face = "italic"))+
  scale_y_continuous(labels = scales::comma) + 
  grafify::scale_fill_grafify(palette = "fishy")

legend <- get_legend(num_fig_Park)
num_fig_Park <- num_fig_Park +  theme(legend.position="none")

cowplot::plot_grid(BA_fig_st, BA_fig_Park, legend, 
                   num_fig_st, num_fig_Park, ncol = 3, rel_widths = c(1, 0.75, 0.25),
                   labels = c("A","B", "","C", "D", ""))
 

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

#are there changes in temp over the period with data?
# test <- met %>% 
#   filter(met_year > 1982) %>% 
#   filter(met_year != 1994) %>% 
#   filter(met_year != 2024) %>% 
#   group_by(met_year) %>% 
#   summarize(met_temp_mon = mean(temp),
#             met_prcp_mon = mean(prcp))
#  
# fit <- glm(met_temp_mon ~ met_year, data = test) 
# summary(fit)

### pollen ############################################
# pollen_annual <- read_csv(file.path("Data for Final Manuscript","annual_pollen_prod.csv")) %>% 
#   mutate(common_name = trimws(gsub("[^A-Za-z[:space:]]","",species_pollenprod)),
#          pollen_prod = as.numeric(gsub("[^0-9.-]", "", gsub("\\..*","", species_pollenprod))),
#          common_name = tolower(common_name)) %>% 
#   select(-species_pollenprod)
# 
# pollen_annual <- left_join(pollen_annual, species_lookup_table) %>% 
#   mutate(genus_species = paste(genus, species, sep = " ")) %>% 
#   mutate(genus_species = fct_reorder(.f = genus_species, .x = -pollen_prod))


# #select only genera with pollen production equations
# pollen_prod_focal_genera <- c("Acer", "Betula", "Platanus", "Quercus", "Morus", "Populus", "Gleditsia", "Juglans", "Ulmus")
# it_dbh_genus_n <- it_dbh_genus %>% filter(Genus %in% pollen_prod_focal_genera)

all_trees_pol <- all_trees %>% 
  mutate(Genus = genus,
         Species = gsub("^\\S+ ", "", species),
         tree_BA = ba_m2)
  

#calculate pollen production for each individual from previous censuses, now including SDs
# unique(it_dbh_genus$sp)
for(i in 1:100){
  Acne_param_a <- rnorm(n = 1, mean = 253.71, sd = 47.75)
  Acne_param_b <- rnorm(n = 1, mean = 0.38, sd = 3.26)
  Acpl_param_a <- rnorm(n = 1, mean = 25.59, sd = 7.00)
  Acpl_param_b <- rnorm(n = 1, mean = 1.22, sd = 0.46)
  Acru_param_a <- rnorm(n = 1, mean = 62.32, sd = 13.50)
  Acru_param_b <- rnorm(n = 1, mean = 1.27, sd = 0.44)
  Acsa_param_a <- rnorm(n = 1, mean = 2.28, sd =0.49)
  Acsa_param_b <- rnorm(n = 1, mean = 21.98, sd =0.28)
  Bepa_param_a <- rnorm(n = 1, mean = 561.16, sd = 228.86)
  Bepa_param_b <- rnorm(n = 1, mean = 5.03, sd =4.42)
  Gltr_param_a <- rnorm(n = 1, mean = 659.91, sd =103.36)
  Gltr_param_b <- rnorm(n = 1, mean = -3.25, sd = 1.97)
  Juni_param_a <- rnorm(n = 1, mean = 239.08, sd = 64.85)
  Juni_param_b <- rnorm(n = 1, mean = 11.47, sd = 8.22)
  Mosp_param_a <- rnorm(n = 1, mean = -67.95, sd = 1366.09)
  Mosp_param_b <- rnorm(n = 1, mean = 254.06, sd = 93.26)*0.578
  Mosp_param_c <- rnorm(n = 1, mean = 6021.57, sd =2011.79)
  Plac_param_a <- rnorm(n = 1, mean = 1066.75, sd = 251.73)
  Plac_param_b <- rnorm(n = 1, mean = 1.26, sd = 8.15)
  Posp_param_a <- rnorm(n = 1, mean = 2.01, sd = 0.24)
  Posp_param_b <- rnorm(n = 1, mean = 24.17, sd = 0.19)
  Qusp_param_a <- rnorm(n = 1, mean = 423.56, sd = 85.45)
  Qusp_param_b <- rnorm(n = 1, mean = 36.20, sd = 11.42)
  Qupa_param_a <- rnorm(n = 1, mean = 327.2, sd =100.94)
  Qupa_param_b <- rnorm(n = 1, mean = 14.9, sd = 7.41)
  Ulsp_param_a <- rnorm(n = 1, mean = 546.56, sd = 89.86) #rnorm(n = 1, mean = 5.86, sd = 0.35)
  Ulsp_param_b <- rnorm(n = 1, mean = 23.76, sd = 17.06) #rnorm(n = 1, mean = 23.11, sd = 0.15)
  
  it_dbh_genus_np_i <-  #
    all_trees_pol %>%  
    mutate(per_tree_pollen_prod = case_when(
      Genus == "Acer" & Species == "negundo"  ~ ( Acne_param_a * tree_BA + Acne_param_b) *0.558, #.558 is the sex ratio,
      Genus == "Acer" & Species == "platanoides"  ~ Acpl_param_a * tree_BA + Acpl_param_b,
      Genus == "Acer" & Species == "rubrum"  ~ ( Acru_param_a * tree_BA + Acru_param_b) * 0.106, #.106 is the sex ratio
      Genus == "Acer" & Species == "saccharinum"~ (exp( Acsa_param_a * tree_BA + Acsa_param_b))/1000000000, #convert to billions
      Genus == "Betula"  ~ Bepa_param_a* tree_BA + Bepa_param_b,
      Genus == "Gleditsia"  ~ Gltr_param_a * tree_BA + Gltr_param_b,
      Genus == "Juglans"  ~ Juni_param_a * tree_BA + Juni_param_b,
      Genus == "Morus"  ~ (Mosp_param_c * tree_BA^2 + Mosp_param_a * tree_BA + Mosp_param_b) *0.578, #.58 adjusts for sex ratio
      Genus == "Platanus"  ~ Plac_param_a * tree_BA + Plac_param_b,
      Genus == "Populus"  ~ (exp( Posp_param_a * tree_BA + Posp_param_b) * 0.482)/1000000000, #convert to billions
      Genus == "Quercus"  ~ Qusp_param_a * tree_BA + Qusp_param_b, #red oaks and unknown oaks
      Genus == "Quercus" & Species == "palustris"  ~ Qupa_param_a * tree_BA + Qupa_param_b, #pin oaks
      Genus == "Ulmus"  ~ ( Ulsp_param_a * tree_BA + Ulsp_param_b) 
    ),
    iter = i ) #did a gut check against fig 3 in Katz et al. 2020; all of these currently line up
  
  
  ifelse(i == 1,
         it_dbh_genus_np_all <- it_dbh_genus_np_i,
         it_dbh_genus_np_all <- bind_rows(it_dbh_genus_np_all, it_dbh_genus_np_i))
  print(i)
}

#calculate total pollen production for each taxon
citywide_pol <- 
  it_dbh_genus_np_all %>% 
  mutate(p_all_trees = per_tree_pollen_prod,
         genus_species = paste(Genus, Species, sep = " ")) %>% 
  group_by(iter, Genus, year_s) %>% 
  summarize(total_p_bil = sum(p_all_trees, na.rm = TRUE) ) %>%  #adding each tree 
  filter(!is.na(total_p_bil)) %>% 
  filter(total_p_bil != 0) %>% 
  mutate(total_p = total_p_bil * 1000000000,
         total_p_tril = total_p / 10^12,
         total_p_quad = total_p / 10^15) %>% 
  group_by(Genus, year_s) %>% 
  summarize(total_p_bil_mean = mean(total_p_bil),
            total_p_bil_sd = sd(total_p_bil),
            total_p_quad_mean = mean(total_p_quad),
            total_p_quad_sd = sd(total_p_quad))

test <- citywide_pol %>% group_by(Genus, year_s) %>% 
  summarize(sum_pol = round(sum(total_p_bil_mean ), 3))

ggplot(citywide_pol, aes(x = year_s, y = total_p_bil_mean, color = Genus)) + geom_point() + geom_line() + 
   theme_few() + scale_y_log10(label=comma) + ylab("pollen production (billions of grains/yr)") + xlab("year") +
  scale_color_discrete(name = "Genus") +  theme(legend.text = element_text(face="italic")) +
  annotation_logticks()
#facet_wrap(~common_name) +


### fig 4:air pollution ##############################################################
oz_to_kg <- 35.27396195
census_years <- c("2005", "2013", "2019", "2021")

#which genera to focus data viz on?
top_10_gen_ba_tib <- city_trees %>% group_by(genus_name) %>% summarize(total_ba_in = sum(ba_in)) %>% 
  arrange(-total_ba_in) %>%  top_n(10) %>% 
  mutate(top_10_gen_ba = total_ba_in) #including for a later merge

top_10_gen_ba <- top_10_gen_ba_tib %>% pull(genus_name)#%>% #print(n = 20) #left_join(., species_lookup_table2)

#get the results from i-Tree
ts_data2_files <- dir("C:/Users/dsk273/Documents/UPPH24/Data for Final Manuscript/Updated projects with Rochester weather/pollutionRemoval",
                      full.names = TRUE)
read_ts_file_fun <- function(csv_filepath){
  df_focal <- read_csv(csv_filepath) %>%  
    clean_names() %>% 
    mutate(year = str_sub(csv_filepath, -8,-5))
}

ts_data2_raw <- purrr::map_dfr(ts_data2_files, .f = read_ts_file_fun) 

#get data at the genus level
aq <-  ts_data2_raw %>%
  rename(common_name = species_name) %>%
  left_join(., species_lookup_table2) %>%
  mutate(genus_species = paste(genus_name, species_name, sep = " "),
         census = year)

aq_gen10_yr <-  aq %>% 
  filter(year %in% census_years)%>% 
  group_by(census, year, genus_name) %>% 
  summarize(co_removed_kg_yr = sum(co_removed_oz_yr) / oz_to_kg,
            o3_removed_kg_yr  = sum(o3_removed_oz_yr)  / oz_to_kg,
            no2_removed_kg_yr  = sum(no2_removed_oz_yr  ) / oz_to_kg,
            so2_removed_kg_yr  = sum(so2_removed_oz_yr ) / oz_to_kg,
            pm2_5_removed_kg_yr  = sum(pm2_5_removed_oz_yr ) / oz_to_kg) %>% 
  mutate(genus_nametop10 = case_when(genus_name %in% top_10_gen_ba ~ genus_name,
                                     .default = "other")) %>% 
  group_by(census, year, genus_nametop10) %>% 
  summarize(co_removed_kg_yr = sum(co_removed_kg_yr),
            o3_removed_kg_yr = sum(o3_removed_kg_yr),
            no2_removed_kg_yr = sum(no2_removed_kg_yr),
            so2_removed_kg_yr = sum(so2_removed_kg_yr),
            pm2_5_removed_kg_yr = sum(pm2_5_removed_kg_yr)) %>% 
  mutate(year = as.numeric(year))

#get the full time series
ts_data <- ts_data2_raw %>%  
  group_by(year) %>% 
  summarize(co_oz_yr = sum(co_removed_oz_yr),
            o3_oz_yr = sum(o3_removed_oz_yr),
            no2_oz_yr = sum(no2_removed_oz_yr),
            so2_oz_yr = sum(so2_removed_oz_yr),
            pm_oz_yr = sum(pm2_5_removed_oz_yr)) %>% 
  mutate(co_kg_yr = co_oz_yr/oz_to_kg, 
         o3_kg_yr = o3_oz_yr/oz_to_kg, 
         no2_kg_yr = no2_oz_yr/oz_to_kg, 
         so2_kg_yr = so2_oz_yr/oz_to_kg, 
         pm_kg_yr = pm_oz_yr/oz_to_kg ) %>% 
  mutate(pm_kg_yr = case_when(pm_kg_yr == 0 ~ NA,
                              pm_kg_yr > 0 ~ pm_kg_yr),
         year = as.numeric(year))

#Carbon Monoxide removal
fig_co <- 
  aq_gen10_yr %>% 
  ggplot(aes(x = year, y = co_removed_kg_yr, fill = genus_nametop10)) + 
  #geom_area(alpha = 0.2) + 
  geom_bar(position="stack", stat = "identity") +
  theme_few() + ylab("CO removed (kg/yr)") + scale_fill_discrete(name = "") +  
  theme(legend.position="none")+
  geom_point(data = ts_data, aes( x= year, y = co_kg_yr ),  inherit.aes = FALSE) + 
  geom_line(data = ts_data, aes( x= year, y = co_kg_yr ),  inherit.aes = FALSE)

#ozone removal
fig_o3 <- aq_gen10_yr %>% 
  ggplot(aes(x = year, y = o3_removed_kg_yr, fill = genus_nametop10)) + 
  geom_bar(position="stack", stat = "identity")+
  theme_few() + ylab("O3 removed (kg/yr)") + scale_fill_discrete(name = "") +  
  theme(legend.position="none") +
  geom_point(data = ts_data, aes( x= year, y = o3_kg_yr),  inherit.aes = FALSE) + 
  geom_line(data = ts_data, aes( x= year, y = o3_kg_yr ),  inherit.aes = FALSE)


#NO2 removal
fig_no2 <- aq_gen10_yr %>% 
  # mutate(top10_ba = case_when(genus_nametop10 %in% top_10_gen_ba ~ genus_name,
  #                             .default = "other")) %>% 
  ggplot(aes(x = year, y = no2_removed_kg_yr , fill = genus_nametop10)) + 
  # geom_area() + 
  geom_bar(position="stack", stat = "identity")+
  theme_few() + ylab("NO2 removed (kg/yr)") + scale_fill_discrete(name = "") +  
  theme(legend.position="none") +
  geom_point(data = ts_data, aes( x= year, y = no2_kg_yr),  inherit.aes = FALSE) + 
  geom_line(data = ts_data, aes( x= year, y = no2_kg_yr),  inherit.aes = FALSE)



#SO2 removal
fig_so2 <- aq_gen10_yr %>% 
  # mutate(top10_ba = case_when(genus_name %in% top_10_gen_ba ~ genus_name,
  #                             .default = "other")) %>% 
  ggplot(aes(x = year, y = so2_removed_kg_yr , fill = genus_nametop10)) + 
  # geom_area() + 
  geom_bar(position="stack", stat = "identity")+
  theme_few() + ylab("SO2 removed (kg/yr)") + scale_fill_discrete(name = "") +  
  theme(legend.position="none")+
  geom_point(data = ts_data, aes( x= year, y = so2_kg_yr),  inherit.aes = FALSE) + 
  geom_line(data = ts_data, aes( x= year, y = so2_kg_yr),  inherit.aes = FALSE)


#PM2.5 removal
fig_pm <- aq_gen10_yr %>% 
  # mutate(top10_ba = case_when(genus_name %in% top_10_gen_ba ~ genus_name,
  #                             .default = "other")) %>% 
  ggplot(aes(x = year, y = pm2_5_removed_kg_yr, fill = genus_nametop10)) + 
 # geom_area() + 
  geom_bar(position="stack", stat = "identity")+
  theme_few() + ylab("PM2.5 removed (kg/yr)") + scale_fill_discrete(name = "") +  
  theme(legend.position="none")+
  geom_point(data = ts_data, aes( x= year, y = pm_kg_yr),  inherit.aes = FALSE) + 
  geom_line(data = ts_data, aes( x= year, y = pm_kg_yr),  inherit.aes = FALSE)


#get a shared legend
#fig to get legend from
fig_leg <- aq_gen10_yr %>% 
  ggplot(aes(x = census, y = co_removed_kg_yr, fill = genus_nametop10)) + 
  #geom_area(alpha = 0.2) + 
  geom_bar(position="stack", stat = "identity")+
  theme_few() + ylab("CO removed (kg/yr)") + scale_fill_discrete(name = "tree genera") +
  guides(fill=guide_legend(ncol=2)) +  theme(legend.text = element_text(face="italic")) 
legend <- get_legend(fig_leg)


#combine AQ figs for fig 4
plot_grid(fig_o3, fig_pm, fig_co, fig_so2, fig_no2, legend, ncol = 2,
         labels = c("A","B","C","D","E", ""))


### Fig. 6: leaf area and hydrology ##############################################################
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

#get the non-census years
ts_data <- read_csv("ithacatreesbenefits241217.csv") %>% clean_names() %>% 
  mutate(h20_intercepted_m3_yr = water_intercepted_gal_yr / 264.172,
         avoided_runoff_m3_yr  = avoided_runoff_gal_yr / 264.172)

names(ts_data)

#runoff avoided
fig_runoff <- hy_gen10_yr %>% 
  ggplot(aes(x = census, y = avoided_runoff_m3 , fill = genus_nametop10)) + 
  #geom_area(alpha = 0.2) + 
  geom_bar(position="stack", stat = "identity")+ scale_y_continuous(label=comma) +
  theme_few() + ylab(expression(paste(runoff~avoided~"(",m^3~"/yr)"))) + scale_fill_discrete(name = "") +  
  theme(legend.position="none")  + xlab("year")+
  geom_point(data = ts_data, aes( x= year, y = avoided_runoff_m3_yr),  inherit.aes = FALSE) + 
  geom_line(data = ts_data, aes( x= year, y = avoided_runoff_m3_yr),  inherit.aes = FALSE)


#water intercepted
fig_h20intercept <- hy_gen10_yr %>% 
  ggplot(aes(x = census, y = h20_intercepted_m3 , fill = genus_nametop10)) + 
  #geom_area(alpha = 0.2) + 
  geom_bar(position="stack", stat = "identity")+ scale_y_continuous(label=comma) +xlab("year")+
  theme_few() + ylab(expression(paste(water~intercepted~"(",m^3~"/yr)"))) + scale_fill_discrete(name = "") +  
  theme(legend.position="none")+
  geom_point(data = ts_data, aes( x= year, y = h20_intercepted_m3_yr),  inherit.aes = FALSE) + 
  geom_line(data = ts_data, aes( x= year, y = h20_intercepted_m3_yr),  inherit.aes = FALSE)

#leaf area
fig_leafarea <- hy_gen10_yr %>% 
  ggplot(aes(x = census, y = leaf_area_m2 , fill = genus_nametop10)) + 
   geom_bar(position="stack", stat = "identity")+ scale_y_continuous(label=comma) + xlab("year")+
  theme_few() + ylab(expression(paste(leaf~area~"(",m^2,")"))) + scale_fill_discrete(name = "") +  
  theme(legend.position="none")

#get a shared legend
#fig to get legend from
fig_leg <- hy_gen10_yr %>% 
  ggplot(aes(x = census, y = h20_intercepted_m3, fill = genus_nametop10)) + 
  geom_bar(position="stack", stat = "identity")+
  scale_fill_discrete(name = "tree genera") +
  guides(fill=guide_legend(ncol=1)) +  theme(legend.text = element_text(face="italic")) 
legend <- get_legend(fig_leg)


#combine leaf area and hydro figs
plot_grid(fig_leafarea, fig_runoff, fig_h20intercept, legend, ncol = 4, rel_widths = c(3,3,3,1),
          labels = c("A","B","C"))
