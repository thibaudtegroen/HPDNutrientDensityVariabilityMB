# get fishbase data for IUCN hydrobasins referenced data

# Thibaud te Groen 

# oktober 2022

#code adapted from Tamara Keijzer IUCN_hybas_fishbase.R 
library(rfishbase)
options(FISHBASE_VERSION="21.06")

# Import & cleaning Data -----------------------------------------------------------
#set directory
setwd("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Fishbase")

#**Install and Load packages**

library(plotly)
library(tmap)
library(magrittr)
library(purrr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(dplyr)
library(foreach)
library(rfishbase)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)

asian_food_balance_names <- unique (read.csv("fishspecies_asian_food_balance_names.csv", header = FALSE, sep = ','))
colnames(asian_food_balance_names)[1]  <- "species"
asian_food_balance <- read.csv("fishspecies_asian_food_balance.csv")
nutr <- read.csv("fish_protein_github.csv")

#**Validate Dataset with Fishbase Names**
#*Validate & Synonyms*

# Validate Asian Food Balance 
fb_names=array()

your_list=unique(asian_food_balance_names$species) 

for(i in 1:length(your_list)) {
  
  fb_names[i]=as.character(validate_names(your_list[i])[1])
  
}
asian_match_species=data.frame(species=your_list,fb_names=fb_names)

asian_match_species=data.frame(binomial=your_list,fb_names=fb_names)
write.csv(asian_match_species,'C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Fishbase/asian_fishbase_names.csv', 
          row.names = F)

#**Count matching species**
#Nutr
sum(is.na(match_nutr$fb_name)) #count non matched species with fish base #430
sum(!is.na(unique(match_nutr$fb_name)))#exclude non matched species #left = 5474

#
#**Save matched species dataset as csv**
#write matched species between Species Nutrient Tool and species in Fish Base database to csv
write.csv(match_species,'C:/Industrial Ecology/Jaar 3/Masterscriptie/
          Databases/Fishbase/match_nutr_1.csv', row.names = F)

