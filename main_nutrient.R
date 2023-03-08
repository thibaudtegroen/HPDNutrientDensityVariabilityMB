# Get nutrient data, calculate and map Coefficient Variation  over LMB

# Thibaud te Groen 

# oktober 2022

#code adapted from Tamara Keijzer IUCN_hybas_fishbase.R 
#Based on validate_fishbase_names.R
library(rfishbase)
options(FISHBASE_VERSION="21.06")

# Import & cleaning Data -----------------------------------------------------------
#set directory

setwd("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Fishbase")

#**Install and Load packages**
#install.packages('tidyverse')
#library(tidyverse)
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

source("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/R-script/functions_thibaud_te_groen_thesis_1.R")
#source("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/R-script/validate_fishbase_names.R")

#** load species data **
#Global Nutrient Values from Fishbase Nutrient Tool
nutr <- read.csv("fish_protein_github.csv")

#Tamara validated IUCN fishbase species names from the Mekong
tam_species <- read.csv("Mekong_fbnames_IUCNhybas.csv")

#Fishbase matched species names with Fishbase Nutrient Tool
match_nutr <- read.csv("match_nutr.csv")

# Tamara dataset damming
mekong_ID <- read.csv("mekong_DI_filtered.csv") 

#import hydro(sub)basins from hydrobasin
hb <-read_sf('C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Fishbase/hydrobasin_level_12/hybas_lake_as_lev12_v1c.shp')

sp_nutr <- data_cleaning(match_nutr, nutr, tam_species, mekong_ID) #create dataframe with fishbase names, species names and micro nutrient values without NA values

#current status CV-values dataframe with geomtery
sp_cur_nutr_cv_NA <- create_cur_cv_hb_df (sp_nutr) %>%
  filter(MAIN_BAS != '4120024890') %>% #excluded matched subbasin which is not in Mekong Basin
  st_as_sf()

#future status CV-values dataframe with geomtery
sp_fut_nutr_cv_NA <- create_fut_cv_hb_df (sp_nutr) %>%
  filter(MAIN_BAS != '4120024890') %>% #excluded matched subbasin which is not in Mekong Basin
  st_as_sf()

#current status CV-values dataframe without geometry
df_sp_fut_nutr_cv_NA <- data.frame(sp_fut_nutr_cv_NA)
#future status CV-values dataframe without geometry
df_sp_cur_nutr_cv_NA <- data.frame(sp_cur_nutr_cv_NA)


ttm()
sf::sf_use_s2(FALSE)

#Main analysis: Cumulative distribution function nutrients freshwater fish species and current and future Total, maximum and minimum MB CV   ----------------------------------------------------------------

#create df with species
sp_nutr_unique <- sp_nutr[!duplicated(sp_nutr[,'fb_name']),] # only unique species 

#exclude species which are not present due to HPD
sp_cur_nutr_unique <- sp_nutr_unique %>% filter(hab_curdams == TRUE) #current with HPD


#cumulative distribution function of nutrient concentrations of freshwater fishes in MB
#calcium
plot_cal <- ggplot(sp_cur_nutr_unique, aes(x = Calcium_mu)) + 
  stat_ecdf(mapping = aes(x = Calcium_mu), 
            #geom = "line", 
            color = 'black',
  ) +
  labs(y = "f(Ca)", x = "Ca concentration (mg/100g)") +
  
  #scale 0-900 mg/100g
  scale_x_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 900, 
                                  by = 100)) 

#iron
plot_iron <- ggplot(sp_cur_nutr_unique, aes(x = Iron_mu)) + 
  stat_ecdf(mapping = aes(), #geom = "line"
            ) +
  labs(y = "f(Fe)", x = "Fe concentration (mg/100g)") +
  scale_x_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 10, 
                                  by = 1))

#protein
plot_protein <- ggplot(sp_cur_nutr_unique, aes(x = Protein_mu)) + 
  stat_ecdf(mapping = aes(), #geom = "line"
            ) +
  labs(y = "f(PRO)", x = "PRO concentration (g/100g)")+
  scale_x_continuous(expand = expansion (0),
                     breaks = seq(from=15, 
                                  to = 20, 
                                  by = 1))

#selenium
plot_sel <- ggplot(sp_cur_nutr_unique, aes(x = Selenium_mu)) + 
  stat_ecdf(mapping = aes(), #geom = "line"
            ) +
  labs(y = "f(Se)", x = "Se concentration (μg/100g)")+
  scale_x_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 400, 
                                  by = 50))
#selenium
plot_vit_A <- ggplot(sp_cur_nutr_unique, aes(x = Vitamin_A_mu )) + 
  stat_ecdf(mapping = aes(), #geom = "line"
            ) +
  labs(y = "f(VA)", x = "VA concentration (μg/100g)") +
  scale_x_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 600, 
                                  by = 50))

#zinc
plot_zinc <- ggplot(sp_cur_nutr_unique, aes(x = Zinc_mu)) + 
  stat_ecdf(mapping = aes(), #geom = "line"
            ) +
  labs(y = "f(Zn)", x = "Zn concentration (μg/100g)")+
  scale_x_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 5, 
                                by = 1))

#arranged ecdf plot
ecdf_plot = ggarrange(plot_cal, plot_iron, plot_protein, plot_sel, plot_vit_A, plot_zinc,
                      labels = c("A","B","C","D","E","F"),
                      ncol = 3, nrow =3)
#save
ggsave(filename = "ecdf_plot_Ca_Fe_PRO_Se_VA_Zn.png", plot = ecdf_plot,
       path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
       device = png, width = 10, height = 7, dpi = 300)


#current impact HPD minimum and maximum (range) in CV values in the Mekong Basin

#current status df without geometry
df_sp_cur_nutr_cv_NA <- data.frame(sp_cur_nutr_cv_NA)

#future status df without geometry
df_sp_fut_nutr_cv_NA <- data.frame(sp_fut_nutr_cv_NA)

#min max df of subbasins' nutrient CVs and mean
mekong_cur_min_max_mean_median_cv <- data.frame(
  nutrient = c("Ca",
               "Fe", 
               "PRO",
               "Se", 
               "VA", 
               "Zn"),
  cu_min = c(min(df_sp_cur_nutr_cv_NA$mean_Calcium),
             min(df_sp_cur_nutr_cv_NA$mean_Iron),
             min(df_sp_cur_nutr_cv_NA$mean_Protein),
             min(df_sp_cur_nutr_cv_NA$mean_Selenium),
             min(df_sp_cur_nutr_cv_NA$mean_Vitamin_A),
             min(df_sp_cur_nutr_cv_NA$mean_Zinc)),
  cu_max = c(max(df_sp_cur_nutr_cv_NA$mean_Calcium),
             max(df_sp_cur_nutr_cv_NA$mean_Iron),
             max(df_sp_cur_nutr_cv_NA$mean_Protein),
             max(df_sp_cur_nutr_cv_NA$mean_Selenium),
             max(df_sp_cur_nutr_cv_NA$mean_Vitamin_A),
             max(df_sp_cur_nutr_cv_NA$mean_Zinc)),
  
  cu_mean_CV = c(mean(df_sp_cur_nutr_cv_NA$CV_Calcium, na.rm=TRUE),
                 mean(df_sp_cur_nutr_cv_NA$CV_Iron, na.rm=TRUE),
                 mean(df_sp_cur_nutr_cv_NA$CV_Protein, na.rm=TRUE),
                 mean(df_sp_cur_nutr_cv_NA$CV_Selenium, na.rm=TRUE), 
                 mean(df_sp_cur_nutr_cv_NA$CV_Vitamin_A, na.rm=TRUE),
                 mean(df_sp_cur_nutr_cv_NA$CV_Zinc, na.rm=TRUE)),
                 
                 
  cu_med_CV = c(median(df_sp_cur_nutr_cv_NA$CV_Calcium, na.rm=TRUE),
                median(df_sp_cur_nutr_cv_NA$CV_Iron, na.rm=TRUE),
                median(df_sp_cur_nutr_cv_NA$CV_Protein, na.rm=TRUE),
                median(df_sp_cur_nutr_cv_NA$CV_Selenium, na.rm=TRUE),
                median(df_sp_cur_nutr_cv_NA$CV_Vitamin_A, na.rm=TRUE),
                median(df_sp_cur_nutr_cv_NA$CV_Zinc, na.rm=TRUE)),
  
  cu_min_CV = c(min(df_sp_cur_nutr_cv_NA$CV_Calcium, na.rm=TRUE),
                min(df_sp_cur_nutr_cv_NA$CV_Iron, na.rm=TRUE),
                min(df_sp_cur_nutr_cv_NA$CV_Selenium,na.rm=TRUE),
                min(df_sp_cur_nutr_cv_NA$CV_Protein, na.rm=TRUE),
                min(df_sp_cur_nutr_cv_NA$CV_Vitamin_A, na.rm=TRUE),
                min(df_sp_cur_nutr_cv_NA$CV_Zinc, na.rm=TRUE)),
  
  cu_max_CV = c(max(df_sp_cur_nutr_cv_NA$CV_Calcium, na.rm=TRUE),
                max(df_sp_cur_nutr_cv_NA$CV_Iron, na.rm=TRUE),
                max(df_sp_cur_nutr_cv_NA$CV_Protein, na.rm=TRUE),
                max(df_sp_cur_nutr_cv_NA$CV_Selenium,na.rm=TRUE),
                max(df_sp_cur_nutr_cv_NA$CV_Vitamin_A, na.rm=TRUE),
                max(df_sp_cur_nutr_cv_NA$CV_Zinc, na.rm=TRUE))
)


cu_ranges_plot <- mekong_cur_min_max_mean_median_cv %>%
  ggplot(aes(x = nutrient), y = cu_med_CV) +
  geom_linerange(aes(ymin = cu_min_CV, ymax = cu_max_CV, x = nutrient),
                 size = 4, alpha = 0.25) +
  geom_point(aes(y = cu_min_CV), size = 3, color = 'black') +
  geom_point(aes(y = cu_max_CV), size = 3, color = 'black') +
  geom_point(aes(y = cu_med_CV), size = 3, color = 'red') +
  coord_flip() +
  scale_y_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 150, 
                                  by = 10),
                     limits = c(0,140)) +
  ylab("Coefficient of variation (%)") +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold",
                              hjust = 0.5,
                              margin = margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    #axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.y = element_blank(),
    #axis.ticks.x = element_blank(),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA))


ggsave(filename = "range_cu_cv_plot_Ca_Fe_PRO_Se_VA_Zn.png", plot = cu_ranges_plot,
       path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
       device = png, width = 12, height = 8, dpi = 300)



#Main analysis: Results of spatial differences in current status of CV values across subbasins in the Mekong Basin ----------------------------------------------------------------

#Nutrient CV spatial distribution in current status (Figures are indicated).
#all ranges are from 0 % to 140%

#Figure 4A: Calcium (Ca) CV current distribution
plt_cur_shape_cv_cal <- tm_shape(sp_cur_nutr_cv_NA) + 
  tm_polygons("CV_Calcium", 
              style="cont", 
              breaks = seq(0, 140, 10), 
              title="",
              palette = "Reds",
              #n = 10, 
              textNA = "\u2264 1 species",
              colorNA = "grey",              
              border.alpha = 0.05) +
  tm_layout(legend.show = F,
            frame = FALSE,
            main.title = "A",
            main.title.position = c('left', 'top'),
            main.title.size = 1,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 10, 90, 140), x, "")}),
            legend.height = 1) 
#save
tmap_save(tm = plt_cur_shape_cv_cal, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/cur_cv_Ca.png", device = png)

#Figure 4B: Iron (Fe) CV current
plt_cur_shape_cv_iro <- tm_shape(sp_cur_nutr_cv_NA) + 
  tm_polygons("CV_Iron", 
              style="cont", 
              breaks = seq(0, 140, 10),
              title="",
              palette = "Reds",
              #n = 10,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  tm_layout(legend.show = F,
            frame = FALSE,
            main.title = "B",
            main.title.position = c('left', 'top'),
            main.title.size = 1,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 10, 90, 140), x, "")}),
            legend.height = 1) 
#save
tmap_save(tm = plt_cur_shape_cv_iro, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/cur_cv_Fe.png", device = png)

#Figure 4C: Protein CV current
plt_cur_shape_cv_prot <- tm_shape(sp_cur_nutr_cv_NA) + 
  tm_polygons("CV_Protein", 
              style="cont", 
              breaks = seq(0, 140, 10),
              title = "",
              #n = 10,
              palette = "Reds",
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  tm_layout(legend.show = F,
            #legend.outside = TRUE,
            #legend.outside.position = 'right',
            #legend.outside.size = 0.2,
            #legend.title.size = 0.1,
            #legend.width = 
            #legend.height =
            frame = FALSE,
            main.title = "C",
            main.title.position = c('left', 'top'),
            main.title.size = 1,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 10, 90, 140), x, "")}),
            legend.height = 1)
#save
tmap_save(tm = plt_cur_shape_cv_prot, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/cur_cv_PRO.png", device = png)

#Figure 4D: Selenium CV current
plt_cur_shape_cv_sel <- tm_shape(sp_cur_nutr_cv_NA) + 
  tm_polygons("CV_Selenium", 
              style="cont", 
              breaks = seq(0, 140, 10),
              title="current Selenium_CV (%)",
              palette = "Reds",
              #n= 10,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  
  tm_layout(legend.show = F,
            frame = FALSE,
            main.title = "D",
            main.title.position = c('left', 'top'),
            main.title.size = 1, 
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 10, 90, 140), x, "")}),
            legend.height = 1) 
#save
tmap_save(tm = plt_cur_shape_cv_sel, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/cur_cv_Se.png", device = png)

#Figure 4E: Vitamin A CV current PRESENTATION
plt_cur_shape_cv_vit_A <- tm_shape(sp_cur_nutr_cv_NA) + 
  tm_polygons("CV_Vitamin_A", 
              style="cont", 
              breaks = seq(0, 140, 10),
              title="",
              palette = "Reds",
              #n = 10,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  tm_layout(legend.show = F,
            frame = FALSE,
            main.title = "E",
            main.title.position = c('left', 'top'),
            main.title.size = 1, 
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 10, 90, 140), x, "")}),
            legend.height = 1) 

#save 
tmap_save(tm = plt_cur_shape_cv_vit_A, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/cur_cv_VA.png", device = png)

#Figure 4F: Zinc CV current
plt_cur_shape_cv_zin <- tm_shape(sp_cur_nutr_cv_NA) + 
  tm_polygons("CV_Zinc", 
              style="cont", 
              breaks = seq(0, 140, 10),
              title="",
              palette = "Reds",
              #n = 10,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  tm_layout(legend.show = F,
            frame = FALSE,
            main.title = "F",
            main.title.position = c('left', 'top'),
            main.title.size = 1, 
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('left', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 10, 90, 140), x, "")}),
            legend.height = 1) 
#save
tmap_save(tm = plt_cur_shape_cv_zin, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/cur_cv_Zn.png", device = png)


#general legend for arranged plot
legend_spatial_cv_cur_map <- tm_shape(sp_cur_nutr_cv_NA) +
  tm_polygons("CV_Zinc", 
              style="cont", 
              breaks = seq(0, 140, 10),
              title= "",
              palette = "Reds",
              #n = 10,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  tm_layout(legend.only = T, 
            scale=3, 
            asp=0,
            main.title= "Coefficient of variation (%)", 
            main.title.position = c('left', 'top'),
            main.title.size = 3,
            legend.width = 15,
            legend.text.size = 1,
            legend.position  = c('left', 'top'),
            legend.height = 3,
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 10, 90, 140), x, "")}))

#arranged plot
spatial_cur_cv_plot_Ca_Fe_PRO_Se_VA_Zn <- tmap_arrange(plt_cur_shape_cv_cal, plt_cur_shape_cv_iro, plt_cur_shape_cv_prot, legend_spatial_cv_cur_map,
                                          plt_cur_shape_cv_sel, plt_cur_shape_cv_vit_A, plt_cur_shape_cv_zin, 
                                          ncol = 4, nrow = 2) 
#save arranged plot 
tmap_save(tm = spatial_cur_cv_plot_Ca_Fe_PRO_Se_VA_Zn, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/cur_arrange_all.png", 
          device = png)
  
#Main analysis: Results of spatial differences in future status of CV values across subbasins in the Mekong Basin ----------------------------------------------------------------


#This code describes the difference between current and future status.

#future status (difference future - current)
df_difference_cv_mean <- data.frame(HYBAS_ID = df_sp_fut_nutr_cv_NA$HYBAS_ID,
                                    diff_cv_se = df_sp_fut_nutr_cv_NA$CV_Selenium - sp_cur_nutr_cv_NA$CV_Selenium,
                                    diff_cv_iro = df_sp_fut_nutr_cv_NA$CV_Iron - sp_cur_nutr_cv_NA$CV_Iron,
                                    diff_cv_VA = df_sp_fut_nutr_cv_NA$CV_Vitamin_A - sp_cur_nutr_cv_NA$CV_Vitamin_A,
                                    diff_cv_Ca = df_sp_fut_nutr_cv_NA$CV_Calcium - sp_cur_nutr_cv_NA$CV_Calcium,
                                    diff_cv_Zn = df_sp_fut_nutr_cv_NA$CV_Zinc - sp_cur_nutr_cv_NA$CV_Zinc,
                                    diff_cv_PRO = df_sp_fut_nutr_cv_NA$CV_Protein - sp_cur_nutr_cv_NA$CV_Protein,
                                    diff_mean_se = df_sp_fut_nutr_cv_NA$mean_Selenium - sp_cur_nutr_cv_NA$mean_Selenium,
                                    diff_mean_iro = df_sp_fut_nutr_cv_NA$mean_Iron - sp_cur_nutr_cv_NA$mean_Iron,
                                    diff_mean_VA = df_sp_fut_nutr_cv_NA$mean_Vitamin_A - sp_cur_nutr_cv_NA$mean_Vitamin_A,
                                    diff_mean_Ca = df_sp_fut_nutr_cv_NA$mean_Calcium - sp_cur_nutr_cv_NA$mean_Calcium,
                                    diff_mean_Zn = df_sp_fut_nutr_cv_NA$mean_Zinc - sp_cur_nutr_cv_NA$mean_Zinc,
                                    diff_mean_PRO = df_sp_fut_nutr_cv_NA$mean_Protein - sp_cur_nutr_cv_NA$mean_Protein)

#future impact HPD on Maximum, minimum CV with geometry
#create sf dataframe for difference between future and current nutrient CV values in subbasins
diff_dam_cv <- left_join(df_difference_cv_mean, hb %>% 
                           dplyr::select(HYBAS_ID,MAIN_BAS, DIST_MAIN, SUB_AREA, geometry)) %>%
  
  st_as_sf()

#future impact HPD on Maximum, minumim mean with geometry 
#create sf dataframe for difference between future and current mean nutrient values in subbasins
diff_dam_mean <- left_join(df_difference_cv_mean, hb %>% 
                             dplyr::select(HYBAS_ID,MAIN_BAS, DIST_MAIN, SUB_AREA, geometry)) %>%
  
  st_as_sf()

#Overall Difference dataframe min, max, values and mean, min and max CV subbasins
#df with min, max of mean and min, max, and mean of nutrient CV values 
mekong_dif_mean_min_max_mean_cv <- data.frame(
  Selenium = c(cu_min = min(df_difference_cv_mean$diff_mean_se),
               cu_max = max(df_difference_cv_mean$diff_mean_se),
               cu_mean_CV = mean(df_difference_cv_mean$diff_cv_se, na.rm=TRUE),
               cu_min_CV = min(df_difference_cv_mean$diff_cv_se,na.rm=TRUE),
               cu_max_CV = max(df_difference_cv_mean$diff_cv_se,na.rm=TRUE)),
  Zinc = c(cu_min = min(df_difference_cv_mean$diff_mean_Zn ),
           cu_max = max(df_difference_cv_mean$diff_mean_Zn ),
           cu_mean_CV = mean(df_difference_cv_mean$diff_cv_Zn, na.rm=TRUE),
           cu_min_CV = min(df_difference_cv_mean$diff_cv_Zn, na.rm=TRUE),
           cu_max_CV = max(df_difference_cv_mean$diff_cv_Zn, na.rm=TRUE)),
  
  Protein = c(cu_min = min(df_difference_cv_mean$diff_mean_PRO),
              cu_max = max(df_difference_cv_mean$diff_mean_PRO),
              cu_mean_CV = mean(df_difference_cv_mean$diff_cv_PRO, na.rm=TRUE),
              cu_min_CV = min(df_difference_cv_mean$diff_cv_PRO, na.rm=TRUE),
              cu_max_CV = max(df_difference_cv_mean$diff_cv_PRO, na.rm=TRUE)),
  
  Calcium = c(cu_min = min(df_difference_cv_mean$diff_mean_Ca),
              cu_max = max(df_difference_cv_mean$diff_mean_Ca),
              cu_mean_CV = mean(df_difference_cv_mean$diff_cv_Ca, na.rm=TRUE),
              cu_min_CV = min(df_difference_cv_mean$diff_cv_Ca, na.rm=TRUE),
              cu_max_CV = max(df_difference_cv_mean$diff_cv_Ca, na.rm=TRUE)),
  
  Iron = c(cu_min = min(df_difference_cv_mean$diff_mean_iro),
           cu_max = max(df_difference_cv_mean$diff_mean_iro),
           cu_mean_CV = mean(df_difference_cv_mean$diff_cv_iro, na.rm=TRUE),
           cu_min_CV = min(df_difference_cv_mean$diff_cv_iro, na.rm=TRUE),
           cu_max_CV = max(df_difference_cv_mean$diff_cv_iro, na.rm=TRUE)),
  
  Vitamin_A = c(cu_min = min(df_difference_cv_mean$diff_mean_VA),
                cu_max = max(df_difference_cv_mean$diff_mean_VA),
                cu_mean_CV = mean(df_difference_cv_mean$diff_cv_VA, na.rm=TRUE),
                cu_min_CV = min(df_difference_cv_mean$diff_cv_VA, na.rm=TRUE),
                cu_max_CV = max(df_difference_cv_mean$diff_cv_VA, na.rm=TRUE))
)

mekong_diff_min_max_mean_median_cv <- data.frame(
  nutrient = c("Ca",
               "Fe", 
               "PRO",
               "Se", 
               "VA", 
               "Zn"),
  fu_min_mean = c(min(df_difference_cv_mean$diff_mean_Ca),
             min(df_difference_cv_mean$diff_mean_iro),
             min(df_difference_cv_mean$diff_mean_PRO),
             min(df_difference_cv_mean$diff_mean_se),
             min(df_difference_cv_mean$diff_mean_VA),
             min(df_difference_cv_mean$diff_mean_VA)),
  
  fu_max_mean = c(max(df_difference_cv_mean$diff_mean_Ca),
             max(df_difference_cv_mean$diff_mean_iro),
             max(df_difference_cv_mean$diff_mean_PRO),
             max(df_difference_cv_mean$diff_mean_se),
             max(df_difference_cv_mean$diff_mean_VA),
             max(df_difference_cv_mean$diff_mean_Zn)),
  
  fu_mean_CV = c(mean(df_difference_cv_mean$diff_cv_Ca, na.rm=TRUE),
                 mean(df_difference_cv_mean$diff_cv_iro, na.rm=TRUE),
                 mean(df_difference_cv_mean$diff_cv_PRO, na.rm=TRUE),
                 mean(df_difference_cv_mean$diff_cv_se, na.rm=TRUE), 
                 mean(df_difference_cv_mean$diff_cv_VA, na.rm=TRUE),
                 mean(df_difference_cv_mean$diff_cv_Zn, na.rm=TRUE)),
  
  
  fu_med_CV = c(median(df_difference_cv_mean$diff_cv_Ca, na.rm=TRUE),
                median(df_difference_cv_mean$diff_cv_iro, na.rm=TRUE),
                median(df_difference_cv_mean$diff_cv_PRO, na.rm=TRUE),
                median(df_difference_cv_mean$diff_cv_se, na.rm=TRUE), 
                median(df_difference_cv_mean$diff_cv_VA, na.rm=TRUE),
                median(df_difference_cv_mean$diff_cv_Zn, na.rm=TRUE)),
  
  fu_min_CV = c(min(df_difference_cv_mean$diff_cv_Ca, na.rm=TRUE),
                min(df_difference_cv_mean$diff_cv_iro, na.rm=TRUE),
                min(df_difference_cv_mean$diff_cv_PRO, na.rm=TRUE),
                min(df_difference_cv_mean$diff_cv_se,na.rm=TRUE),
                min(df_difference_cv_mean$diff_cv_VA, na.rm=TRUE),
                min(df_difference_cv_mean$diff_cv_Zn, na.rm=TRUE)),
  
  fu_max_CV = c(max(df_difference_cv_mean$diff_cv_Ca, na.rm=TRUE),
                max(df_difference_cv_mean$diff_cv_iro, na.rm=TRUE),
                max(df_difference_cv_mean$diff_cv_PRO, na.rm=TRUE),
                max(df_difference_cv_mean$diff_cv_se,na.rm=TRUE),
                max(df_difference_cv_mean$diff_cv_VA, na.rm=TRUE),
                max(df_difference_cv_mean$diff_cv_Zn, na.rm=TRUE))
)

fu_ranges_plot <- mekong_diff_min_max_mean_median_cv %>%
  ggplot(aes(x = nutrient), y = fu_med_CV) +
  geom_linerange(aes(ymin = fu_min_CV, ymax = fu_max_CV, x = nutrient),
                 size = 4, alpha = 0.25) +
  geom_point(aes(y = fu_min_CV), size = 3, color = "black") +
  geom_point(aes(y = fu_max_CV), size = 3, color = "black") +
  geom_point(aes(y = fu_med_CV), size = 3, color = "red") +
  coord_flip() +
  scale_y_continuous(expand = expansion (0),
                     breaks = seq(from=-60, 
                                  to = 60, 
                                  by = 10),
                     limits = c(-60,60)) +
  ylab("Change in Coefficient of variation (%)") +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold",
                              hjust = 0.5,
                              margin = margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    #axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.y = element_blank(),
    #axis.ticks.x = element_blank(),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA))


ggsave(filename = "range_fut_cv_plot_Ca_Fe_PRO_Se_VA_Zn.png", plot = fu_ranges_plot,
       path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
       device = png, width = 12, height = 8, dpi = 300)


#maps future status: spatial distribution of difference between future and current CV-values of subbasins (Figure 5)
#ranges kept on -50% to 50% for all nutrients

#Figure 5A: diff Ca CV
plt_diff_shape_cv_Ca <- tm_shape(diff_dam_cv) +
  tm_polygons("diff_cv_Ca", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="",
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.show = F,
            main.title = "A",
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)
#save
tmap_save(tm = plt_diff_shape_cv_Ca , filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/diff_cv_Ca.png", device = png)


#Figure 5B: diff Fe CV
plt_diff_shape_cv_iro <- tm_shape(diff_dam_cv) +
  tm_polygons("diff_cv_iro", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="",
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.show = F,
            main.title = "B",
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)
#save
tmap_save(tm = plt_diff_shape_cv_iro, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/diff_Fe.png", device = png)

#Figure 5C: diff protein CV
plt_diff_shape_cv_PRO <- tm_shape(diff_dam_cv) +
  tm_polygons("diff_cv_PRO", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="",
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.show = F,
            main.title = "C",
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)
#save
tmap_save(tm = plt_diff_shape_cv_PRO, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/diff_cv_PRO.png", device = png)

#Figure 5D: Se current and future difference CV
plt_diff_shape_cv_sel <- tm_shape(diff_dam_cv) + #sp_nutr_cv_NA_pop_2
  tm_polygons("diff_cv_se", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="",
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.show = F,
            main.title = "D",
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1) 
#save
tmap_save(tm = plt_diff_shape_cv_sel, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/diff_cv_Se.png", device = png)

#Figure 5E: diff VA CV
plt_diff_shape_cv_VA <- tm_shape(diff_dam_cv) +
  tm_polygons("diff_cv_VA", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="",
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.show = F,
            main.title = "E",
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)
#save
tmap_save(tm = plt_diff_shape_cv_VA, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/diff_VA.png", device = png)


#Figure 5F: diff Zn CV
plt_diff_shape_cv_zn <- tm_shape(diff_dam_cv) +
  tm_polygons("diff_cv_Zn", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="",
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.show = F,
            main.title = "F",
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)

#save
tmap_save(tm = plt_diff_shape_cv_zn, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/diff_cv_zn.png", 
          device = png)


#legend map arranged difference map of CV-values in the subbasins
legend_spatial_cv_fut_map <- tm_shape(diff_dam_cv) +
  tm_polygons("diff_cv_PRO", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="Change in CV (%)",
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.only = T, 
            scale=3, 
            asp=0,
            main.title.position = c('left', 'top'),
            main.title.size = 3,
            legend.width = 15,
            legend.text.size = 0.86,
            legend.position  = c('left', 'top'),
            legend.height = 3
            #legend.format = list(fun = function (x) {ifelse(x %in% c(0, 10, 90, 180), x, "")})
            )


#arrange map
spatial_fut_cv_plot_Ca_Fe_PRO_Se_VA_Zn <- tmap_arrange(plt_diff_shape_cv_Ca, plt_diff_shape_cv_iro, plt_diff_shape_cv_PRO, legend_spatial_cv_fut_map,
                                                       plt_diff_shape_cv_sel, plt_diff_shape_cv_VA, plt_diff_shape_cv_zn, 
                                                       ncol = 4, nrow = 2) 
#save
tmap_save(tm = spatial_fut_cv_plot_Ca_Fe_PRO_Se_VA_Zn, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/fut_arrange_all.png", 
          device = png,
          dpi = 300,
          height = 2500,
          width = 2250
)





#Main analysis: Results of spatial differences in current status of NR1 and NR6 values across subbasins in the Mekong Basin ----------------------------------------------------------------

#ttm()

#current dam influence on individual and cumulative nutrient density

#create nutrient density score dataframes (first per species - than mean for every hydrobasin)
cur_sp_nutr <- sp_nutr[, c("HYBAS_ID", "fb_name", "hab_curdams", "hab_futdams", "Iron_mu",
                         "Selenium_mu", "Zinc_mu", "Vitamin_A_mu", "Protein_mu","Calcium_mu", "Omega_3_mu" )]

cur_dam_nutrient <- cur_sp_nutr %>% filter(hab_curdams == TRUE) #filter species which are extinct in that hydrobasin now (current)

#add RNIs of different nutrients to df
cur_dam_nutrient$RNI_adult_iron <- 13.7 #mg/per day
cur_dam_nutrient$RNI_adult_calcium <- 700 #mg/per day
cur_dam_nutrient$RNI_adult_zinc <- 6.5 #mg/per day
cur_dam_nutrient$RNI_adult_selenium <- 34 #ug/per day
cur_dam_nutrient$RNI_adult_protein <- 48 #mg/per day
cur_dam_nutrient$RNI_adult_Vitamin_A <- 600 #ug/per day

#calculate individual nutrient density for every subbasins
cur_dam_nutrient$cu_NRn_adult_iron <- (cur_dam_nutrient$Iron_mu / cur_dam_nutrient$RNI_adult_iron) *100
cur_dam_nutrient$cu_NRn_adult_calcium <- (cur_dam_nutrient$Calcium_mu / cur_dam_nutrient$RNI_adult_calcium) *100
cur_dam_nutrient$cu_NRn_adult_zinc <- (cur_dam_nutrient$Zinc_mu / cur_dam_nutrient$RNI_adult_zinc) *100
cur_dam_nutrient$cu_NRn_adult_selenium <- (cur_dam_nutrient$Selenium_mu / cur_dam_nutrient$RNI_adult_selenium) *100 # selenium is in the WHO 34 ug ipv mg (wrongly cited) / need to be capped at 100%
cur_dam_nutrient$cu_NRn_adult_protein <- (cur_dam_nutrient$Protein_mu / cur_dam_nutrient$RNI_adult_protein) *100
cur_dam_nutrient$cu_NRn_adult_Vitamin_A <- (cur_dam_nutrient$Vitamin_A_mu / cur_dam_nutrient$RNI_adult_Vitamin_A) *100


#function capped to cap individual nutrient densities at 100%
capped <- function(old_percentage_value) { 
  if(old_percentage_value > 100) new_value <- 100
  if (old_percentage_value < 100)  new_value <- old_percentage_value
  return(new_value)
}

#apply capped function to individual nutrients
cur_dam_nutrient$cu_NRn_adult_iron <- sapply(cur_dam_nutrient$cu_NRn_adult_iron,capped)
cur_dam_nutrient$cu_NRn_adult_calcium <- sapply(cur_dam_nutrient$cu_NRn_adult_calcium,capped)
cur_dam_nutrient$cu_NRn_adult_zinc <- sapply(cur_dam_nutrient$cu_NRn_adult_zinc,capped)
cur_dam_nutrient$cu_NRn_adult_selenium <- sapply(cur_dam_nutrient$cu_NRn_adult_selenium,capped)
cur_dam_nutrient$cu_NRn_adult_protein <- sapply(cur_dam_nutrient$cu_NRn_adult_protein,capped)
cur_dam_nutrient$cu_NRn_adult_Vitamin_A <- sapply(cur_dam_nutrient$cu_NRn_adult_Vitamin_A,capped)

#take the mean of nutrient densities of current presence of species in subbasin to get current nutrient density value for every subbasin
#and add cumulative nutrient density
cur_dam_adult_mean_hb_join <- cur_dam_nutrient %>%
  group_by(HYBAS_ID)%>%
  summarise(
    cu_mean_NRn_adult_iron = mean(cu_NRn_adult_iron), #Fe density
    cu_mean_NRn_adult_calcium = mean(cu_NRn_adult_calcium), #Ca density
    cu_mean_NRn_adult_zinc = mean(cu_NRn_adult_zinc), #Zn density
    cu_mean_NRn_adult_selenium = mean(cu_NRn_adult_selenium), #Se density
    cu_mean_NRn_adult_protein = mean(cu_NRn_adult_protein), #PRO density
    cu_mean_NRn_adult_Vitamin_A = mean(cu_NRn_adult_Vitamin_A), #VA density
    cu_mean_NRn_adult_tot =  (cu_mean_NRn_adult_iron + cu_mean_NRn_adult_calcium + cu_mean_NRn_adult_zinc +
                                cu_mean_NRn_adult_selenium + cu_mean_NRn_adult_protein + cu_mean_NRn_adult_Vitamin_A)) #cumulative density


#create current sf dataframe based on HYBAS ID and add Distance to outlets and surface of the subbasin
cur_dam_adult_mean_hb <- left_join(cur_dam_adult_mean_hb_join , hb %>% 
                                     dplyr::select(HYBAS_ID,MAIN_BAS, DIST_MAIN, SUB_AREA, geometry)) %>%
  filter(MAIN_BAS != '4120024890') %>% #exclude this subbasin (not part of MB) due to matching error
  
  st_as_sf() 

#create df (without sf) of current densities 
cur_dam_adult_mean_hb_df <- data.frame(cur_dam_adult_mean_hb)


#current minimum, maximum and median NR1 and NR6
mekong_cu_min_max_mean_median_NR1 <- data.frame(
  nutrient = c("Calcium",
               "Iron", 
               "Protein",
               "Selenium", 
               "Vitamin A", 
               "Zinc"),
  
  cu_med_NR1 = c(median(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_calcium, na.rm=TRUE),
                median(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_iron, na.rm=TRUE),
                median(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_protein, na.rm=TRUE),
                median(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_selenium, na.rm=TRUE), 
                median(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_Vitamin_A, na.rm=TRUE),
                median(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_zinc, na.rm=TRUE)),
                
  
  cu_min_NR1 = c(min(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_calcium, na.rm=TRUE),
                min(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_iron, na.rm=TRUE),
                min(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_protein, na.rm=TRUE),
                min(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_selenium,na.rm=TRUE),
                min(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_Vitamin_A, na.rm=TRUE),
                min(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_zinc, na.rm=TRUE)),
                
  
  cu_max_NR1 = c(max(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_calcium, na.rm=TRUE),
                max(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_iron, na.rm=TRUE),
                max(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_protein, na.rm=TRUE),
                max(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_selenium,na.rm=TRUE),
                max(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_Vitamin_A, na.rm=TRUE),
                max(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_zinc, na.rm=TRUE))
)

#cumulative NR6 min, max, median across subbasins
mekong_cu_min_max_mean_median_NR6 <- data.frame(
  nutrient = "NR6",
  
  cu_med_NR6 = median(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_tot, na.rm=TRUE),
  
  cu_min_NR6 = min(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_tot, na.rm=TRUE),
  
  cu_max_NR6 = max(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_tot, na.rm=TRUE)
)

cu_NR1_ranges_plot <- mekong_cu_min_max_mean_median_NR1 %>%
  ggplot(aes(x = nutrient), y = cu_med_NR1) +
  geom_linerange(aes(ymin = cu_min_NR1, ymax = cu_max_NR1, x = nutrient),
                 size = 4, alpha = 0.25) +
  geom_point(aes(y = cu_min_NR1), size = 3) +
  geom_point(aes(y = cu_max_NR1), size = 3) +
  geom_point(aes(y = cu_med_NR1), size = 3, color = "red") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, .02)),
                     breaks = seq(from=0, 
                                  to = 100, 
                                  by = 5),
                     limits = c(0,100)) +
  ylab(expression("NR"[1]* "(%)" )) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold",
                              hjust = 0.5,
                              margin = margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    #axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.y = element_blank(),
    #axis.ticks.x = element_blank(),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA))

#NR6
cu_NR6_ranges_plot <- mekong_cu_min_max_mean_median_NR6 %>%
  ggplot(aes(x = nutrient), y = cu_med_NR6) +
  geom_linerange(aes(ymin = cu_min_NR6, ymax = cu_max_NR6, x = nutrient),
                 size = 4, alpha = 0.25) +
  geom_point(aes(y = cu_min_NR6), size = 3) +
  geom_point(aes(y = cu_max_NR6), size = 3) +
  geom_point(aes(y = cu_med_NR6), size = 3, color = "red") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, .02)),
                     breaks = seq(from=0, 
                                  to = 400, 
                                  by = 50),
                     limits = c(0,400)) +
  ylab(expression("NR"[6] * "(%)" )) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold",
                              hjust = 0.5,
                              margin = margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    #axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.y = element_blank(),
    #axis.ticks.x = element_blank(),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA))





#save NR1 ranges plot
ggsave(filename = "range_NR1_plot_Ca_Fe_PRO_Se_VA_Zn.png", plot = cu_NR1_ranges_plot,
       path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
       device = png, width = 12, height = 8, dpi = 300)

#save NR6 current ranges plot
ggsave(filename = "range_cu_NR6_plot.png", plot = cu_NR6_ranges_plot,
       path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
       device = png, width = 12, height = 8, dpi = 300)


#disable sf errors due to geometry
sf::sf_use_s2(FALSE)


#Spatial map current dam scenario
#ranges for individual nutrient density 0% - 100%
#ranges for cumulative nutrient density 100% - 400% - since subbasins lack values between 0-100 or 330 - 600%
# figures are indicated 

#Figure 9B: Cumulative Nutrient Density (NR6, %)
plt_cur_density_tot <- tm_shape(cur_dam_adult_mean_hb) + 
  tm_polygons(col = "cu_mean_NRn_adult_tot", 
              style ="cont",
              title= expression("NR"[6] *"(%)"),
              title.size = 2,
              breaks = seq(100, 400, 25),
              palette = "seq",
              border.alpha = 0.05) +
  tm_layout(aes.palette = list(seq = "YlOrRd"),
            legend.show = T,
            legend.outside = T,
            scale = 2,
            title.size = 2,
            frame = FALSE,
            main.title = "B",
            main.title.size = 2,
            main.title.position = c('left', 'top'),
            legend.width = 10,
            legend.text.size = 1.5,
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 100, 200, 300, 400), x, "")}),
            legend.position  = c('right', 'top'),
            legend.height = 1)

#save
tmap_save(tm = plt_cur_density_tot, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/cur_NR_tot.png", 
          device = png, width = 1800, height = 3200, dpi =300)

#Figure 7A: Calcium density
plt_cur_density_Ca <- tm_shape(cur_dam_adult_mean_hb) + 
  tm_polygons(col = "cu_mean_NRn_adult_calcium", 
              style ="cont",
              title="",
              breaks = seq(0, 100, 5),
              palette = "seq",
              border.alpha = 0.05) +
  tm_layout(aes.palette = list(seq = "YlOrRd"),
            legend.show = F,
            frame = FALSE,
            main.title = "A",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            legend.width = 10,
            legend.text.size = 1.5,
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 20, 40, 60, 80, 100), x, "")}),
            legend.position  = c('right', 'top'),
            legend.height = 1)

#save
tmap_save(tm = plt_cur_density_Ca, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/cur_NR_Ca.png", device = png)

#Figure 7B: Fe density
plt_cur_density_iro <- tm_shape(cur_dam_adult_mean_hb) + 
  tm_polygons(col = "cu_mean_NRn_adult_iron", 
              style ="cont",
              title="",
              breaks = seq(0, 100, 5),
              palette = "seq",
              border.alpha = 0.05) +
  tm_layout(aes.palette = list(seq = "YlOrRd"),
            legend.show = F,
            frame = FALSE,
            main.title = "B",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            legend.width = 10,
            legend.text.size = 1.5,
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 20, 40, 60, 80, 100), x, "")}),
            legend.position  = c('right', 'top'),
            legend.height = 1)

tmap_save(tm = plt_cur_density_iro, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/cur_NR_Fe.png", device = png)


#Figure 7C: Protein(PRO) density
plt_cur_density_PRO <- tm_shape(cur_dam_adult_mean_hb) + 
  tm_polygons(col = "cu_mean_NRn_adult_protein", 
              style ="cont",
              title="",
              breaks = seq(0, 100, 5),
              palette = "seq",
              border.alpha = 0.05) +
  tm_layout(aes.palette = list(seq = "YlOrRd"),
            legend.show = F,
            frame = FALSE,
            main.title = "C",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            legend.width = 10,
            legend.text.size = 1.5,
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 20, 40, 60, 80, 100), x, "")}),
            legend.position  = c('right', 'top'),
            legend.height = 1)


tmap_save(tm = plt_cur_density_PRO, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/cur_NR_PRO.png", device = png)




#Figure 7D: Selenium density
plt_cur_density_se <- tm_shape(cur_dam_adult_mean_hb) + 
  tm_polygons(col = "cu_mean_NRn_adult_selenium", 
              style ="cont",
              title="",
              breaks = seq(0, 100, 5),
              palette = "seq",
              border.alpha = 0.05) +
  tm_layout(aes.palette = list(seq = "YlOrRd"),
            legend.show = F,
            frame = FALSE,
            main.title = "D",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            legend.width = 10,
            legend.text.size = 1.5,
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 20, 40, 60, 80, 100), x, "")}),
            legend.position  = c('right', 'top'),
            legend.height = 1)

tmap_save(tm = plt_cur_density_se, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/cur_NR_Se.png", device = png)


#Figure 7E: VA density
plt_cur_density_VA <- tm_shape(cur_dam_adult_mean_hb) + 
  tm_polygons(col = "cu_mean_NRn_adult_Vitamin_A", 
              style ="cont",
              title="",
              breaks = seq(0, 100, 5),
              palette = "seq",
              border.alpha = 0.05) +
  tm_layout(aes.palette = list(seq= "YlOrRd"),
            legend.show = F,
            frame = FALSE,
            main.title = "E",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            legend.width = 10,
            legend.text.size = 1.5,
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 20, 40, 60, 80, 100), x, "")}),
            legend.position  = c('right', 'top'),
            legend.height = 1)

tmap_save(tm = plt_cur_density_VA, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/cur_NR_VA.png", device = png)

#Figure 7F: Zinc density
plt_cur_density_Zn <- tm_shape(cur_dam_adult_mean_hb) + 
  tm_polygons(col = "cu_mean_NRn_adult_zinc", 
              style ="cont",
              title="",
              breaks = seq(0, 100, 5),
              palette = "seq",
              border.alpha = 0.05) +
  tm_layout(aes.palette = list(seq = "YlOrRd"),
            legend.show = F,
            frame = FALSE,
            main.title = "F",
            main.title.position = c('left', 'top'),
            main.title.size = 1,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 20, 40, 60, 80, 100), x, "")}),
            legend.position  = c('right', 'top'),
            legend.height = 1)


tmap_save(tm = plt_cur_density_Zn, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/cur_NR_Zn.png", device = png)

#individual nutrient density legend for arranged spatial map 
legend_cur_density_spatial <- tm_shape(cur_dam_adult_mean_hb) + 
  tm_polygons(col = "cu_mean_NRn_adult_zinc", 
              style ="cont",
              title="Individual Nutrient Density (NR1, %)",
              breaks = seq(0, 100, 5),
              palette = "seq",
              border.alpha = 0.05) +
  tm_layout(aes.palette = list(seq = "YlOrRd"),
            legend.only = T,
            main.title = "Individual Nutrient Density (NR1, %)",
            main.title.position = c('left', 'top'),
            main.title.size = 1,
            legend.width = 10,
            legend.text.size = 1,
            legend.position  = c('left', 'top'),
            legend.height = 2,
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 20, 40, 60, 80, 100), x, "")}))

#arranged map
spatial_cur_density_plot_Ca_Fe_PRO_Se_VA_Zn <- tmap_arrange(plt_cur_density_Ca, plt_cur_density_iro, plt_cur_density_PRO, legend_cur_density_spatial,
                                                       plt_cur_density_se, plt_cur_density_VA, plt_cur_density_Zn, 
                                                       ncol = 4, nrow = 2) 
#save
tmap_save(tm = spatial_cur_density_plot_Ca_Fe_PRO_Se_VA_Zn, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/cur_density_arrange_all.png",
          width = 2000, height = 2000, dpi = 300,
          device = png)



#create df (without sf) of current and future nutrient densities
cur_dam_adult_mean_hb_df <- data.frame(cur_dam_adult_mean_hb)
fut_dam_adult_mean_hb_df <- data.frame(fut_dam_adult_mean_hb)

#assign categories current
cur_dam_adult_mean_hb_df$cu_category <-  cut(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_tot,
                                 breaks = c(0, 100, 200, 300, 400, 500, 600),
                                 labels = c("0%-100%", "100%-200%","200%-300%", "300%-400%", "400%-500%", "500% - 600%"))
 
cur_dam_adult_mean_hb_df$time_period <- "current"

#assign categories future
fut_dam_adult_mean_hb_df$fu_category <- cut(fut_dam_adult_mean_hb_df$fu_mean_NRn_adult_tot,
                                         breaks = c(0, 100, 200, 300, 400, 500, 600),
                                         labels = c("0%-100%", "100%-200%","200%-300%", "300%-400%", "400%-500%", "500% - 600%"))
fut_dam_adult_mean_hb_df$time_period <- "future"

#merge rows for barplots
barplot_1 <- data.frame(HYBAS_ID_2x = c(cur_dam_adult_mean_hb_df[,"HYBAS_ID"], fut_dam_adult_mean_hb_df[,"HYBAS_ID"]), 
                        cu_fu_mean = c(cur_dam_adult_mean_hb_df[,"cu_mean_NRn_adult_tot"], fut_dam_adult_mean_hb_df[,"fu_mean_NRn_adult_tot"]),
                        cu_fu_time = c(cur_dam_adult_mean_hb_df[,"time_period"], fut_dam_adult_mean_hb_df[,"time_period"]),
                        cu_fu_classes = c(cur_dam_adult_mean_hb_df[,"cu_category"], fut_dam_adult_mean_hb_df[,"fu_category"]),
                        cu_fu_area = c(cur_dam_adult_mean_hb_df[,"SUB_AREA"], fut_dam_adult_mean_hb_df[,"SUB_AREA"]),
                        cu_fu_dist = c(cur_dam_adult_mean_hb_df[,"DIST_MAIN"], fut_dam_adult_mean_hb_df[,"DIST_MAIN"])
                        )
barplot_final <- barplot_1 %>% group_by(cu_fu_classes,cu_fu_time) %>%
  summarise(total_count = n(), total_area = sum (cu_fu_area)) 
  
barplot_final$upper = barplot_final$total_area + 500
barplot_final$lower = barplot_final$total_area - 500

#install.packages("ggforce") 
library(ggforce)
#install.packages("ggbreak")
library(ggbreak)
#create barplot with current and future status and based on subbasins area
barplot_plot <- barplot_final %>% 
  ggplot(aes(x=cu_fu_classes,
             y= total_area,
             fill=cu_fu_time,)) + 
  geom_col(width = .5, position = position_dodge(.6),
           color = "black") + 
  scale_fill_manual(values = c("#FAFAFA", "#D4D4D4", "#737373")) +
  #scale_y_log10(limits= c(1, 2e5),
               # expand = expansion(0))+
  scale_y_continuous(
    breaks = seq(1000, 10000, 1000),
    limits= c(0, 10000),
    expand = expansion(0))+
    labs(
    x = "Nutrient Density classes",
    y = bquote("Total Area log"~km^2),
    fill = NULL) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold",
                              hjust = 0.5,
                              margin = margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.x = element_blank(),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend(
      keywidth = 1.2,
      keyheight = 1.2,
      default.unit= "cm"
    )
  )


bar_plot_100_200 <- data.frame (barplot_final %>% 
  dplyr::filter(cu_fu_classes %in% "100%-200%"))

bar_plot_200_300 <- data.frame (barplot_final %>% 
                                  dplyr::filter(cu_fu_classes %in% "200%-300%"))

bar_plot_300_400 <- data.frame (barplot_final %>% 
                                  dplyr::filter(cu_fu_classes %in% "300%-400%"))

bar_plot_100_200_plt <- bar_plot_100_200 %>% 
  ggplot(aes(x=cu_fu_classes,
             y= total_area,
             fill=cu_fu_time)) + 
  geom_col(width = .5, position = position_dodge(.6),
           color = "black") + 
  scale_fill_manual(values = c("#FAFAFA", "#D4D4D4", "#737373")) +
  #scale_y_log10(limits= c(1, 2e5),
  # expand = expansion(0))
  scale_y_continuous(
    breaks = seq(0, 150000, 15000),
    limits= c(0, 150000),
    expand = expansion(0))+
  labs(
    x = "Nutrient Density classes",
    y = bquote("Total Area log"~km^2),
    fill = NULL) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold",
                              hjust = 0.5,
                              margin = margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend(
      keywidth = 1.2,
      keyheight = 1.2,
      default.unit= "cm"
    )
  )

bar_plot_200_300_plt <- bar_plot_200_300 %>% 
  ggplot(aes(x=cu_fu_classes,
             y= total_area,
             fill=cu_fu_time)) + 
  geom_col(width = .5, position = position_dodge(.6),
           color = "black") + 
  scale_fill_manual(values = c("#FAFAFA", "#D4D4D4", "#737373")) +
  #scale_y_log10(limits= c(1, 2e6)) +
  #expand = expansion(0))+
 scale_y_continuous(breaks = seq(0, 650000, 65000),
                    limits= c(0, 650000),
                    expand = expansion(0))+
  labs(
    x = "Nutrient Density classes",
    y = bquote("Total Area log"~km^2),
    fill = NULL) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold",
                              hjust = 0.5,
                              margin = margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend(
      keywidth = 1.2,
      keyheight = 1.2,
      default.unit= "cm"
    )
  )

bar_plot_300_400_plt <- bar_plot_300_400 %>% 
  ggplot(aes(x=cu_fu_classes,
             y= total_area,
             fill=cu_fu_time)) + 
  geom_col(width = .5, position = position_dodge(.6),
           color = "black") + 
  scale_fill_manual(values = c("#FAFAFA", "#D4D4D4", "#737373")) +
  #scale_y_log10(limits= c(1, 2e5)) +
  # expand = expansion(0))+
  scale_y_continuous(
    breaks = seq(0, 7000, 1000),
    limits= c(0, 7000),
    expand = expansion(0))+
  labs(
    x = "Nutrient Density classes",
    y = bquote("Total Area log"~km^2),
    fill = NULL) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold",
                              hjust = 0.5,
                              margin = margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA)
  ) +
  guides(
    fill = guide_legend(
      keywidth = 1.2,
      keyheight = 1.2,
      default.unit= "cm"
    )
  )


bar_plot_nutr_den_classes_area <- ggarrange(bar_plot_100_200_plt + rremove("ylab") + rremove("xlab"), 
                                            bar_plot_200_300_plt + rremove("ylab") + rremove("xlab"),
                                            bar_plot_300_400_plt + rremove("ylab") + rremove("xlab"),
                                            common.legend = TRUE, legend = "right",
                                          labels = c("A","",""),
                                          ncol = 3, nrow =1)
bar_plot_nutr_den_classes_area_an <- annotate_figure(bar_plot_nutr_den_classes_area,
                bottom = text_grob("Nutrient density classes", color = "black", size = 22),
                left = text_grob(expression("Total area (km"^2 * ")"), color = "black", size = 22, rot = 90))



#save barplot
ggsave(filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/barchart_apa.jpeg", 
       plot = bar_plot_nutr_den_classes_area_an, 
       width = 10, 
       height = 6,
       dpi = 300)
#jpeg(file="saving_plot1.jpeg")barplot_1,'C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Fishbase/barplot_1.csv', row.names = F)

#
cur_dam_adult_mean_hb_df %>% 
  count(cu_category == '100%-200%',
        cu_category == '200%-300%',
        cu_category == '300%-400%',
        )





#Main analysis: Results of spatial differences in future status of nutrient density values across subbasins in the Mekong Basin ----------------------------------------------------------------

df_sp_fut_nutr_cv_NA <- data.frame(sp_fut_nutr_cv_NA)
df_sp_cur_nutr_cv_NA <- data.frame(sp_cur_nutr_cv_NA)

df_sp_fut_NR <- data.frame(fut_dam_adult_mean_hb)
df_sp_cur_NR <- data.frame(cur_dam_adult_mean_hb)

#future dam influence
fut_sp_nutr <- sp_nutr[, c("HYBAS_ID", "fb_name", "hab_curdams", "hab_futdams", "Iron_mu",
                           "Selenium_mu", "Zinc_mu", "Vitamin_A_mu", "Protein_mu","Calcium_mu", "Omega_3_mu" )]

fut_dam_nutrient <- fut_sp_nutr %>% filter(hab_futdams == TRUE) #filter species which are extinct in that hydrobasin now (current)

fut_dam_nutrient$RNI_adult_iron <- 13.7 #mg/per day
fut_dam_nutrient$RNI_adult_calcium <- 700 #mg/per day
fut_dam_nutrient$RNI_adult_zinc <- 6.5 #mg/per day
fut_dam_nutrient$RNI_adult_selenium <- 34 #ug/per day
fut_dam_nutrient$RNI_adult_protein <- 48 #mg/per day
fut_dam_nutrient$RNI_adult_Vitamin_A <- 600 #ug/per day


fut_dam_nutrient$fu_NRn_adult_iron <- (fut_dam_nutrient$Iron_mu / fut_dam_nutrient$RNI_adult_iron) *100
fut_dam_nutrient$fu_NRn_adult_calcium <- (fut_dam_nutrient$Calcium_mu / fut_dam_nutrient$RNI_adult_calcium) *100
fut_dam_nutrient$fu_NRn_adult_zinc <- (fut_dam_nutrient$Zinc_mu / fut_dam_nutrient$RNI_adult_zinc) *100
fut_dam_nutrient$fu_NRn_adult_selenium <- (fut_dam_nutrient$Selenium_mu / fut_dam_nutrient$RNI_adult_selenium) *100 # selenium is in the WHO 34 ug ipv mg (wrongly cited) / need to be capped at 100%
fut_dam_nutrient$fu_NRn_adult_protein <- (fut_dam_nutrient$Protein_mu / fut_dam_nutrient$RNI_adult_protein) *100
fut_dam_nutrient$fu_NRn_adult_Vitamin_A <- (fut_dam_nutrient$Vitamin_A_mu / fut_dam_nutrient$RNI_adult_Vitamin_A) *100


fut_dam_nutrient$fu_NRn_adult_iron <- sapply(fut_dam_nutrient$fu_NRn_adult_iron,capped)
fut_dam_nutrient$fu_NRn_adult_calcium <- sapply(fut_dam_nutrient$fu_NRn_adult_calcium,capped)
fut_dam_nutrient$fu_NRn_adult_zinc <- sapply(fut_dam_nutrient$fu_NRn_adult_zinc,capped)
fut_dam_nutrient$fu_NRn_adult_selenium <- sapply(fut_dam_nutrient$fu_NRn_adult_selenium,capped)
fut_dam_nutrient$fu_NRn_adult_protein <- sapply(fut_dam_nutrient$fu_NRn_adult_protein,capped)
fut_dam_nutrient$fu_NRn_adult_Vitamin_A <- sapply(fut_dam_nutrient$fu_NRn_adult_Vitamin_A,capped)



fut_dam_adult_mean_hb_join <- fut_dam_nutrient %>%
  group_by(HYBAS_ID)%>%
  summarise(
    fu_mean_NRn_adult_iron = mean(fu_NRn_adult_iron),
    fu_mean_NRn_adult_calcium = mean(fu_NRn_adult_calcium),
    fu_mean_NRn_adult_zinc = mean(fu_NRn_adult_zinc),
    fu_mean_NRn_adult_selenium = mean(fu_NRn_adult_selenium),
    fu_mean_NRn_adult_protein = mean(fu_NRn_adult_protein),
    fu_mean_NRn_adult_Vitamin_A = mean(fu_NRn_adult_Vitamin_A),
    fu_mean_NRn_adult_tot =  (fu_mean_NRn_adult_iron + fu_mean_NRn_adult_calcium + fu_mean_NRn_adult_zinc +
                                fu_mean_NRn_adult_selenium + fu_mean_NRn_adult_protein + fu_mean_NRn_adult_Vitamin_A))


fut_dam_adult_mean_hb <- left_join(fut_dam_adult_mean_hb_join, hb %>% 
                                     dplyr::select(HYBAS_ID,MAIN_BAS, DIST_MAIN, SUB_AREA, geometry)) %>%
  filter(MAIN_BAS != '4120024890') %>%
  
  st_as_sf() 

df_difference_NR_mean <- data.frame(HYBAS_ID = sp_fut_nutr_cv_NA$HYBAS_ID,
                                    diff_NR_Fe = df_sp_fut_NR$fu_mean_NRn_adult_iron - df_sp_cur_NR$cu_mean_NRn_adult_iron,
                                    diff_NR_Se = df_sp_fut_NR$fu_mean_NRn_adult_selenium - df_sp_cur_NR$cu_mean_NRn_adult_selenium,
                                    diff_NR_VA = df_sp_fut_NR$fu_mean_NRn_adult_Vitamin_A - df_sp_cur_NR$cu_mean_NRn_adult_Vitamin_A,
                                    diff_NR_Ca = df_sp_fut_NR$fu_mean_NRn_adult_calcium - df_sp_cur_NR$cu_mean_NRn_adult_calcium,
                                    diff_NR_Zn = df_sp_fut_NR$fu_mean_NRn_adult_zinc - df_sp_cur_NR$cu_mean_NRn_adult_zinc,
                                    diff_NR_PRO = df_sp_fut_NR$fu_mean_NRn_adult_protein - df_sp_cur_NR$cu_mean_NRn_adult_protein,
                                    diff_NR_tot = df_sp_fut_NR$fu_mean_NRn_adult_tot - df_sp_cur_NR$cu_mean_NRn_adult_tot)
                                    


diff_dam_NR <- left_join(df_difference_NR_mean, hb %>% 
                           dplyr::select(HYBAS_ID,MAIN_BAS, DIST_MAIN, SUB_AREA, geometry)) %>%
  
  st_as_sf()

#create df without sf of future densities
fut_dam_adult_mean_hb_df <- data.frame(fut_dam_adult_mean_hb)

#future minimum, maximum and median NR1 and NR6
mekong_fu_min_max_mean_median_NR1 <- data.frame(
  nutrient = c("Calcium",
               "Iron", 
               "Protein",
               "Selenium", 
               "Vitamin A", 
               "Zinc"),
  
  fu_med_NR1 = c(median(df_difference_NR_mean$diff_NR_Ca, na.rm=TRUE),
                 median(df_difference_NR_mean$diff_NR_Fe, na.rm=TRUE),
                 median(df_difference_NR_mean$diff_NR_PRO, na.rm=TRUE),
                 median(df_difference_NR_mean$diff_NR_Se, na.rm=TRUE), 
                 median(df_difference_NR_mean$diff_NR_VA, na.rm=TRUE),
                 median(df_difference_NR_mean$diff_NR_Zn, na.rm=TRUE)),
  
  
  fu_min_NR1 = c(min(df_difference_NR_mean$diff_NR_Ca, na.rm=TRUE),
                 min(df_difference_NR_mean$diff_NR_Fe, na.rm=TRUE),
                 min(df_difference_NR_mean$diff_NR_PRO, na.rm=TRUE),
                 min(df_difference_NR_mean$diff_NR_Se,na.rm=TRUE),
                 min(df_difference_NR_mean$diff_NR_VA, na.rm=TRUE),
                 min(df_difference_NR_mean$diff_NR_Zn, na.rm=TRUE)),
  
  
  fu_max_NR1 = c(max(df_difference_NR_mean$diff_NR_Ca, na.rm=TRUE),
                 max(df_difference_NR_mean$diff_NR_Fe, na.rm=TRUE),
                 max(df_difference_NR_mean$diff_NR_PRO, na.rm=TRUE),
                 max(df_difference_NR_mean$diff_NR_Se, na.rm=TRUE),
                 max(df_difference_NR_mean$diff_NR_VA, na.rm=TRUE),
                 max(df_difference_NR_mean$diff_NR_Zn, na.rm=TRUE))
)

mekong_cu_fu_min_max_mean_median_NR1 <- data.frame(
  nutrient = c("Ca",
               "Change in Ca",
               "Fe",
               "Change in Fe",
               "PRO",
               "Change in PRO",
               "Se", 
               "Change in Se",
               "VA", 
               "Change in VA",
               "Zn",
               "Change in Zn"),
  
  cu_fu_med_NR1 = c(median(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_calcium, na.rm=TRUE),
                    median(df_difference_NR_mean$diff_NR_Ca, na.rm=TRUE),
                    median(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_iron, na.rm=TRUE),
                 median(df_difference_NR_mean$diff_NR_Fe, na.rm=TRUE),
                 median(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_protein, na.rm=TRUE),
                 median(df_difference_NR_mean$diff_NR_PRO, na.rm=TRUE),
                 median(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_selenium, na.rm=TRUE),
                 median(df_difference_NR_mean$diff_NR_Se, na.rm=TRUE), 
                 median(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_Vitamin_A, na.rm=TRUE),
                 median(df_difference_NR_mean$diff_NR_VA, na.rm=TRUE),
                 median(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_zinc, na.rm=TRUE),
                 median(df_difference_NR_mean$diff_NR_Zn, na.rm=TRUE)),
  
  
  cu_fu_min_NR1 = c(min(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_calcium, na.rm=TRUE),
                    min(df_difference_NR_mean$diff_NR_Ca, na.rm=TRUE),
                    min(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_iron, na.rm=TRUE),
                    min(df_difference_NR_mean$diff_NR_Fe, na.rm=TRUE),
                    min(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_protein, na.rm=TRUE),
                    min(df_difference_NR_mean$diff_NR_PRO, na.rm=TRUE),
                    min(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_selenium,na.rm=TRUE),
                    min(df_difference_NR_mean$diff_NR_Se,na.rm=TRUE),
                    min(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_Vitamin_A, na.rm=TRUE),
                    min(df_difference_NR_mean$diff_NR_VA, na.rm=TRUE),
                    min(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_zinc, na.rm=TRUE),
                    min(df_difference_NR_mean$diff_NR_Zn, na.rm=TRUE)),
  
  
  cu_fu_max_NR1 = c(max(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_calcium, na.rm=TRUE),
                    max(df_difference_NR_mean$diff_NR_Ca, na.rm=TRUE),
                    max(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_iron, na.rm=TRUE),
                    max(df_difference_NR_mean$diff_NR_Fe, na.rm=TRUE),
                    max(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_protein, na.rm=TRUE),
                    max(df_difference_NR_mean$diff_NR_PRO, na.rm=TRUE),
                    max(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_selenium,na.rm=TRUE),
                    max(df_difference_NR_mean$diff_NR_Se, na.rm=TRUE),
                    max(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_Vitamin_A, na.rm=TRUE),
                    max(df_difference_NR_mean$diff_NR_VA, na.rm=TRUE),
                    max(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_zinc, na.rm=TRUE),
                    max(df_difference_NR_mean$diff_NR_Zn, na.rm=TRUE))
)



#cumulative difference NR6 min, max, median across sub basins
mekong_fu_min_max_mean_median_NR6 <- data.frame(
  nutrient = c("NR6"),
  
  fu_med_NR6 = median(df_difference_NR_mean$diff_NR_tot, na.rm=TRUE),
  
  fu_min_NR6 = min(df_difference_NR_mean$diff_NR_tot, na.rm=TRUE),
  
  fu_max_NR6 = max(df_difference_NR_mean$diff_NR_tot, na.rm=TRUE)
)

mekong_cu_fu_min_max_mean_median_NR6 <- data.frame(
  nutrient = c("NR6", 
               "Change in NR6"),
               
  cu_fu_med_NR6 = c(median(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_tot, na.rm=TRUE),
                    median(df_difference_NR_mean$diff_NR_tot, na.rm=TRUE)),
  
  cu_fu_min_NR6 = c(min(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_tot, na.rm=TRUE),
                    min(df_difference_NR_mean$diff_NR_tot, na.rm=TRUE)),
  
  cu_fu_max_NR6 = c(max(cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_tot, na.rm=TRUE),
                    max(df_difference_NR_mean$diff_NR_tot, na.rm=TRUE))
)

#ranges plot current and future NR1

# lock in levels for plot
mekong_cu_fu_min_max_mean_median_NR1$nutrient <- factor(mekong_cu_fu_min_max_mean_median_NR1$nutrient, 
                                                        levels = mekong_cu_fu_min_max_mean_median_NR1$nutrient) 
cu_fu_NR1_ranges_plot <- mekong_cu_fu_min_max_mean_median_NR1 %>%
  ggplot(aes(x = nutrient, 
             y = cu_fu_med_NR1, color = nutrient)) +
  geom_linerange(aes(ymin = cu_fu_min_NR1, ymax = cu_fu_max_NR1, x = nutrient),
                 size = 4, alpha = 0.25) +
  geom_point(aes(y = cu_fu_min_NR1, x = nutrient), size = 3, color = "black") +
  geom_point(aes(y = cu_fu_max_NR1, x = nutrient), size = 3, color = "black") +
  geom_point(aes(y = cu_fu_med_NR1, x = nutrient), size = 3, color = "red") +
  coord_flip() + 
  scale_x_discrete(limits=rev) +
  scale_color_manual(name = "nutrient",
                     values = c("#737373", "#D4D4D4",
                                "#737373", "#D4D4D4",
                                "#737373", "#D4D4D4",
                                "#737373", "#D4D4D4",
                                "#737373", "#D4D4D4",
                                "#737373", "#D4D4D4"))+
  
  scale_y_continuous(expand = expansion(mult = c(0, .02)),
                     breaks = seq(from=-5, 
                                  to = 75, 
                                  by = 5),
                     limits = c(-5,75)) +
  ylab("%") +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold",
                              hjust = 0.5,
                              margin = margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    #axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.y = element_blank(),
    #axis.ticks.x = element_blank(),
    legend.position = "none",
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA))

#difference NR6
fu_NR6_ranges_plot <- mekong_cu_fu_min_max_mean_median_NR6 %>%
  ggplot(aes(x = nutrient), y = cu_fu_med_NR6) +
  geom_linerange(aes(ymin = cu_fu_min_NR6, ymax = cu_fu_max_NR6, x = nutrient),
                 size = 4, alpha = 0.25) +
  geom_point(aes(y = cu_fu_min_NR6), size = 3) +
  geom_point(aes(y = cu_fu_max_NR6), size = 3) +
  geom_point(aes(y = cu_fu_med_NR6), size = 3, color = "red") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, .02)),
                     breaks = seq(from=-5, 
                                  to = 75, 
                                  by = 5),
                     limits = c(-5,75)) +
  ylab(expression(" % " )) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold",
                              hjust = 0.5,
                              margin = margin(b = 15)),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 22, color = "black",
                              face = "bold"),
    axis.text = element_text(size = 22, color = "black"),
    axis.text.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 17),
    #axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.y = element_blank(),
    #axis.ticks.x = element_blank(),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(color = "black"),
    legend.text = element_text(size = 15),
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),
    legend.key = element_rect(color = NA, fill = NA))


#save NR1 ranges plot
ggsave(filename = "range_fu_NR1_plot_Ca_Fe_PRO_Se_VA_Zn.png", plot = fu_NR1_ranges_plot,
       path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
       device = png, width = 12, height = 8, dpi = 300)

#save NR1 ranges plot
ggsave(filename = "range_cu_fu_NR1_plot_Ca_Fe_PRO_Se_VA_Zn.png", plot = cu_fu_NR1_ranges_plot,
       path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
       device = png, width = 12, height = 8, dpi = 300)

#save NR6 ranges plot
ggsave(filename = "range_fu_NR6_plot.png", plot = fu_NR1_ranges_plot,
       path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
       device = png, width = 12, height = 8, dpi = 300)


#Cumulative nutrient density difference
#Figure 9C
plt_diff_shape_NR_tot <- tm_shape(diff_dam_NR) +
  tm_polygons("diff_NR_tot", 
              style="cont", 
              breaks = c(-80,-70, -60, -50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50, 60, 70, 80), 
              title=expression("Change in NR"[6] * "(%)"),
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  
  tm_layout(legend.show = TRUE,
            legend.outside = T,
            scale = 3,
            title.size = 2,
            main.title = "C",
            main.title.position = c('left', 'top'),
            main.title.size = 1.5,
            legend.width = 10,
            legend.text.size = 1.5,
            frame = FALSE,
            legend.outside.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(-80,-70, -60, -50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50, 60, 70, 80), x, "")}),
            legend.height = 1)


tmap_save(tm = plt_diff_shape_NR_tot, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/diff_NR_tot.png", 
          device = png,
          width = 2100, height = 3300, dpi =300)

spatial_fut_density_CU_FU <- tmap_arrange(plt_cur_density_tot, plt_diff_shape_NR_tot, 
                                                            ncol = 2, nrow = 1) 

tmap_save(tm = spatial_fut_density_CU_FU, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/diff_NR_tot.png", 
          device = png)

#Individual Densities
#Figure 8A: Diff NR Calcium
plt_diff_shape_NR_Ca <- tm_shape(diff_dam_NR) +
  tm_polygons("diff_NR_Ca", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="",
              palette = "RdBu",
              n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  
  tm_layout(legend.show = F,
            main.title = "A",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)

tmap_save(tm = plt_diff_shape_NR_Ca, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/diff_NR_Ca.png", device = png)



tmap_save(tm = plt_diff_shape_NR_Fe, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/diff_NR_Fe.png", device = png)

#Figure 8B: Diff NR Fe
plt_diff_shape_NR_Fe <- tm_shape(diff_dam_NR) +
  tm_polygons("diff_NR_Fe", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="",
              palette = "RdBu",
              n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  
  tm_layout(legend.show = F,
            main.title = "B",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)

tmap_save(tm = plt_diff_shape_NR_Fe, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/diff_NR_Fe.png", device = png)

#Figure 8C: Diff NR PRO
plt_diff_shape_NR_PRO <- tm_shape(diff_dam_NR) +
  tm_polygons("diff_NR_PRO", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="",
              palette = "RdBu",
              n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  
  tm_layout(legend.show = F,
            main.title = "C",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)

tmap_save(tm = plt_diff_shape_NR_PRO, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/diff_NR_PRO.png", device = png)

#Figure 8D: Diff NR Se
plt_diff_shape_NR_Se <- tm_shape(diff_dam_NR) +
  tm_polygons("diff_NR_Se", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="",
              palette = "RdBu",
              n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  
  tm_layout(legend.show = F,
            main.title = "D",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)

tmap_save(tm = plt_diff_shape_NR_Se, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/diff_NR_Se.png", device = png)

#Figure 8E: Diff NR VA
plt_diff_shape_NR_VA <- tm_shape(diff_dam_NR) +
  tm_polygons("diff_NR_VA", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="Difference NR VA (%)",
              palette = "RdBu",
              n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  
  tm_layout(legend.show = F,
            main.title = "E",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)

tmap_save(tm = plt_diff_shape_NR_VA, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/diff_NR_VA.png", device = png)


#Figure 8F: Diff NR Zn
plt_diff_shape_NR_Zn <- tm_shape(diff_dam_NR) +
  tm_polygons("diff_NR_Zn", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="",
              palette = "RdBu",
              n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  
  tm_layout(legend.show = F,
            main.title = "F",
            main.title.size = 1,
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            #legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1)

tmap_save(tm = plt_diff_shape_NR_Zn, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/NR/diff_NR_Zn.png", device = png)

#legend map difference map Individual Densities
legend_spatial_NR_fut_map <- tm_shape(diff_dam_NR) +
  tm_polygons("diff_NR_Zn", 
              style="cont", 
              breaks = c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), 
              title="Change in NR1 (%)",
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.05) +
  
  tm_layout(legend.only = T, 
            scale=3, 
            asp=0,
            main.title.position = c('left', 'top'),
            main.title.size = 3,
            legend.width = 15,
            legend.text.size = 1,
            legend.position  = c('left', 'top'),
            legend.height = 3
            #legend.format = list(fun = function (x) {ifelse(x %in% c(0, 10, 90, 180), x, "")})
  )



spatial_fut_density_plot_Ca_Fe_PRO_Se_VA_Zn <- tmap_arrange(plt_diff_shape_NR_Ca, plt_diff_shape_NR_Fe, plt_diff_shape_NR_PRO, legend_spatial_NR_fut_map,
                                                       plt_diff_shape_NR_Se, plt_diff_shape_NR_VA, plt_diff_shape_NR_Zn, 
                                                       ncol = 4, nrow = 2) 

tmap_save(tm = spatial_fut_density_plot_Ca_Fe_PRO_Se_VA_Zn, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cv/fut_density_arrange_all.png", 
          device = png)

#Exploratory analysis: Correlations species vs. size ----------------------------------------------------------------
#**Add species traits - SIZE**
#

sp_nutr_unique <- sp_nutr[!duplicated(sp_nutr[,'fb_name']),] # only unique species 

sp_cur_nutr_unique <- sp_nutr_unique %>% filter(hab_curdams == TRUE) #current
sp_fut_nutr_unique <- sp_nutr_unique %>% filter(hab_futdams == TRUE)

sp_nutr_unique_small <- sp_cur_nutr_unique[, c("fb_name", "Iron_mu",
                          "Selenium_mu", "Zinc_mu", "Vitamin_A_mu", "Protein_mu","Calcium_mu", "Omega_3_mu" )]


sp_size <- species(sp_nutr_unique_small$fb_name, # vector of species names
                        fields = c("Species", "Brack","Saltwater","Fresh","AnaCat","Length", "Importance"))
colnames(sp_size) <- c("fb_name", "Brackish","Saltwater","Freshwater","Migration","Length", "Commercial")

sp_size_nutr <- left_join(sp_nutr_unique_small, sp_size, by ="fb_name")


# scatterplot (without dam scenarios)
#plot(sp_nutr_4$DIST_MAIN, sp_nutr_4$mean_NRn_adult_tot, main="Scatterplot Example",
# xlab="km ", ylab="percentage ", pch=19)
#abline(lm(sp_nutr_4$mean_NRn_adult_tot ~ sp_nutr_4$DIST_MAIN, data = mtcars), 
#                                     col = "blue")
#cor(sp_nutr_4$DIST_MAIN, sp_nutr_4$mean_NRn_adult_tot, method = c("pearson", "kendall", "spearman"))
#cor.test(sp_nutr_4$DIST_MAIN, sp_nutr_4$mean_NRn_adult_tot, method=c("pearson", "kendall", "spearman"))

#current vs. future
#plot(cur_dam_adult_mean_hb_df$DIST_MAIN,cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_tot, main="Scatterplot Example",
# xlab="distance to main basin (km) ", ylab=" nutrient density (%) ", pch=19)

#abline(lm(cur_dam_adult_mean_hb_df$DIST_MAIN ~ cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_tot, data = mtcars), 
#col = "blue")

#cor(cur_dam_adult_mean_hb_df$DIST_MAIN, cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_tot, method = c("pearson", "kendall", "spearman"))
#cor.test(cur_dam_adult_mean_hb_df$DIST_MAIN, cur_dam_adult_mean_hb_df$cu_mean_NRn_adult_tot, method=c("pearson", "kendall", "spearman"))
library("ggpubr")

#Correlation analysis


#Ca CONTENT x SIZE
#Outliers
#Ca concentration
sp_size_nutr[which(sp_size_nutr$Calcium_mu %in% c(boxplot.stats(sp_size_nutr$Calcium_mu)$out)),]


#remove outliers from dataframe
quartiles_Ca <- quantile(sp_size_nutr$Calcium_mu, probs=c(.25, .75), na.rm = FALSE)
IQR_Ca <- IQR(sp_size_nutr$Calcium_mu)

Lower_Ca <- quartiles_Ca[1] - 1.5*IQR_Ca
Upper_Ca <- quartiles_Ca[2] + 1.5*IQR_Ca 



sp_size_nutr_Ca_no_out <- subset(sp_size_nutr, sp_size_nutr$Calcium_mu > Lower_Ca & sp_size_nutr$Calcium_mu < Upper_Ca)

sp_size_nutr_Ca_size_no_out <- subset(sp_size_nutr_Ca_no_out, sp_size_nutr_Ca_no_out$Length > Lower_size & sp_size_nutr_Ca_no_out$Length < Upper_size)

#Assumption 1: Are the data from each of the 2 variables (x, y) follow a normal distribution? 

#shapiro-wilk normality test 
shapiro.test(sp_size_nutr_Ca_size_no_out$Length)#NO, because n = 57, W=0.80, p = 3.037*10^-7
shapiro.test(sp_size_nutr_Ca_size_no_out$Calcium_mu) #NO, because n = 57, W = 0.92, p = 0.0016

#qqplot
QQ_Ca_size <- ggqqplot(sp_size_nutr_Ca_size_no_out$Length, ylab = "Length") #NO
QQ_Ca <- ggqqplot(sp_size_nutr_Ca_size_no_out$Calcium_mu, ylab = "Calcium_mu") #YES


#Assumption 2: Covariation linear? YES
Ca_scat_plt <- ggscatter(sp_size_nutr_Ca_size_no_out, x = "Length", y = "Calcium_mu", 
          add = "reg.line", conf.int = F, 
          cor.coeff.args = list(method = "spearman", label.x = 120, label.sep = "\n"),
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "length (cm) ", ylab = "Ca concentration (mg/100g)") +
  scale_y_continuous(
    expand = expansion(0),
    breaks = get_breaks(by = 250 , from = 0),
    limits =  c(0, 750)
  ) +
  scale_x_continuous(
    #expand = expansion(0),
    breaks = get_breaks(by = 40, from = 0),
    limits = c(0, 160)
  ) 


#correlation Ca x size  #S = 47729, p-value = 1.077e-05, n=57
cor_Ca_size <- cor.test(sp_size_nutr_Ca_size_no_out$Calcium_mu, sp_size_nutr_Ca_size_no_out$Length,method="spearman", exact = F)

#Fe concentration X Size

#Outliers
#Iron concentration
sp_size_nutr[which(sp_size_nutr$Iron_mu %in% c(boxplot.stats(sp_size_nutr$Iron_mu)$out)),]


#Size
sp_size_nutr[which(sp_size_nutr$Length %in% c(boxplot.stats(sp_size_nutr$Length)$out)),]


#remove outliers from dataframe
quartiles_Fe <- quantile(sp_size_nutr$Iron_mu, probs=c(.25, .75), na.rm = FALSE)
IQR_Fe <- IQR(sp_size_nutr$Iron_m)

Lower_Fe <- quartiles_Fe[1] - 1.5*IQR_Fe
Upper_Fe <- quartiles_Fe[2] + 1.5*IQR_Fe 


quartiles_size <- quantile(sp_size_nutr$Length, probs=c(.25, .75), na.rm = FALSE)
IQR_size <- IQR(sp_size_nutr$Length)
Lower_size <- quartiles_size[1] - 1.5*IQR_size
Upper_size <- quartiles_size[2] + 1.5*IQR_size


sp_size_nutr_Fe_no_out <- subset(sp_size_nutr, sp_size_nutr$Iron_mu > Lower_Fe & sp_size_nutr$Iron_mu < Upper_Fe)

sp_size_nutr_Fe_size_no_out <- subset(sp_size_nutr_Fe_no_out, sp_size_nutr_Fe_no_out$Length > Lower_size & sp_size_nutr_Fe_no_out$Length < Upper_size)

#Assumption 1: Are the data from each of the 2 variables (x, y) follow a normal distribution? 

#shapiro-wilk normality test 
shapiro.test(sp_size_nutr_Fe_size_no_out$Length)#NO, because n = 55, W=0.80, p = 3.44 * 10^-7
shapiro.test(sp_size_nutr_Fe_size_no_out$Iron_mu) #NO, because n = 55, W = 0.93, p = 0.0047

#qqplot
QQ_Fe_size <- ggqqplot(sp_size_nutr_Fe_size_no_out$Length, ylab = "Length") #NO - Spearman test
QQ_Fe <- ggqqplot(sp_size_nutr_Fe_size_no_out$Iron_mu, ylab = "Iron_mu") #YES 


#Assumption 2: Covariation linear?
Fe_scat_plt <- ggscatter(sp_size_nutr_Fe_size_no_out, x = "Length", y = "Iron_mu", 
          add = "reg.line", conf.int = F, 
          cor.coef = TRUE, cor.method = "spearman",
          cor.coeff.args = list(method = "spearman", label.x = 120, label.sep = "\n"),
          xlab = "length (cm) ", ylab = "Fe concentration (mg/100g)") +
  scale_y_continuous(
    expand = expansion(0),
    breaks = get_breaks(by = 0.5 , from = 0),
    limits =  c(0, 2.5)
  ) +
  scale_x_continuous(
    #expand = expansion(0),
    breaks = get_breaks(by = 40, from = 0),
    limits = c(0, 160)
  ) 


#spearman correlation Fe x size  #S = 42312, p-value = 3.66e-05, n=55
cor_Fe_size <- cor.test(sp_size_nutr_Fe_size_no_out$Iron_mu, sp_size_nutr_Fe_size_no_out$Length,method="spearman", exact = F)

#PRO content x SIZE
sp_size_nutr[which(sp_size_nutr$Protein_mu %in% c(boxplot.stats(sp_size_nutr$Protein_mu)$out)),]


#remove outliers from dataframe
quartiles_PRO <- quantile(sp_size_nutr$Protein_mu, probs=c(.25, .75), na.rm = FALSE)
IQR_PRO <- IQR(sp_size_nutr$Protein_mu)

Lower_PRO <- quartiles_PRO[1] - 1.5*IQR_PRO
Upper_PRO <- quartiles_PRO[2] + 1.5*IQR_PRO 



sp_size_nutr_PRO_no_out <- subset(sp_size_nutr, sp_size_nutr$Protein_mu > Lower_PRO & sp_size_nutr$Protein_mu < Upper_PRO)

sp_size_nutr_PRO_size_no_out <- subset(sp_size_nutr_PRO_no_out, sp_size_nutr_PRO_no_out$Length > Lower_size & sp_size_nutr_PRO_no_out$Length < Upper_size)

#Assumption 1: Are the data from each of the 2 variables (x, y) follow a normal distribution? 

#shapiro-wilk normality test 
shapiro.test(sp_size_nutr_PRO_size_no_out$Length)#NO, because n = 55, W=0.80, p = 4.003e-07
shapiro.test(sp_size_nutr_PRO_size_no_out$Protein_mu) #YES, because n = 55, W = 0.98, p = 0.5112

#qqplot
QQ_PRO_size <- ggqqplot(sp_size_nutr_PRO_size_no_out$Length, ylab = "Length") #NO
QQ_PRO <- ggqqplot(sp_size_nutr_PRO_size_no_out$Protein_mu, ylab = "Protein_mu") #YES


#Assumption 2: Covariation linear? YES
PRO_scat_plt <- ggscatter(sp_size_nutr_PRO_size_no_out, x = "Length", y = "Protein_mu", 
                         #add = "reg.line",
                         cor.coeff.args = list(method = "spearman", label.x = 120, label.sep = "\n"),
                         conf.int = F, 
                         cor.coef = TRUE, cor.method = "spearman",
                         xlab = "length (cm) ", ylab = "PRO concentration (g/100g)") +
  scale_y_continuous(
    expand = expansion(0),
    breaks = get_breaks(by = 2 , from = 15),
    limits =  c(15, 21)
  ) +
  scale_x_continuous(
    #expand = expansion(0),
    breaks = get_breaks(by = 40, from = 0),
    limits = c(0, 160)
  ) 

#spearman correlation PRO x size  #S = 33144, p-value = 0.1522, n=55
cor_PRO_size <- cor.test(sp_size_nutr_PRO_size_no_out$Protein_mu, sp_size_nutr_PRO_size_no_out$Length, method="spearman", exact = F)


#Se content x SIZE
sp_size_nutr[which(sp_size_nutr$Selenium_mu %in% c(boxplot.stats(sp_size_nutr$Selenium_mu)$out)),]


#remove outliers from dataframe
quartiles_Se <- quantile(sp_size_nutr$Selenium_mu, probs=c(.25, .75), na.rm = FALSE)
IQR_Se <- IQR(sp_size_nutr$Selenium_mu)

Lower_Se <- quartiles_Se[1] - 1.5*IQR_Se
Upper_Se <- quartiles_Se[2] + 1.5*IQR_Se 



sp_size_nutr_Se_no_out <- subset(sp_size_nutr, sp_size_nutr$Selenium_mu > Lower_Se & sp_size_nutr$Selenium_mu < Upper_Se)

sp_size_nutr_Se_size_no_out <- subset(sp_size_nutr_Se_no_out, sp_size_nutr_Se_no_out$Length > Lower_size & sp_size_nutr_Se_no_out$Length < Upper_size)

#Assumption 1: Are the data from each of the 2 variables (x, y) follow a normal distribution? 

#shapiro-wilk normality test 
shapiro.test(sp_size_nutr_Se_size_no_out$Length) #NO, because n = 53, W=0.78997, p = 2.873e-07
shapiro.test(sp_size_nutr_Se_size_no_out$Selenium_mu) #NO, because n = 53, W = 0.94865, p = 0.02355

#qqplot
QQ_Se_size <- ggqqplot(sp_size_nutr_Se_size_no_out$Length, ylab = "Length") #NO
QQ_Se <- ggqqplot(sp_size_nutr_Se_size_no_out$Selenium_mu, ylab = "Selenium_mu") #YES


#Assumption 2: Covariation linear? YES

#scatterplot n = 53
Se_scat_plt <- ggscatter(sp_size_nutr_Se_size_no_out, x = "Length", y = "Selenium_mu", 
                          add = "reg.line", conf.int = F,
                         cor.coeff.args = list(method = "spearman", label.x = 120, label.sep = "\n"),
                          cor.coef = TRUE, cor.method = "spearman",
                          xlab = "length (cm) ", ylab = "Se concentration (µg/100g)") +
  scale_y_continuous(
    expand = expansion(0),
    breaks = get_breaks(by = 30, from = 0),
    limits =  c(0, 150)
  ) +
  scale_x_continuous(
    #expand = expansion(0),
    breaks = get_breaks(by = 40, from = 0),
    limits = c(0, 160)
  ) 

#spearman correlation Ca x size  #S = 12423, p-value = 0.000142, n=53
cor_Se_size <- cor.test(sp_size_nutr_Se_size_no_out$Selenium_mu, sp_size_nutr_Se_size_no_out$Length,method="spearman", exact = F)


#VA content x SIZE
sp_size_nutr[which(sp_size_nutr$Vitamin_A_mu %in% c(boxplot.stats(sp_size_nutr$Vitamin_A_mu)$out)),]


#remove outliers from dataframe
quartiles_VA <- quantile(sp_size_nutr$Vitamin_A_mu, probs=c(.25, .75), na.rm = F)
IQR_VA <- IQR(sp_size_nutr$Vitamin_A_mu)

Lower_VA <- quartiles_VA[1] - 1.5*IQR_VA
Upper_VA <- quartiles_VA[2] + 1.5*IQR_VA 



sp_size_nutr_VA_no_out <- subset(sp_size_nutr, sp_size_nutr$Vitamin_A_mu > Lower_VA & sp_size_nutr$Vitamin_A_mu < Upper_VA)

sp_size_nutr_VA_size_no_out <- subset(sp_size_nutr_VA_no_out, sp_size_nutr_VA_no_out$Length > Lower_size & sp_size_nutr_VA_no_out$Length < Upper_size)

#Assumption 1: Are the data from each of the 2 variables (x, y) follow a normal distribution? 

#shapiro-wilk normality test 
shapiro.test(sp_size_nutr_VA_size_no_out$Length) #NO, because n = 51, W=0.8251, p = 2.914e-06
shapiro.test(sp_size_nutr_VA_size_no_out$Vitamin_A_mu) #No, because n = 51, W = 0.92829, p = 0.004282

#qqplot
QQ_VA_size <- ggqqplot(sp_size_nutr_VA_size_no_out$Length, ylab = "Length") #NO
QQ_VA <- ggqqplot(sp_size_nutr_VA_size_no_out$Vitamin_A_mu, ylab = "Vitamin_A_mu") #YES


#Assumption 2: Covariation linear? YES

#scatterplot n = 51
VA_scat_plt <- ggscatter(sp_size_nutr_VA_size_no_out, x = "Length", y = "Vitamin_A_mu", 
                         #add = "reg.line", 
                         conf.int = F,
                         cor.coeff.args = list(method = "spearman", label.x = 120, label.sep = "\n"),
                         cor.coef = TRUE, cor.method = "spearman",
                         xlab = "length (cm) ", ylab = "VA concentration (µg/100g)") +
  scale_y_continuous(
    expand = expansion(0),
    breaks = get_breaks(by = 25, from = 0),
    limits =  c(0, 150)
  ) +
  scale_x_continuous(
    #expand = expansion(0),
    breaks = get_breaks(by = 40, from = 0),
    limits = c(0, 160)
  ) 


#spearman correlation VA x size  #S = 27989, p-value = 0.05873, n=51
cor_VA_size <- cor.test(sp_size_nutr_VA_size_no_out$Vitamin_A_mu, sp_size_nutr_VA_size_no_out$Length, method="spearman", exact = F)


#Zn content x SIZE
sp_size_nutr[which(sp_size_nutr$Zinc_mu %in% c(boxplot.stats(sp_size_nutr$Zinc_mu)$out)),]


#remove outliers from dataframe
quartiles_Zn <- quantile(sp_size_nutr$Zinc_mu, probs=c(.25, .75), na.rm = F)
IQR_Zn <- IQR(sp_size_nutr$Zinc_mu)

Lower_Zn <- quartiles_Zn[1] - 1.5*IQR_Zn
Upper_Zn <- quartiles_Zn[2] + 1.5*IQR_Zn 



sp_size_nutr_Zn_no_out <- subset(sp_size_nutr, sp_size_nutr$Zinc_mu > Lower_Zn & sp_size_nutr$Zinc_mu < Upper_Zn)

sp_size_nutr_Zn_size_no_out <- subset(sp_size_nutr_Zn_no_out, sp_size_nutr_Zn_no_out$Length > Lower_size & sp_size_nutr_Zn_no_out$Length < Upper_size)

#Assumption 1: Are the data from each of the 2 variables (x, y) follow a normal distribution? 

#shapiro-wilk normality test 
shapiro.test(sp_size_nutr_Zn_size_no_out$Length) #NO, because n = 56, W=0.80103, p = 2.968e-07
shapiro.test(sp_size_nutr_Zn_size_no_out$Zinc_mu) #No, because n = 56, W = 0.96433, p = 0.09629

#qqplot
QQ_Zn_size <- ggqqplot(sp_size_nutr_Zn_size_no_out$Length, ylab = "Length") #NO
QQ_Zn <- ggqqplot(sp_size_nutr_Zn_size_no_out$Zinc_mu, ylab = "Zinc_mu") #YES


#Assumption 2: Covariation linear? YES

#scatterplot n = 51
Zn_scat_plt <- ggscatter(sp_size_nutr_Zn_size_no_out, x = "Length", y = "Zinc_mu", 
                         add = "reg.line", conf.int = F, 
                         cor.coeff.args = list(method = "spearman", label.x = 120, label.sep = "\n"),
                         cor.coef = TRUE, cor.method = "spearman",
                         xlab = "length (cm) ", ylab = "Zn concentration (µg/100g)") +
  scale_y_continuous(
    expand = expansion(0),
    breaks = get_breaks(by = 0.5, from = 0),
    limits =  c(0, 3.5)
  ) +
  scale_x_continuous(
    breaks = get_breaks(by = 40, from = 0),
    limits = c(0, 160)
  ) 
  

#spearman correlation Zn x size  #S = 47335, p-value = 3.94e-07, n = 56
cor_Zn_size <- cor.test(sp_size_nutr_Zn_size_no_out$Zinc_mu, sp_size_nutr_Zn_size_no_out$Length,method="spearman", exact = F)


#arrange QQ plots and Scatterplots
QQplot_size_Ca_Fe_PRO_Se_VA_Zn <- ggarrange(QQ_Ca_size, QQ_Ca, QQ_Fe_size,  QQ_Fe, 
                                            QQ_PRO_size, QQ_PRO, QQ_Ca_size, QQ_Se, 
                                            QQ_VA_size, QQ_VA, QQ_Zn_size, QQ_Zn,
                                       labels = c("A","B","C","D",
                                                  "E","F","G","H",
                                                  "I", "J","K","L"),
                                       ncol = 4, nrow =3)

#list all correlations in list
library(broom)
library(purrr)

nutrients_size_cor <- list(
  Calcium = cor_Zn_size,
  Iron = cor_Fe_size,
  Protein = cor_PRO_size,
  Selenium = cor_Se_size,
  Vitamin_A = cor_VA_size,
  Zinc = cor_Zn_size
)

nutrients_size_cor_df <- data.frame(map_dfr(nutrients_size_cor, tidy, .id = 'nutrient'))

#all scatterplots where PRO and VA because non-significant
scat_plot_Ca_Fe_PRO_Se_VA_Zn <- ggarrange(Ca_scat_plt, Fe_scat_plt, PRO_scat_plt,
                                          Se_scat_plt, VA_scat_plt, Zn_scat_plt,
          labels = c("A","B","C",
                     "D", "E","F"),
          ncol = 3, nrow =2)




#save
ggsave(filename = "QQ_plot_size_Ca_Fe_PRO_Se_VA_Zn.png", plot = QQplot_size_Ca_Fe_PRO_Se_VA_Zn,
       path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
       device = png, width = 10, height = 7, dpi = 300)
ggsave(filename = "scat_plot_Ca_Fe_PRO_Se_VA_Zn.png", plot = scat_plot_Ca_Fe_PRO_Se_VA_Zn,
       path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
       device = png, width = 11, height = 7, dpi = 300)







#Appendix C: Freshwater fish species in the Mekong Basin ----------------------------------------------------------------

#Table C2: Current and difference map of total species per hydrobasin

#current
df_cur_sp_nutr <- sp_nutr %>% filter(hab_curdams == TRUE)
df_cur_sp_nutr_1 <- df_cur_sp_nutr %>%
  group_by(HYBAS_ID) %>%
  summarise(total_count = n(),
            .groups = 'drop') %>%
  as.data.frame()
  
df_cur_sp_nutr_2 <- left_join(df_cur_sp_nutr_1, hb %>% 
                                 dplyr::select(HYBAS_ID,MAIN_BAS, geometry)) %>%
  filter(MAIN_BAS != '4120024890') %>%
  
  st_as_sf()


plt_cur_shape_total_sp <- tm_shape(df_cur_sp_nutr_2) + 
  tm_polygons("total_count", 
              style="cont", 
              breaks = seq(0, 64, 4), 
              title="",
              palette = "Reds",
              #n = 10, 
              textNA = "\u2264 1 species",
              colorNA = "grey",              
              border.alpha = 0.05) +
  tm_layout(legend.show = T,
            frame = FALSE,
            main.title = "Total species",
            main.title.position = c('right', 'top'),
            main.title.size = 3,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% seq(0, 64, 8), x, "")}),
            legend.height = 1,
            legend.outside = T) 



tmap_save(tm = plt_cur_shape_total_sp, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cur_total_sp_hb.png", 
          device = png,
          width = 3000, 
          height = 3300, 
          dpi =300)
tmap_save(plt_cur_shape_total_sp, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/cur_total_sp_hb.html")

#difference
df_fut_sp_nutr <- sp_nutr %>% filter(hab_futdams == TRUE)
df_fut_sp_nutr_1 <- df_fut_sp_nutr %>%
  group_by(HYBAS_ID) %>%
  summarise(total_count = n(),
            .groups = 'drop') %>%
  as.data.frame()
df_diff_sp <- data.frame(HYBAS_ID = df_fut_sp_nutr_1$HYBAS_ID,
                         total_species = df_fut_sp_nutr_1$total_count - df_cur_sp_nutr_1$total_count)

df_diff_sp_1 <- left_join(df_diff_sp, hb %>% 
                                dplyr::select(HYBAS_ID,MAIN_BAS, geometry)) %>%
  filter(MAIN_BAS != '4120024890') %>%
  
  st_as_sf()

plt_fut_shape_total_sp <- tm_shape(df_diff_sp_1) + 
  tm_polygons("total_species", 
              style="cont", 
              breaks = seq(-15, 15, 1), 
              title="",
              palette = "RdBu",
              #n = 10, 
              textNA = "\u2264 1 species",
              colorNA = "grey",              
              border.alpha = 0.05) +
  tm_layout(legend.show = T,
            frame = FALSE,
            main.title = "Total species",
            main.title.position = c('right', 'top'),
            main.title.size = 3,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% seq(-15, 15, 3), x, "")}),
            legend.height = 1,
            legend.outside = T) 

tmap_save(tm = plt_fut_shape_total_sp, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/fut_total_sp_hb.png", 
          device = png,
          width = 3000, 
          height = 3300, 
          dpi =300)
tmap_save(plt_fut_shape_total_sp, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/fut_total_sp_hb.html")

#Appendix D: Results of spatial differences in mean nutrient values across subbasins in the Mekong Basin ----------------------------------------------------------------

ttm()
sf::sf_use_s2(FALSE)

#create dataframe current status subbasins with geometry
sp_cur_nutr_cv_NA <- create_cur_cv_hb_df (sp_nutr) %>%
  filter(MAIN_BAS != '4120024890') %>%
  st_as_sf()

#create dataframe current status subbasins with geometry
sp_fut_nutr_cv_NA <- create_fut_cv_hb_df (sp_nutr) %>%
  filter(MAIN_BAS != '4120024890') %>%
  st_as_sf()

#Appendix D: Results of spatial differences in mean nutrient values across subbasins in the Mekong Basin
#spatial distribution of the mean

#Figure D1A: current Ca
plt_cur_shape_mean_cal <- tm_shape(sp_cur_nutr_cv_NA) + 
  tm_polygons("mean_Calcium", 
              style="cont", 
              breaks = seq(0, 800, 100),
              title="mg/100g",
              title.size = 2,
              palette = "Reds",
              #n = 10, 
              textNA = "\u2264 1 species",
              colorNA = "grey",              
              border.alpha = 0.025) +
  tm_layout(legend.show = TRUE,
            frame = FALSE,
            main.title = "A: Ca",
            main.title.position = c('left', 'top'),
            #legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 200, 400, 600, 800), x, "")}),
            legend.height = 1,
            legend.outside = T,
            legend.outside.position = 'right') 

tmap_save(tm = plt_cur_shape_mean_cal, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/mean_nutrient/cur_nutrient_Ca.png", 
          device = png,
          width = 1500, 
          height = 2200,
          dpi = 300)

#Figure D1B: Ca mean difference
plt_diff_shape_mean_Ca <- tm_shape(diff_dam_mean) +
  tm_polygons("diff_mean_Ca", 
              style="cont", 
              breaks = seq(-250, 250, 25), 
              title="mg/100g",
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.show = TRUE,
            main.title = "B: Change in Ca",
            main.title.position = c('left', 'top'),
            frame = FALSE,
            #legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(-250,-200, -150, -100, -50, 0, 50, 100, 
                                                                     150, 200, 250), x, "")}),
            legend.height = 1,
            legend.outside = T,
            legend.outside.position = 'right')

tmap_save(tm = plt_diff_shape_mean_Ca, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/mean_nutrient/diff_mean_Ca.png",
          device = png,
          width = 1500, 
          height = 2200,
          dpi = 300)

#Figure D1C: current Iron
plt_cur_shape_mean_iro <- tm_shape(sp_cur_nutr_cv_NA) + 
  tm_polygons("mean_Iron", 
              style="cont", 
              breaks = seq(0, 8, 0.4),
              title="mg/100g",
              palette = "Reds",
              #n = 10,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  tm_layout(legend.show = TRUE,
            frame = FALSE,
            main.title = "C: Fe",
            main.title.position = c('left', 'top'),
            #legend.width = 10,
            legend.text.size = 1.5,
            #legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(0,2,4,6, 8), x, "")}),
            legend.height = 1,
            legend.outside.position = "right",
            #legend.outside.size = 0.35,
            legend.outside = TRUE,
            ) 

tmap_save(tm = plt_cur_shape_mean_iro, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/mean_nutrient/cur_nutrient_Fe.png", 
          device = png,
          width = 1500, 
          height = 2200,
          dpi = 300,
          ) 

#Figure D1D: diff Fe mean
plt_diff_shape_mean_iro <- tm_shape(diff_dam_mean) +
  tm_polygons("diff_mean_iro", 
              style="cont", 
              breaks = seq(-5,5,0.5), 
              title="mg/100g",
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.show = TRUE,
            main.title = "D: Change in Fe",
            main.title.position = c('left', 'top'),
            frame = FALSE,
            #legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(-5,-4, -3, -2, -1, 0, 1, 2, 3, 4, 5), x, "")}),
            legend.height = 1,
            legend.outside = T,
            legend.outside.position = 'right')

tmap_save(tm = plt_diff_shape_mean_iro, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/mean_nutrient/diff_mean_Fe.png", 
          device = png,
          width = 1500, 
          height = 2200,
          dpi = 300)


#Figure D3A: Current PRO
plt_cur_shape_mean_prot <- tm_shape(sp_cur_nutr_cv_NA) + 
  tm_polygons("mean_Protein", 
              style="cont", 
              breaks = seq(15, 25, 1.25),
              title = "g/100g",
              #n = 10,
              palette = "Reds",
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  tm_layout(legend.show = TRUE,
            frame = FALSE,
            main.title = "E: PRO",
            main.title.position = c('left', 'top'),
            #legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('left', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(15, 17.5, 20, 22.5, 25), x, "")}),
            legend.height = 1,
            legend.outside.position = "right",
            #legend.outside.size = 0.35,
            legend.outside = TRUE)

tmap_save(tm = plt_cur_shape_mean_prot, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/mean_nutrient/cur_nutrient_PRO.png", 
          device = png,
          width = 1500, 
          height = 2200,
          dpi = 300)

#Figure D3B: diff PRO mean
plt_diff_shape_mean_PRO <- tm_shape(diff_dam_mean) +
  tm_polygons("diff_mean_PRO", 
              style="cont", 
              breaks = seq(-5, 5, 0.5), 
              title="g/100g",
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.show = TRUE,
            main.title = "F:Change in PRO",
            main.title.position = c('left', 'top'),
            frame = FALSE,
            #legend.width = 10,
            legend.outside = T,
            legend.outside.position = 'right',
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(-5.0,-4.0, -3.0, -2.0, -1.0, 0, 1.0, 2.0, 3.0, 4.0, 5.0), x, "")}),
            legend.height = 1)

tmap_save(tm = plt_diff_shape_mean_PRO, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/mean_nutrient/diff_mean_PRO.png", 
          device = png,
          width = 1500, 
          height = 2200,
          dpi = 300)


#Figure D4A: current Se
plt_cur_shape_mean_sel <- tm_shape(sp_cur_nutr_cv_NA) +
  tm_polygons("mean_Selenium", 
              style="cont", 
              breaks = seq(0, 200, 25),
              title=" μg/100g",
              palette = "Reds",
              #n= 10,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  tm_layout(legend.show = TRUE,
            main.title = "G: Se",
            main.title.position = c('left', 'top'),
            frame = FALSE,
            #legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 50, 100, 150, 200), x, "")}),
            legend.height = 1,
            legend.outside.position = "right",
            #legend.outside.size = 0.35,
            legend.outside = TRUE) 


tmap_save(tm = plt_cur_shape_mean_sel, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/mean_nutrient/cur_nutrient_Se.png", 
          device = png,
          width = 1500, 
          height = 2200,
          dpi = 300)

#Figure D4B:Se current and future difference mean
plt_diff_shape_mean_sel <- tm_shape(diff_dam_mean) +
  tm_polygons("diff_mean_se", 
              style="cont", 
              breaks = seq(-50,50, 5), 
              title="µg/100g",
              palette = "RdBu",
              #n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.show = TRUE,
            main.title = "H: Change in Se",
            main.title.position = c('left', 'top'),
            frame = FALSE,
            #legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1,
            legend.outside = T,
            legend.outside.position = 'right') 

tmap_save(tm = plt_diff_shape_mean_sel, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/mean_nutrient/diff_mean_Se.png", 
          device = png,
          width = 1500, 
          height = 2200,
          dpi = 300)


#Figure D5A: current VA
plt_cur_shape_mean_vit_A <- tm_shape(sp_cur_nutr_cv_NA) + 
  tm_polygons("mean_Vitamin_A", 
              style="cont", 
              breaks = seq(0, 320, 40),
              title="μg/100g",
              palette = "Reds",
              #n = 10,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  tm_layout(legend.show = TRUE,
            frame = FALSE,
            main.title = "I: VA",
            main.title.position = c('left', 'top'),
            #legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 80, 160, 240, 320), x, "")}),
            legend.height = 1,
            legend.outside.position = "right",
            #legend.outside.size = 0.35,
            legend.outside = TRUE) 

tmap_save(tm = plt_cur_shape_mean_vit_A, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/mean_nutrient/cur_nutrient_VA.png", 
          device = png,
          width = 1500, 
          height = 2200,
          dpi = 300)

#Figure D5B: diff VA mean
plt_diff_shape_mean_VA <- tm_shape(diff_dam_mean) +
  tm_polygons("diff_mean_VA", 
              style="cont", 
              breaks = seq(-50, 50, 5), 
              title="µg/100g",
              palette = "RdBu",
              n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.show = TRUE,
            main.title = "J: Change in VA ",
            main.title.position = c('left', 'top'),
            frame = FALSE,
            #legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(-50,-40, -30, -20, -10, 0, 10, 20, 30, 40, 50), x, "")}),
            legend.height = 1,
            legend.outside = T,
            legend.outside.position = 'right')

tmap_save(tm = plt_diff_shape_mean_VA, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/mean_nutrient/diff_mean_VA.png", 
          device = png,
          width = 1500, 
          height = 2200,
          dpi = 300
          )


#Figure D6A: current Zn
plt_cur_shape_mean_zin <- tm_shape(sp_cur_nutr_cv_NA) + 
  tm_polygons("mean_Zinc", 
                style="cont", 
                breaks = seq(0, 3.6, 0.2),
                title="μg/100g",
                palette = "Reds",
                #n = 10,
                textNA = "\u2264 1 species",
                colorNA = "grey",
                border.alpha = 0.025) +
  tm_layout(legend.show = TRUE,
            frame = FALSE,
            main.title = "K: Zn",
            main.title.position = c('left', 'top'),
            #legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(0, 0.4, 0.8, 1.2, 1.6, 2.0, 2.4, 2.8, 3.2, 3.6), x, "")}),
            legend.height = 1,
            legend.outside.position = "right",
            #legend.outside.size = 0.35,
            legend.outside = TRUE
            )

tmap_save(tm = plt_cur_shape_mean_zin, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/mean_nutrient/cur_nutrient_Zn.png", 
          device = png,
          width = 1500, 
          height = 2200,
          dpi = 300)


#Figure D6B: diff Zn mean
plt_diff_shape_mean_zn <- tm_shape(diff_dam_mean) +
  tm_polygons("diff_mean_Zn", 
              style="cont", 
              breaks = seq(-5,5, 0.5), 
              title="µg/100g",
              palette = "RdBu",
              n= 10,
              midpoint = NA,
              textNA = "\u2264 1 species",
              colorNA = "grey",
              border.alpha = 0.025) +
  
  tm_layout(legend.show = TRUE,
            main.title = "L: Change in Zn",
            main.title.position = c('left', 'top'),
            frame = FALSE,
            legend.width = 10,
            legend.text.size = 1.5,
            legend.position  = c('right', 'top'),
            legend.format = list(fun = function (x) {ifelse(x %in% c(-5.0,-4.0, -3.0, -2.0, -1.0, 0, 1.0, 2.0, 3.0, 4.0, 5.0), x, "")}),
            legend.height = 1,
            legend.outside = T,
            legend.outside.position = 'right')

tmap_save(tm = plt_diff_shape_mean_zn, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/mean_nutrient/diff_mean_zn.png", 
          device = png,
          width = 1500, 
          height = 2200,
          dpi = 300)




#Figure D1A, D1B
spatial_cur_mean_plot_Ca_cur_fut <- tmap_arrange(plt_cur_shape_mean_cal, plt_diff_shape_mean_Ca,
                                                       ncol = 1, nrow = 1) 
#save
tmap_save(tm = spatial_cur_mean_plot_Ca_cur_fut, 
          filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/mean_nutrient/cur_mean_arrange_Ca_cur_fut.png", 
          device = png)


#Figure D1A, D1B
spatial_cur_mean_plot_Se_VA_Zn <- tmap_arrange(plt_cur_shape_mean_sel, plt_cur_shape_mean_vit_A, plt_cur_shape_mean_zin, 
                                               ncol = 3, nrow = 1)
  
                                               
#Figure D2A, D2B                                               
                                               
#Figure D3A, D3B  


#Figure D4A, D4B

#Figure D5A, D5B

#Figure D6A, D6B


#save
tmap_save(tm = spatial_cur_mean_plot_Ca_Fe_PRO, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/mean_nutrient/cur_mean_arrange_Ca_Fe_PRO.png", 
          device = png)

tmap_save(tm = spatial_cur_mean_plot_Se_VA_Zn, filename = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken/mean_nutrient/cur_mean_arrange_Se_VA_Zn.png", 
          device = png)


#future differences mean























