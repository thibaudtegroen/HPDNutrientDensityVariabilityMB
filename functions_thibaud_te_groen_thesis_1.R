#**R Functions Thesis
# Thibaud te Groen 

# oktober 2022

#code adapted from Tamara Keijzer IUCN_hybas_fishbase.R 
#Based on validate_fishbase_names.R



#**Data Cleaning and Create dataframes**

#Omit NA values and duplicates fb species names and 
#create dataframe with nutrients and fishbase names in Mekong

data_cleaning <- function(match_fb_sp, nutrient_tool, fb_species_tam, dam_extinction_data){
  
  #dataframe_1 <- data.frame(na.omit(unique(dataframe_1$fb_name)))
  #colnames(match_fb_sp)[1] <- 'fb_name'
 ## colnames(nutrient_tool)[1] <- 'fb_name' #however GitHub dataframe is in reality not fishbase matched names
 # colnames(fb_species_tam)[1] <- 'fb_name'
 # 
  #match and left join Nutrient Values with matched Nutrient Fishbase Names with 
  species_dataframe_1 <- left_join(nutrient_tool, match_fb_sp, by = c("species"= "binomial"))
  
  #skip NA values nutrients 
  species_dataframe_2 <- species_dataframe_1[!is.na(species_dataframe_1$Selenium_mu),]
  
  #skip duplicates
  species_dataframe_3 <- species_dataframe_2[!duplicated(species_dataframe_2$fb_name),]
  
  #match cleaned nutrient data set with database species Tamara
  species_dataframe_4 <- left_join(fb_species_tam, species_dataframe_3, by = "fb_name")
  
  #skip NA values
  cleaned_nutrients_dataframe <- species_dataframe_4[!is.na(species_dataframe_4$Selenium_mu),]
  
  
  samenvoeging <- left_join(dam_extinction_data, cleaned_nutrients_dataframe, by = "fb_name") 
  
  samenvoeging_2 <- samenvoeging[!is.na(samenvoeging$Selenium_mu),]
  
  #samenvoeging <- left_join(mekong_ID, cleaned_nutrients_dataframe, by = "fb_name") 
  
  #samenvoeging_2 <- samenvoeging[!is.na(samenvoeging$Selenium_mu),]
  
  return(samenvoeging_2)
}

data_cleaning_1 <- function(match_fb_sp, nutrient_tool, fb_species_tam, dam_extinction_data){ 
  #Asian Food Balance, Nutrient Base and Extinction base
  
  #dataframe_1 <- data.frame(na.omit(unique(dataframe_1$fb_name)))
  #colnames(match_fb_sp)[1] <- 'fb_name'
  ## colnames(nutrient_tool)[1] <- 'fb_name' #however GitHub dataframe is in reality not fishbase matched names
  # colnames(fb_species_tam)[1] <- 'fb_name'
  # 
  #match and left join Nutrient Values with matched Nutrient Fishbase Names with 
  species_dataframe_1 <- left_join(nutrient_tool, match_fb_sp, by = c("species"= "binomial"))
  
  #skip NA values nutrients 
  species_dataframe_2 <- species_dataframe_1[!is.na(species_dataframe_1$Selenium_mu),]
  
  #skip duplicates
  species_dataframe_3 <- species_dataframe_2[!duplicated(species_dataframe_2$fb_name),]
  
  #match cleaned nutrient data set with database species Tamara
  species_dataframe_4 <- left_join(fb_species_tam, species_dataframe_3, by = "fb_name")
  
  #skip NA values
  cleaned_nutrients_dataframe <- species_dataframe_4[!is.na(species_dataframe_4$Selenium_mu),]
  
  
  samenvoeging <- left_join(dam_extinction_data, cleaned_nutrients_dataframe, by = "fb_name") 
  
  samenvoeging_2 <- samenvoeging[!is.na(samenvoeging$Selenium_mu),]
  
  #samenvoeging <- left_join(mekong_ID, cleaned_nutrients_dataframe, by = "fb_name") 
  
  #samenvoeging_2 <- samenvoeging[!is.na(samenvoeging$Selenium_mu),]
  
  return(samenvoeging_2)
}

# Create traits dataframe fishbase and add same dataframe
traits_fb <- function (df_no_traits, df_geo_data) {
  
  #equal column names (fishbase names)
  colnames(df_no_traits)[1] <- 'fb_name'
  colnames(df_geo_data)[1] <- 'fb_name'
  
  traits.sp <- species(df_no_traits$fb_name, # vector of species names
                     
                     fields = c("Species", "Brack","Saltwater",
                                "Fresh","AnaCat","Length", "Importance"))

  colnames(traits.sp) = c("fb_name", "Brackish","Saltwater","Freshwater",
                          "Migration","Length", "Commercial")
  
  traits.sp$Migration[traits.sp$Migration == " "] <- NA
  
  #create new dataframe with geodata and fishbase characteristics
  df_fb_sp_traits_nutr_mu_conf_geo_data <- left_join(df_no_traits, df_geo_data, by = 'fb_name')
  
  #rename all columns
  df_fb_sp_traits_nutr_mu_geo_data <- df_fb_sp_traits_nutr_mu_conf_geo_data[c("fb_name",'geom', "spec_code",'Selenium_mu', 
                 'Zinc_mu', 'Protein_mu', 'Omega_3_mu', 'Calcium_mu', 
                 'Iron_mu', 'Vitamin_A_mu') ]
  
  
  return(df_fb_sp_traits_nutr_mu_geo_data)

}

#create cv and hydrobasin dataframe
create_cur_cv_hb_df <- function(df_nutr_dam){
  
  df_nutr_dam_1 <- df_nutr_dam %>% filter(hab_curdams == TRUE)
  #create new dataframe with CV and mean values of micronutrients
  sp_nutr_cv_NA <- df_nutr_dam_1 %>% #group by HydroBasin ID
    group_by(HYBAS_ID) %>% 
    summarise(CV_Selenium = sd(Selenium_mu)/mean(Selenium_mu)*100, 
              CV_Zinc = sd(Zinc_mu)/mean(Zinc_mu)*100, 
              CV_Protein = sd(Protein_mu)/mean(Protein_mu)*100, 
              CV_Omega_3 = sd(Omega_3_mu)/mean(Omega_3_mu)*100,
              CV_Calcium = sd(Calcium_mu)/mean(Calcium_mu)*100,
              CV_Iron = sd(Iron_mu)/mean(Iron_mu)*100,
              CV_Vitamin_A = sd(Vitamin_A_mu)/mean(Vitamin_A_mu)*100,
              mean_Selenium = mean(Selenium_mu), 
              mean_Zinc = mean(Zinc_mu), 
              mean_Protein = mean(Protein_mu), 
              mean_Omega_3 = mean(Omega_3_mu),
              mean_Calcium = mean(Calcium_mu),
              mean_Iron = mean(Iron_mu),
              mean_Vitamin_A = mean(Vitamin_A_mu)
    )
  
  mekong_cv_NA <- left_join(sp_nutr_cv_NA, hb %>% 
                              dplyr::select(HYBAS_ID,MAIN_BAS, geometry)) %>%
  
    st_as_sf()
  
  col_order <- c("HYBAS_ID", "MAIN_BAS",  
                 "mean_Selenium","mean_Zinc", "mean_Protein", "mean_Omega_3", "mean_Calcium","mean_Iron", "mean_Vitamin_A",
                 "CV_Selenium", "CV_Zinc", "CV_Protein", "CV_Omega_3", "CV_Calcium", "CV_Iron", "CV_Vitamin_A", "geometry" 
                 )
  
  mekong_cv_NA_reorder <- mekong_cv_NA[, col_order] #
  
  write_sf(mekong_cv_NA_reorder,"C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Fishbase/mekong_cv/hb_mekong_cv.gpkg")
  write.csv(mekong_cv_NA_reorder,"C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Fishbase/mekong_cv/hb_mekong_cv.csv")
  return(mekong_cv_NA_reorder)
}


create_fut_cv_hb_df <- function(df_nutr_dam){
  
  df_nutr_dam_1 <- df_nutr_dam %>% filter(hab_futdams == TRUE)
  #create new dataframe with CV and mean values of micronutrients
  sp_nutr_cv_NA <- df_nutr_dam_1 %>% #group by HydroBasin ID
    group_by(HYBAS_ID) %>% 
    summarise(CV_Selenium = sd(Selenium_mu)/mean(Selenium_mu)*100, 
              CV_Zinc = sd(Zinc_mu)/mean(Zinc_mu)*100, 
              CV_Protein = sd(Protein_mu)/mean(Protein_mu)*100, 
              CV_Omega_3 = sd(Omega_3_mu)/mean(Omega_3_mu)*100,
              CV_Calcium = sd(Calcium_mu)/mean(Calcium_mu)*100,
              CV_Iron = sd(Iron_mu)/mean(Iron_mu)*100,
              CV_Vitamin_A = sd(Vitamin_A_mu)/mean(Vitamin_A_mu)*100,
              mean_Selenium = mean(Selenium_mu), 
              mean_Zinc = mean(Zinc_mu), 
              mean_Protein = mean(Protein_mu), 
              mean_Omega_3 = mean(Omega_3_mu),
              mean_Calcium = mean(Calcium_mu),
              mean_Iron = mean(Iron_mu),
              mean_Vitamin_A = mean(Vitamin_A_mu)
    )
  
  mekong_cv_NA <- left_join(sp_nutr_cv_NA, hb %>% 
                              dplyr::select(HYBAS_ID,MAIN_BAS, geometry)) %>%
    
    st_as_sf()
  
  col_order <- c("HYBAS_ID","MAIN_BAS",  
                 "mean_Selenium","mean_Zinc", "mean_Protein", "mean_Omega_3", "mean_Calcium","mean_Iron", "mean_Vitamin_A",
                 "CV_Selenium", "CV_Zinc", "CV_Protein", "CV_Omega_3", "CV_Calcium", "CV_Iron", "CV_Vitamin_A", "geometry" 
  )
  
  mekong_cv_NA_reorder <- mekong_cv_NA[, col_order] #
  
  write_sf(mekong_cv_NA_reorder,"C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Fishbase/mekong_cv/hb_mekong_cv.gpkg")
  write.csv(mekong_cv_NA_reorder,"C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Fishbase/mekong_cv/hb_mekong_cv.csv")
  return(mekong_cv_NA_reorder)
}


#create dataframe with aggregated nutrient densities per hydrobasin
create_cv_hb_df <- function(df_nutr_dam){
  
  #create new dataframe with CV and mean values of micronutrients
  sp_nutr_cv_NA <- df_nutr_dam %>% #group by HydroBasin ID
    group_by(HYBAS_ID) %>% 
    summarise(CV_Selenium = sd(Selenium_mu)/mean(Selenium_mu)*100, 
              CV_Zinc = sd(Zinc_mu)/mean(Zinc_mu)*100, 
              CV_Protein = sd(Protein_mu)/mean(Protein_mu)*100, 
              CV_Omega_3 = sd(Omega_3_mu)/mean(Omega_3_mu)*100,
              CV_Calcium = sd(Calcium_mu)/mean(Calcium_mu)*100,
              CV_Iron = sd(Iron_mu)/mean(Iron_mu)*100,
              CV_Vitamin_A = sd(Vitamin_A_mu)/mean(Vitamin_A_mu)*100,
              mean_Selenium = mean(Selenium_mu), 
              mean_Zinc = mean(Zinc_mu), 
              mean_Protein = mean(Protein_mu), 
              mean_Omega_3 = mean(Omega_3_mu),
              mean_Calcium = mean(Calcium_mu),
              mean_Iron = mean(Iron_mu),
              mean_Vitamin_A = mean(Vitamin_A_mu)
    )
  
  mekong_cv_NA <- left_join(sp_nutr_cv_NA, hb %>% 
                              dplyr::select(HYBAS_ID,MAIN_BAS, DIST_MAIN, geometry)) %>%
    
    st_as_sf()
  
  col_order <- c("MAIN_BAS", "HYBAS_ID", 
                 "mean_Selenium","mean_Zinc", "mean_Protein", "mean_Omega_3", "mean_Calcium","mean_Iron", "mean_Vitamin_A",
                 "CV_Selenium", "CV_Zinc", "CV_Protein", "CV_Omega_3", "CV_Calcium", "CV_Iron", "CV_Vitamin_A", "geometry" 
  )
  
  mekong_cv_NA_reorder <- mekong_cv_NA[, col_order] #
  
  write_sf(mekong_cv_NA_reorder,"C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Fishbase/mekong_cv/hb_mekong_cv.gpkg")
  write.csv(mekong_cv_NA_reorder,"C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Fishbase/mekong_cv/hb_mekong_cv.csv")
  return(mekong_cv_NA_reorder)
}



#create hydrobasinID, cv, mean, pop count, pop density
create_cv_hb_pop_df <- function(df_nutr_dam){
  
  sp_nutr_cv_NA <- df_nutr_dam %>% 
    group_by(HYBAS_ID) %>% #group by HydroBasin ID
    summarise(CV_Selenium = sd(Selenium_mu)/mean(Selenium_mu)*100, 
              CV_Zinc = sd(Zinc_mu)/mean(Zinc_mu)*100, 
              CV_Protein = sd(Protein_mu)/mean(Protein_mu)*100, 
              CV_Omega_3 = sd(Omega_3_mu)/mean(Omega_3_mu)*100,
              CV_Calcium = sd(Calcium_mu)/mean(Calcium_mu)*100,
              CV_Iron = sd(Iron_mu)/mean(Iron_mu)*100,
              CV_Vitamin_A = sd(Vitamin_A_mu)/mean(Vitamin_A_mu)*100,
              mean_Selenium = mean(Selenium_mu), 
              mean_Zinc = mean(Zinc_mu), 
              mean_Protein = mean(Protein_mu), 
              mean_Omega_3 = mean(Omega_3_mu),
              mean_Calcium = mean(Calcium_mu),
              mean_Iron = mean(Iron_mu),
              mean_Vitamin_A = mean(Vitamin_A_mu)
    )
  
  mekong_cv_NA <- left_join(sp_nutr_cv_NA, hb_pop %>% 
                              dplyr::select(MAIN_BAS, HYBAS_ID, pop_ct_ssu,pop_ct_usu, ppd_pk_sav, ppd_pk_uav, geometry)) %>% 
    st_as_sf()
  
  col_order <- c("MAIN_BAS", "HYBAS_ID",
                 "pop_ct_ssu", "pop_ct_usu",
                 "ppd_pk_sav", "ppd_pk_uav",
                 "mean_Selenium","mean_Zinc", "mean_Protein", "mean_Omega_3", "mean_Calcium","mean_Iron", "mean_Vitamin_A",
                 "CV_Selenium", "CV_Zinc", "CV_Protein", "CV_Omega_3", "CV_Calcium", "CV_Iron", "CV_Vitamin_A", "geometry" 
  )
  
  mekong_cv_NA_reorder <- mekong_cv_NA[, col_order]
  
  write_sf(mekong_cv_NA_reorder,"C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Fishbase/mekong_cv/hb_pop_mekong_cv.gpkg")
  write.csv(mekong_cv_NA_reorder,"C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Fishbase/mekong_cv/hb_pop_mekong_cv.csv")
  return(mekong_cv_NA_reorder)
}

total_mean_std_cv <- function(df_nutrients){
mekong_mean_std_cv <- data.frame(
  mean_Selenium = mean(Selenium_mu), 
  mean_Zinc = mean(Zinc_mu),
  mean_Protein = mean(Protein_mu), 
  mean_Omega_3 = mean(Omega_3_mu),
  mean_Calcium = mean(Calcium_mu),
  mean_Iron = mean(Iron_mu),
  mean_Vitamin_A = mean(Vitamin_A_mu), 
  
  std_Selenium = sd(Selenium_mu), 
  std_Zinc = sd(Zinc_mu), 
  std_Protein = sd(Protein_mu), 
  std_Omega_3 = sd(Omega_3_mu),
  std_Calcium = sd(Calcium_mu),
  std_Iron = sd(Iron_mu),
  std_Vitamin_A = sd(Vitamin_A_mu),
  
  CV_Selenium = sd(Selenium_mu)/mean(Selenium_mu), 
  CV_Zinc = sd(Zinc_mu)/mean(Zinc_mu),
  CV_Protein = sd(Protein_mu)/mean(Protein_mu), 
  CV_Omega_3 = sd(Omega_3_mu)/mean(Omega_3_mu),
  CV_Calcium = sd(Calcium_mu)/mean(Calcium_mu),
  CV_Iron = sd(Iron_mu)/mean(Iron_mu),
  CV_Vitamin_A = sd(Vitamin_A_mu)/mean(Vitamin_A_mu)
)

  
  
  #col_order <- c("mean_Selenium","mean_Zinc", "mean_Protein", "mean_Omega_3", "mean_Calcium","mean_Iron", "mean_Vitamin_A", 
                # "std_Selenium","std_Zinc", "std_Protein", "std_Omega_3", "std_Calcium", "std_Iron", "std_Vitamin_A",
               #  "CV_Selenium", "CV_Zinc", "CV_Protein", "CV_Omega_3", "CV_Calcium", "CV_Iron", "CV_Vitamin_A") 
                 
  #mekong_cv_NA_reorder <- mekong_cv_NA[, col_order]
                 
  #write.csv(mekong_cv_NA_reorder,"C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Fishbase/mekong_cv/mekong_mean_std_mean.csv")
  return(mekong_mean_std_cv)
}
