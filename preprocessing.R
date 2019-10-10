#load and install all packages

if (!require('plyr')) install.packages('plyr'); library('plyr')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('reshape2')) install.packages('reshape2'); library('reshape2')
if (!require('Hmisc')) install.packages('Hmisc'); library('Hmisc')
if (!require('sf')) install.packages('sf'); library('sf')
if (!require('data.table')) install.packages('data.table'); library('data.table')
if (!require('rgeos')) install.packages('rgeos'); library('rgeos')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')

source("conversion.R")
source("helper.R")
source("model.R")
source("storeRules.R")

build_base <- function(input_file_path, output_csv=FALSE, output_spec=NULL, apply_rules=FALSE, apply_min_turnover=15000, apply_store_filter=NA, scaling_factor=1.0){

  ##Load all input files
  message("Loading input files...")
  LSOA_demand_surface <- "LSOA Demand Surface.csv"
  Store_list <- "StoreList.csv"
  Drivetime_matrix_a <- "Drive_time_matrix_adf.rds"
  constants <- "Constants.csv"
  
  LSOA_demand_surface <- paste(input_file_path,LSOA_demand_surface, sep="")
  Store_list <- paste(input_file_path,Store_list, sep="")
  Drivetime_matrix_a <- paste(input_file_path,Drivetime_matrix_a, sep="")
  constants <- paste(input_file_path,constants, sep="")
  
  error <- FALSE
  
  if(!file.exists(LSOA_demand_surface)){
    message('Missing LSOA demand surface input data file!')
    error <- TRUE
  }
  
  if(!file.exists(Store_list)){
    message('Missing Store List input data file!')
    error <- TRUE
  }
  
  if(!file.exists(Drivetime_matrix_a)){
    message('Missing Drivetime matrix input data file!')
    error <- TRUE
  }
  
  if(!file.exists(constants)){
    message('Missing constants input data file!')
    error <- TRUE
  }
  
  if(error){
    stop('Terminating optimisation')
  }
  
  # read csv input file
  LSOA_demand_surface_df <- read.csv(file = LSOA_demand_surface, fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  Store_list_df <- read.csv(file = Store_list, fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  Drive_time_matrix_adf <- readRDS(file = Drivetime_matrix_a)
  constants_df <- read.csv(file = constants, fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  
  message("Loading input files complete...")
  
  
  #Decrypt data - todo for future
  
  
  #Preprocess data into correct format
  message("Pre-processing input files...")
  LSOA_demand_surface_df <- subset(LSOA_demand_surface_df, select=c("geography_id", "Demand_value"))
  LSOA_demand_surface_df <- plyr::rename(LSOA_demand_surface_df, c("geography_id"="LSOA", "Demand_value"="DEMAND"))
  LSOA_demand_surface_df <- LSOA_demand_surface_df[!(duplicated(LSOA_demand_surface_df)), ]
  
  Store_list_df <- plyr::rename(Store_list_df, c("Store_id"="store"))
  Store_list_df <- Store_list_df[!(duplicated(Store_list_df)), ]
  message("Pre-processing input files complete...")
  
  #Build base model
  if(apply_rules){
    #compute initial base model
    message("...")
    base_model <- modelfunc(Store_list_df, constants_df, LSOA_demand_surface_df, Drive_time_matrix_adf, output_csv=FALSE, output_spec=NULL, scaling_factor=scaling_factor)
    
    #get list of stores which follow rules
    Store_list_df <- applyStoreRules(Store_list_df, base_model$Gravity_model_store_predictions, min_turnover=apply_min_turnover, store=apply_store_filter)
    rm(base_model)
    
    #recompute base model with new store list
    message("Calculating store demand...")
    base_model <- modelfunc(Store_list_df, constants_df, LSOA_demand_surface_df, Drive_time_matrix_adf, output_csv=output_csv, output_spec=output_spec, scaling_factor=scaling_factor)
  } else {
    message("Calculating store demand...")
    base_model <- modelfunc(Store_list_df, constants_df, LSOA_demand_surface_df, Drive_time_matrix_adf, output_csv=output_csv, output_spec=output_spec, scaling_factor=scaling_factor)
    
  }
  message("Calculating store demand complete...")
  
  #Number of output stores
  n_stores <- nrow(base_model$Gravity_model_store_predictions)
  
  # Output the number of unique LSOAs, and OAs in scope
  message(n_stores, " stores in scope")
  
  return(base_model)
}

preprocess_optimisation <- function(input_file_path, use_distance_estimate_measure, apply_rules, apply_min_turnover, apply_store_filter, scaling_factor){
  
  ##Load all input files
  message("Loading input files...")
  location_hierarchy <- "Output_Area_to_Lower_Layer_Super_Output_Area_to_Middle_Layer_Super_Output_Area_to_Local_Authority_District_December_2017_Lookup_in_G.csv"
  LSOA_demand_surface <- "LSOA Demand Surface.csv"
  LSOA_centroids_NE <- "Lower_Layer_Super_Output_Areas_December_2011_Population_Weighted_Centroids.csv"
  OA_centroids_NE <- "Output_Areas_December_2011_Population_Weighted_Centroids.csv"
  Store_list <- "StoreList.csv"
  Store_centroids <- "StoreList LatLongs.csv"
  LSOA_land_area <- "SAM_LSOA_DEC_2011_EW.csv"
  OA_land_area <- "SAM_OA_DEC_2011_EW.csv"
  Drivetime_matrix_a <- "Drive_time_matrix_adf.rds"
  constants <- "Constants.csv"
  drivetime_list <- "drivetime_list.rds"
  drivetime_list_gen <- "drivetime_list_gen.rds"
  ten_mon_footfall_poly <- "UK10mon_footfall_100m_polygons.shp"
  ten_mon_footfall_count <- "UK10mon_footfall_100m_counts.csv"
  ten_mon_footfall_OAs <- "Footfall by Weight OA.csv"
  london_poly <- "M25.shp"
  competition_restaurants <- "Restaurant Locations.csv"
  oa_pop_counts <- "OA - Population Counts.csv"
  decile_weights <- "decile_weights.csv"
  class_bandings <- "class_bandings.csv"
  
  location_hierarchy <- paste(input_file_path,location_hierarchy, sep="")
  LSOA_demand_surface <- paste(input_file_path,LSOA_demand_surface, sep="")
  LSOA_centroids_NE <- paste(input_file_path,LSOA_centroids_NE, sep="")
  OA_centroids_NE <- paste(input_file_path,OA_centroids_NE, sep="")
  Store_list <- paste(input_file_path,Store_list, sep="")
  Store_centroids <- paste(input_file_path,Store_centroids, sep="")
  LSOA_land_area <- paste(input_file_path,LSOA_land_area, sep="")
  OA_land_area <- paste(input_file_path,OA_land_area, sep="")
  Drivetime_matrix_a <- paste(input_file_path,Drivetime_matrix_a, sep="")
  constants <- paste(input_file_path,constants, sep="")
  drivetime_list <- paste(input_file_path,drivetime_list, sep="")
  drivetime_list_gen <- paste(input_file_path,drivetime_list_gen, sep="")
  ten_mon_footfall_poly <- paste(input_file_path,ten_mon_footfall_poly, sep="")
  ten_mon_footfall_count <- paste(input_file_path,ten_mon_footfall_count, sep="")
  ten_mon_footfall_OAs <- paste(input_file_path,ten_mon_footfall_OAs, sep="")
  london_poly <- paste(input_file_path,london_poly, sep="")
  competition_restaurants <- paste(input_file_path,competition_restaurants, sep="")
  oa_pop_counts <- paste(input_file_path,oa_pop_counts, sep="")
  decile_weights <- paste(input_file_path,decile_weights, sep="")
  class_bandings <- paste(input_file_path,class_bandings, sep="")
  
  error <- FALSE
  
  if(!file.exists(location_hierarchy)){
    message('Missing Location hierarchy input data file!')
    error <- TRUE
  }
  
  if(!file.exists(LSOA_demand_surface)){
    message('Missing LSOA demand surface input data file!')
    error <- TRUE
  }
  
  if(!file.exists(LSOA_centroids_NE)){
    message('Missing LSOA centroids input data file!')
    error <- TRUE
  }
  
  if(!file.exists(OA_centroids_NE)){
    message('Missing OA centroids input data file!')
    error <- TRUE
  }
  
  if(!file.exists(Store_list)){
    message('Missing Store List input data file!')
    error <- TRUE
  }
  
  if(!file.exists(Store_centroids)){
    message('Missing Store centroids input data file!')
    error <- TRUE
  }
  
  if(!file.exists(LSOA_land_area)){
    if (use_distance_estimate_measure){
      
      message('Missing LSOA land area input data file!')
      error <- TRUE
    }
  }
  
  if(!file.exists(OA_land_area)){
    if (use_distance_estimate_measure){
      
      message('Missing OA land area input data file!')
      error <- TRUE
    }
  }
  
  if(!file.exists(Drivetime_matrix_a)){
    message('Missing Drivetime matrix input data file!')
    error <- TRUE
  }
  
  if(!file.exists(constants)){
    message('Missing constants input data file!')
    error <- TRUE
  }
  
  if(!file.exists(drivetime_list)){
    message('Missing drive time list for optimisation input data file!')
    error <- TRUE
  }
  
  if(!file.exists(drivetime_list_gen)){
    message('Missing drive time list for generic location input data file!')
    error <- TRUE
  }
  
  if(!file.exists(ten_mon_footfall_poly)){
    message('Missing ten month polygon footfall input data file!')
    error <- TRUE
  }
  
  if(!file.exists(ten_mon_footfall_count)){
    message('Missing ten month footfall count input data file!')
    error <- TRUE
  }
  
  if(!file.exists(ten_mon_footfall_OAs)){
    message('Missing ten month OA footfall count input data file!')
    error <- TRUE
  }
  
  if(!file.exists(london_poly)){
    message('Missing London polygon shape input data file!')
    error <- TRUE
  }
  
  if(!file.exists(competition_restaurants)){
    message('Missing competition restaurants input data file!')
    error <- TRUE
  }
  
  if(!file.exists(oa_pop_counts)){
    message('Missing OA population counts input data file!')
    error <- TRUE
  }
  
  if(!file.exists(decile_weights)){
    message('Missing decile weights input data file!')
    error <- TRUE
  }
  
  if(!file.exists(class_bandings)){
    message('Missing class bandings input data file!')
    error <- TRUE
  }
  
  if(error){
    stop('Terminating optimisation')
  }
  
  # read csv input file
  location_hierarchy_df <- read.csv(file = location_hierarchy, fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  LSOA_demand_surface_df <- read.csv(file = LSOA_demand_surface, fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  LSOA_centroids_NE_df <- read.csv(file = LSOA_centroids_NE, fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  OA_centroids_NE_df <- read.csv(file = OA_centroids_NE, fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  Store_list_df <- read.csv(file = Store_list, fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  Store_centroids_df <- read.csv(file = Store_centroids, fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  
  if (use_distance_estimate_measure){
    
    LSOA_land_area_df <- read.csv(file = LSOA_land_area, fileEncoding="latin1", stringsAsFactors = FALSE)
    OA_land_area_df <- read.csv(file = OA_land_area, fileEncoding="latin1", stringsAsFactors = FALSE)
  } else {
    
    LSOA_land_area_df <- NULL
    OA_land_area_df <- NULL
  }
  
  Drive_time_matrix_adf <- readRDS(file = Drivetime_matrix_a)
  Drive_time_list_df <- readRDS(file = drivetime_list)
  Drive_time_list_gen_df <- readRDS(file = drivetime_list_gen)
  constants_df <- read.csv(file = constants, fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  
  s.ten_mon_footfall_poly_sf <- st_read(ten_mon_footfall_poly)
  s.london_poly_df <- st_read(london_poly)
  ten_mon_footfall_count_df <- fread(ten_mon_footfall_count)
  ten_mon_footfall_OAs_df <- read.csv(file = ten_mon_footfall_OAs, fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  OA_centroids_NE_df <- read.csv(file = OA_centroids_NE, fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  competition_restaurants_df <- read.csv(file = competition_restaurants, fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  oa_pop_counts_df <- read.csv(file = oa_pop_counts, fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  decile_weights_df <- read.csv(file = decile_weights, fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  class_bandings_df <- read.csv(file = class_bandings, fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  
  message("Loading input files complete...")
  
  
  #Decrypt data - todo for future
  
  
  #Preprocess data into correct format
  message("Pre-processing input files...")
  
  message("Preprocessing location hierarchy...")
  location_hierarchy_df <- subset(location_hierarchy_df, select=c("OA11CD", "LSOA11CD", "RGN11CD", "RGN11NM"))
  location_hierarchy_df <- plyr::rename(location_hierarchy_df, c("OA11CD"="OA", "LSOA11CD"="LSOA", "RGN11CD"="RGN", "RGN11NM"="RGN_NAME"))
  
  message("Preprocessing LSOA demand surface...")
  LSOA_demand_surface_df <- subset(LSOA_demand_surface_df, select=c("geography_id", "Demand_value"))
  LSOA_demand_surface_df <- plyr::rename(LSOA_demand_surface_df, c("geography_id"="LSOA", "Demand_value"="DEMAND"))
  LSOA_demand_surface_df <- LSOA_demand_surface_df[!(duplicated(LSOA_demand_surface_df)), ]
  
  message("Preprocessing LSOA population weighted centroids...")
  LSOA_centroids_NE_df <- subset(LSOA_centroids_NE_df, select=c("X", "Y", "lsoa11cd"))
  LSOA_centroids_NE_df <- plyr::rename(LSOA_centroids_NE_df, c("lsoa11cd"="LSOA"))
  LSOA_centroids_df <- batchConvert(LSOA_centroids_NE_df, "LSOA")
  LSOA_centroids_df <- plyr::rename(LSOA_centroids_df, c("Lat"="lat", "Long"="long"))
  LSOA_centroids_df$LSOA <- as.character(LSOA_centroids_df$LSOA)
  LSOA_centroids_df <- LSOA_centroids_df[!(duplicated(LSOA_centroids_df)), ]
  
  message("Preprocessing OA population weighted centroids...")
  OA_centroids_NE_df <- subset(OA_centroids_NE_df, select=c("X", "Y", "oa11cd"))
  OA_centroids_NE_df <- plyr::rename(OA_centroids_NE_df, c("oa11cd"="OA"))
  OA_centroids_df <- batchConvert(OA_centroids_NE_df, "OA")
  OA_centroids_df <- plyr::rename(OA_centroids_df, c("Lat"="lat", "Long"="long"))
  OA_centroids_df$OA <- as.character(OA_centroids_df$OA)
  OA_centroids_df <- OA_centroids_df[!(duplicated(OA_centroids_df)), ]
  
  message("Preprocessing existing store centroids...")
  Store_centroids_df <- subset(Store_centroids_df, select=c("Store_id", "Lat", "Long"))
  Store_centroids_df <- plyr::rename(Store_centroids_df, c("Store_id"="store", "Lat"="lat", "Long"="long"))
  Store_centroids_df$long <- as.numeric(as.character(Store_centroids_df$long))
  Store_centroids_df$lat <- as.numeric(as.character(Store_centroids_df$lat))
  Store_centroids_df <- Store_centroids_df[!(duplicated(Store_centroids_df)), ]

  message("Preprocessing store list...")
  Store_list_df <- plyr::rename(Store_list_df, c("Store_id"="store"))
  Store_list_df <- Store_list_df[!(duplicated(Store_list_df)), ]
  
  message("Preprocessing drive time list...")
  Drive_time_list_df <- plyr::rename(Drive_time_list_df, c("DriveTime"="drive_time"))
  Drive_time_list_gen_df <- plyr::rename(Drive_time_list_gen_df, c("DriveTime"="drive_time"))
  
  ## aggregate footfall and merge
  message("Preprocessing aggregate footfall data...")
  ten_mon_footfall_count_agg_df <- subset(ten_mon_footfall_count_df, select=c("i", "j", "uniq_users"))
  ten_mon_footfall_count_agg_df <- setDT(ten_mon_footfall_count_agg_df)[, lapply(.SD,sum), by=.(i,j)]
  ten_mon_footfall_poly_df = merge(s.ten_mon_footfall_poly_sf, ten_mon_footfall_count_agg_df, by.x=c("i", "j"), by.y=c("i", "j"))
  ten_mon_footfall_poly_df <- subset(ten_mon_footfall_poly_df, select=c("uniq_users", "geometry"))
  
  
  ## preprocess OA footfall
  message("Preprocessing OA footfall data...")
  ten_mon_footfall_OAs_df <- subset(ten_mon_footfall_OAs_df, select=c("Sum_uniq_users", "OA"))
  ten_mon_footfall_OAs_df <- plyr::rename(ten_mon_footfall_OAs_df, c("Sum_uniq_users"="uniq_users"))
  ten_mon_footfall_OAs_df <- ten_mon_footfall_OAs_df[!(duplicated(ten_mon_footfall_OAs_df)), ]
  
  ## preprocess competition
  message("Preprocessing competition restaurant data...")
  competition_restaurants_df <- subset(competition_restaurants_df, select=c("Retail.Fascia", "Retail.Activity", "Lat", "Long", "Retail.Fspace.sq.Ft"))
  competition_restaurants_df <- plyr::rename(competition_restaurants_df, c("Retail.Fascia"="store brand", "Retail.Activity"="store type", "Lat"="lat", "Long"="long", "Retail.Fspace.sq.Ft"="floorspace"))
  competition_restaurants_df$long <- as.numeric(competition_restaurants_df$long)
  competition_restaurants_df$lat <- as.numeric(competition_restaurants_df$lat)
  competition_restaurants_df$floorspace <- as.numeric(competition_restaurants_df$floorspace)
  competition_restaurants_df <- competition_restaurants_df[!(duplicated(competition_restaurants_df)), ]
  
  #preprocess classes for each OA
  message("Preprocessing classes for each OA...")
  oa_dec_df = mutate(oa_pop_counts_df, 
                     decile_rank_1 = ntile(oa_pop_counts_df$Sum_1km_Population,10),
                     decile_rank_2 = ntile(oa_pop_counts_df$Sum_2km_Population,10),
                     decile_rank_5 = ntile(oa_pop_counts_df$Sum_5km_Population,10),
                     decile_rank_10 = ntile(oa_pop_counts_df$Sum_10km_Population,10),
                     decile_rank_20 = ntile(oa_pop_counts_df$Sum_20km_Population,10))
  
  decile_weights_1 = decile_weights_df[which(decile_weights_df$Decile == "Sum_1km_Population"), ]$Weights
  decile_weights_2 = decile_weights_df[which(decile_weights_df$Decile == "Sum_2km_Population"), ]$Weights
  decile_weights_5 = decile_weights_df[which(decile_weights_df$Decile == "Sum_5km_Population"), ]$Weights
  decile_weights_10 = decile_weights_df[which(decile_weights_df$Decile == "Sum_10km_Population"), ]$Weights
  decile_weights_20 = decile_weights_df[which(decile_weights_df$Decile == "Sum_20km_Population"), ]$Weights
  
  oa_dec_df$Decile_score <- (oa_dec_df$decile_rank_1*decile_weights_1+
                               oa_dec_df$decile_rank_2*decile_weights_2+
                               oa_dec_df$decile_rank_5*decile_weights_5+
                               oa_dec_df$decile_rank_10*decile_weights_10+
                               oa_dec_df$decile_rank_20*decile_weights_20)/10
  
  oa_class_df <- mutate(oa_dec_df, decile_score_perc_rank = ntile(oa_dec_df$Decile_score,100))
  oa_class_df$decile_score_perc_rank <- round(oa_class_df$decile_score_perc_rank, -1)
  oa_class_df$decile_score_perc_rank <- oa_class_df$decile_score_perc_rank/100
  oa_class_df <- merge(oa_class_df, class_bandings_df, by.x="decile_score_perc_rank", by.y="percent_rank")
  oa_class_df <- subset(oa_class_df, select=c("Key", "class"))
  oa_class_df <- plyr::rename(oa_class_df, c("Key"="OA"))
  
  if (use_distance_estimate_measure){
    message("Preprocessing LSOA and OA land area...")
    
    LSOA_land_area_df <- subset(LSOA_land_area_df, select=c("LSOA11CD", "AREAEHECT"))
    LSOA_land_area_df <- plyr::rename(LSOA_land_area_df, c("LSOA11CD"="LSOA", "AREAEHECT"="land_area"))
    LSOA_land_area_df <- LSOA_land_area_df[!(duplicated(LSOA_land_area_df)), ]
    
    
    OA_land_area_df <- subset(OA_land_area_df, select=c("OA11CD", "AREAEHECT"))
    OA_land_area_df <- plyr::rename(OA_land_area_df, c("OA11CD"="OA", "AREAEHECT"="land_area"))
    OA_land_area_df <- OA_land_area_df[!(duplicated(OA_land_area_df)), ]
  }
  
  if (apply_rules){
    message("...")
    #Build base model and apply store rules which do not meet the criteria
    base_model <- modelfunc(Store_list_df, constants_df, LSOA_demand_surface_df, Drive_time_matrix_adf, output_csv=FALSE, output_spec=NULL, scaling_factor=scaling_factor)
    Store_list_df <- applyStoreRules(Store_list_df, base_model$Gravity_model_store_predictions, apply_min_turnover, apply_store_filter)
    rm(base_model) # save memory
  }
  
  # merge datasets to find inscope OA, LSOA and store list
  #Get the overlapping set (inner join) of all in-scope locations
  location_hierarchy_df <- subset(location_hierarchy_df, LSOA %in% LSOA_demand_surface_df$LSOA)
  
  #Subset with drivetime matrix
  location_hierarchy_df <- subset(location_hierarchy_df, LSOA %in% rownames(Drive_time_matrix_adf))
  location_hierarchy_df <- subset(location_hierarchy_df, LSOA %in% LSOA_centroids_df$LSOA)
  location_hierarchy_df <- subset(location_hierarchy_df, OA %in% OA_centroids_df$OA)
  location_hierarchy_df <- subset(location_hierarchy_df, OA %in% ten_mon_footfall_OAs_df$OA)
  location_hierarchy_df <- subset(location_hierarchy_df, OA %in% oa_class_df$OA)
  
  Store_list_df <- subset(Store_list_df, store %in% colnames(Drive_time_matrix_adf))
  Store_list_df <- subset(Store_list_df, store %in% Store_centroids_df$store)
  Store_centroids_df <- subset(Store_centroids_df, store %in% Store_list_df$store)
  
  if (use_distance_estimate_measure){
    
    location_hierarchy_df <- subset(location_hierarchy_df, LSOA %in% LSOA_land_area_df$LSOA)
    location_hierarchy_df <- subset(location_hierarchy_df, OA %in% OA_land_area_df$OA)
    
  }
  
  message("Pre-processing input files complete...")
  
  
  # Output the number of unique LSOAs, and OAs in scope
  message(length(unique(location_hierarchy_df$LSOA)), " unique LSOAs, ", 
          length(unique(location_hierarchy_df$OA)), " unique OAs and ",
          length(unique(Store_list_df$store)), " stores in scope")
  
  
  LSOA_demand_surface_df <- subset(LSOA_demand_surface_df, LSOA %in% location_hierarchy_df$LSOA)
  LSOA_centroids_df <- subset(LSOA_centroids_df, LSOA %in% location_hierarchy_df$LSOA)
  OA_centroids_df <- subset(OA_centroids_df, OA %in% location_hierarchy_df$OA)
  ten_mon_footfall_OAs_df <- subset(ten_mon_footfall_OAs_df, OA %in% location_hierarchy_df$OA)
  oa_class_df <- subset(oa_class_df, OA %in% location_hierarchy_df$OA)
  
  if (use_distance_estimate_measure){
    
    LSOA_land_area_df <- subset(LSOA_land_area_df, LSOA %in% location_hierarchy_df$LSOA)
    OA_land_area_df <- subset(OA_land_area_df, OA %in% location_hierarchy_df$OA)
    
  }
  
  ## Must keep this output structure for the optimisation!!
  retList <- list("location_hierarchy_df" = location_hierarchy_df,
                  "LSOA_demand_surface_df" = LSOA_demand_surface_df,
                  "LSOA_centroids_df" = LSOA_centroids_df,
                  "OA_centroids_df" = OA_centroids_df,
                  "Store_list_df" = Store_list_df,
                  "Store_centroids_df" = Store_centroids_df,
                  "LSOA_land_area_df" = LSOA_land_area_df,
                  "OA_land_area_df" = OA_land_area_df,
                  "Drive_time_matrix_adf" = Drive_time_matrix_adf,
                  "constants_df"= constants_df,
                  "Drive_time_list_df"=Drive_time_list_df,
                  "Drive_time_list_gen_df"=Drive_time_list_gen_df,
                  "competition_restaurants_df"= competition_restaurants_df, 
                  "ten_mon_footfall_poly_df"= ten_mon_footfall_poly_df, 
                  "ten_mon_footfall_OAs_df" = ten_mon_footfall_OAs_df, 
                  "s.london_poly_df" = s.london_poly_df,
                  "oa_class_df"= oa_class_df)
  
  return(retList)
  
}