##Load all packages
if (!require('geosphere')) install.packages('geosphere'); library('geosphere')
if (!require('data.table')) install.packages('data.table'); library('data.table')
if (!require('osrm')) install.packages('osrm'); library('osrm')
if (!require('sf')) install.packages('sf'); library('sf')
if (!require('sp')) install.packages('sp'); library('sp')
if (!require('rgeos')) install.packages('rgeos'); library('rgeos')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')


source("preprocessing.R")
source("attractivenessFunc.R")

options(osrm.server = "http://router.project-osrm.org/")

## Functions to build metadata for each location (vertex)
build_vertex_metadata <- function(location_hierarchy_df, centroids_LSOA_df,
                                  centroids_OA_df, land_area_LSOA_df,
                                  land_area_OA_df, use_distance_estimate_measure, 
                                  max_dist_LSOA_km,
                                  max_dist_OA_km,
                                  max_dist_store_LSOA_km,
                                  min_demand){
  
  #Build the vertex metadata
  vertex_metadata_LSOA_df <- data.frame(VERTEX=unique(location_hierarchy_df$LSOA),
                                        HIERARCHY="LSOA",
                                        MIN_UB=min_demand,
                                        MAX_LSOA_DIST_KM=max_dist_LSOA_km)
  
  vertex_metadata_LSOA_df <- merge(x=vertex_metadata_LSOA_df, y=centroids_LSOA_df, 
                                   by.x="VERTEX", by.y = "LSOA")
  
  if (use_distance_estimate_measure){
    vertex_metadata_LSOA_df <- merge(x=vertex_metadata_LSOA_df, y=land_area_LSOA_df, 
                                     by.x="VERTEX", by.y = "LSOA")
    
  } else {
    vertex_metadata_LSOA_df$land_area <- NA
    
  }
  vertex_metadata_LSOA_df$STORE_LSOA_DIST_KM <- max_dist_store_LSOA_km
  
  setnames(vertex_metadata_LSOA_df, old = c('land_area','long', 'lat'), 
           new = c('LANDAREA','CENTROID_LONG', 'CENTROID_LAT'))
  
  vertex_metadata_OA_df <- data.frame(VERTEX=unique(location_hierarchy_df$OA),
                                      HIERARCHY='OA',
                                      MIN_UB=min_demand,
                                      MAX_LSOA_DIST_KM=max_dist_OA_km)
  
  vertex_metadata_OA_df <- merge(x=vertex_metadata_OA_df, y=centroids_OA_df, 
                                 by.x="VERTEX", by.y = "OA")
  
  if (use_distance_estimate_measure){
    
    vertex_metadata_OA_df <- merge(x=vertex_metadata_OA_df, y=land_area_OA_df, 
                                   by.x="VERTEX", by.y = "OA")
    
  } else {
    
    vertex_metadata_OA_df$land_area <- NA
  }
  
  
  #Add LSOA to OA metadata
  vertex_metadata_OA_df <- merge(x=vertex_metadata_OA_df, y=location_hierarchy_df, 
                                 by.x="VERTEX", by.y = "OA")
  
  setnames(vertex_metadata_OA_df, old = c('land_area','long', 'lat'), 
           new = c('LANDAREA','CENTROID_LONG', 'CENTROID_LAT'))
  

  
  #Add hierarchy
  vertex_metadata_df <- list("vertex_metadata_LSOA_df" = vertex_metadata_LSOA_df, "vertex_metadata_OA_df" = vertex_metadata_OA_df)
  
  return(vertex_metadata_df)
}


update_upper_bound_LSOA <- function(MAX_LSOA_DIST_KM, LANDAREA, CENTROID_LONG, CENTROID_LAT, 
                                    LSOA_demand_surface_df, LSOA_centroids_df, min_demand, use_distance_estimate_measure){
  
  # function to calculate the upper bound for each LSOA vertex 
  # get the metadata for the vertex (hierarchy) and apply rules
  
  # set the upper bound to be the total demand surface of all LSOAs within max LSOA dist of LSOA centroid * 1/LandDistanceLSOA
  origin_centroids_df <- data.frame("long" = CENTROID_LONG, "lat" = CENTROID_LAT)
  
  # calculate a list of all LSOAs that are 'maxDist' away from centroid lat/ long (will include current LSOA)
  LSOAs <- calculateClosest(origin_centroids_df, LSOA_centroids_df, MAX_LSOA_DIST_KM)
  LSOAs <- LSOAs[["LSOA"]]
  
  # calculate the upper bound measure of demand attractiveness (total demand surface for list of LSOAs)
  LSOA_demand_surface_filtered_df <- LSOA_demand_surface_df[LSOA_demand_surface_df$LSOA %in% LSOAs, ]
  upper_bound <- sum(LSOA_demand_surface_filtered_df$DEMAND)
  
  if (upper_bound<min_demand){
    return(0)
    
  }
  
  if (use_distance_estimate_measure){
    # calculate the upper bound measure of distance (no. of LSOAs in hierarchy/ land area)
    no_LSOAs_in_LSOA <- 1
    
    upper_bound <- upper_bound*(no_LSOAs_in_LSOA/LANDAREA)
    
  }
  return(upper_bound)
  
}

update_lower_bound_LSOA <- function(LSOA, max_dist_store_LSOA_km, total_area_LSOA, CENTROID_LONG, CENTROID_LAT, 
                                    LSOA_demand_surface_df, LSOA_centroids_df, existing_store_centroid_df,
                                    use_distance_estimate_measure){
  
  # set the lower bound to be the LSOA with the min demand surface/ no. of existing stores in LAD * 1/LandArea(MinLSOA)
  # find the list of LSOAs in the LAD
  LSOA_demand_surface_filtered_df <- LSOA_demand_surface_df[LSOA_demand_surface_df$LSOA %in% LSOA, ]
  
  # find the min demand and the LSOAs with the min demand
  min_demand <- min(LSOA_demand_surface_filtered_df$DEMAND)
  
  # get the number of existing stores in LADs
  # calculate a list of all stores that are 'maxDist' away from LSOA centroid lat/ long
  # set the upper bound to be the total demand surface of all LSOAs within max LSOA dist of LSOA centroid * 1/LandDistanceLSOA
  origin_centroids_df <- data.frame("long" = CENTROID_LONG, "lat" = CENTROID_LAT)
  stores <- calculateClosest(origin_centroids_df, existing_store_centroid_df, max_dist_store_LSOA_km)
  
  n_existing_stores <- nrow(na.omit(stores))
  
  # get the max land distance of the min LSOAs
  total_area_LSOA_def <- 1
  total_area_LSOA <- max(total_area_LSOA_def, total_area_LSOA)
  
  
  # compute the lower bound
  lower_bound <- (min_demand/(n_existing_stores+1))
  
  if (use_distance_estimate_measure){
    # calculate the lower bound measure of distance (no. of LSOAs in hierarchy/ land area)
    lower_bound <- lower_bound*(1/total_area_LSOA) 
    
  }
  
  return(lower_bound)
  
}

prune <- function(max_UB, max_LB, UB, LB, min_demand){
  
  if(UB<max_LB){
    return(FALSE)
    
  } 
  return(TRUE) 
}


calculateAttractivenessAndClass <- function(store_name, store_lat, store_long, OA="NA",
                                            attractiveness_fun_arg){
  
  using_opt <- attractiveness_fun_arg$using_opt
  retail_centre_type <- attractiveness_fun_arg$retail_centre_type
  competition_restaurants_df <- attractiveness_fun_arg$competition_restaurants_df
  ten_mon_footfall_poly_df <- attractiveness_fun_arg$ten_mon_footfall_poly_df
  ten_mon_footfall_OAs_df <- attractiveness_fun_arg$ten_mon_footfall_OAs_df
  s.london_poly_df <- attractiveness_fun_arg$s.london_poly_df
  competition_distance_KM <- attractiveness_fun_arg$competition_distance_KM
  oa_class_df <- attractiveness_fun_arg$oa_class_df
  OA_centroids_df <- attractiveness_fun_arg$OA_centroids_df
  store_size <- attractiveness_fun_arg$store_size
  
  #Build dataframe of origin centroids
  origin_centroids_df <- data.frame("long" = store_long, "lat" = store_lat, "store"=store_name)
  
  
  
  #Check if origin centroid is in london
  if(!is.null(evaluate_polygon(polygon_df=s.london_poly_df, store_name=store_name, 
                               store_lat=store_lat, store_long=store_long)))
  {
    
    message("Location to be evaluated is in london")
    in_london <- TRUE
    
  } else {
    
    message("Location to be evaluated is not in london")
    in_london <- FALSE
  }
  
  #calculate footfall and class
  if (using_opt){
    
    #lookup footfall from ten_mon_footfall_OAs_df
    res_OA <- ten_mon_footfall_OAs_df[ten_mon_footfall_OAs_df$OA == OA,]
    
    if (nrow(res_OA) != 1){
      
      message('No. of OAs associated with input OA is ',nrow(res_OA))
      stop('Cannot evaluate footfall for OA location! Terminating optimisation')
    } 
    
    footfall <- res_OA$uniq_users
    message("Footfall calculated to be ", footfall)
    
    #lookup class from OA class list
    class <- oa_class_df[oa_class_df$OA == OA,]$class
    
  } else {
    
    #calculate footfall based on polygon
    res_poly<-evaluate_polygon(polygon_df=ten_mon_footfall_poly_df, store_name=store_name, 
                               store_lat=store_lat, store_long=store_long)
    
    if(is.null(res_poly)){
      
      stop('Cannot evaluate footfall for store location! Terminating optimisation')
      
    } 
    
    footfall <- res_poly$uniq_users
    message("Footfall calculated to be ", footfall)
    
    #calculate class based on nearest OA
    nearest_class <- nearestPoint(origin_centroids_df, OA_centroids_df)
    class <- oa_class_df[oa_class_df$OA == nearest_class$OA,]$class
    
  }
  
  #calculate no. of competition stores in 1km bbox
  stores <- calculateClosest(origin_centroids_df, competition_restaurants_df, competition_distance_KM)
  n_existing_stores <- nrow(na.omit(stores))
  message("Number of competition stores calculated to be ", n_existing_stores)
  
  #calculate competition square feet in 1km bbox
  square_feet <- sum(na.omit(stores)$floorspace)
  message("Competition square feet calculated to be ", square_feet)
  
  
  #calculate attractiveness
  x <- attractiveness_new_store(store_name, retail_centre_type=retail_centre_type, footfall, 
                                competition_one_km=n_existing_stores, 
                                competition_sq_feet_one_km=square_feet,
                                in_london=in_london, class=class, store_size)
  
  return(x)
}


# initialise the workspace
initOptimisation_s1 <- function(location_hierarchy_df, 
                             LSOA_demand_surface_df,
                             centroids_LSOA_df,
                             centroids_OA_df,
                             existing_store_centroid_df,
                             land_area_LSOA_df=NULL,
                             land_area_OA_df=NULL,
                             use_distance_estimate_measure=FALSE,
                             max_dist_LSOA_km=50,
                             max_dist_OA_km=25,
                             max_dist_store_LSOA_km=30,
                             min_demand=0){
  
  
  #Build vertex metadata
  df.g <- build_vertex_metadata(location_hierarchy_df, centroids_LSOA_df,
                                centroids_OA_df, land_area_LSOA_df,
                                land_area_OA_df, use_distance_estimate_measure, 
                                max_dist_LSOA_km,
                                max_dist_OA_km,
                                max_dist_store_LSOA_km,
                                min_demand)
  
  #Calculate upper and lower bounds for all LSOAs
  #Loop through LSOAs and calculate upper bound
  #However, to speed things up we consider vectorising 
  df.g$vertex_metadata_LSOA_df[["UB"]] <- apply(df.g$vertex_metadata_LSOA_df[,c(3, 4, 7, 5, 6)], 1, function(x)
    update_upper_bound_LSOA(x['MAX_LSOA_DIST_KM'], x['LANDAREA'], x['CENTROID_LONG'], x['CENTROID_LAT'],
                            LSOA_demand_surface_df, centroids_LSOA_df, x['MIN_UB'], use_distance_estimate_measure))
  
  df.g$vertex_metadata_LSOA_df[["LB"]] <- apply(df.g$vertex_metadata_LSOA_df[,c(1, 8, 7, 5, 6)], 1, function(x)
    update_lower_bound_LSOA(x['VERTEX'],as.numeric(x['STORE_LSOA_DIST_KM']), as.numeric(x['LANDAREA']), as.numeric(x['CENTROID_LONG']), as.numeric(x['CENTROID_LAT']),
                            LSOA_demand_surface_df, centroids_LSOA_df, existing_store_centroid_df, use_distance_estimate_measure))
  
  df.g$vertex_metadata_LSOA_df$AVG_B <- 0.5*(df.g$vertex_metadata_LSOA_df$UB + df.g$vertex_metadata_LSOA_df$LB)
  
  #Prune the locations
  max_UB <- max(df.g$vertex_metadata_LSOA_df$UB, na.rm = TRUE)
  max_LB <- max(df.g$vertex_metadata_LSOA_df$LB, na.rm = TRUE)
  df.g$vertex_metadata_LSOA_df[["PRUNE"]] <-  apply(df.g$vertex_metadata_LSOA_df[,c(8, 9)], 1, function(x)
    prune(max_UB, max_LB, x['UB'], x['LB'], min_demand))
  
  # Output the number of pruned LSOAs and OAs
  pruned_LSOAs <- dim(subset(df.g$vertex_metadata_LSOA_df, PRUNE==FALSE))[1] 
  remaining_LSOAs <- dim(subset(df.g$vertex_metadata_LSOA_df, PRUNE==TRUE))[1] 
  
  df.g$vertex_metadata_LSOA_df <- subset(df.g$vertex_metadata_LSOA_df, PRUNE==TRUE)
  df.g$vertex_metadata_OA_df <- subset(df.g$vertex_metadata_OA_df, LSOA %in% df.g$vertex_metadata_LSOA_df$VERTEX)
  
  remaining_OAs <- length(unique(df.g$vertex_metadata_OA_df$VERTEX))

    message(pruned_LSOAs, " pruned LSOAs, ", 
          remaining_LSOAs, " remaining LSOAs and ", 
          remaining_OAs, " remaining OAs after S1 optimisation!")
  
  return(df.g)
}

initOptimisation_s2 <- function(df.g, 
                                top_n_perc,
                                type='AVG' #Options are AVG, UB
                                ){
  
  if (type=="UB"){
    df.g$vertex_metadata_LSOA_df <- subset(df.g$vertex_metadata_LSOA_df, 
                                           UB > quantile(UB,prob = 1 - top_n_perc/100))
  } else if (type=="AVG"){
   
    df.g$vertex_metadata_LSOA_df <- subset(df.g$vertex_metadata_LSOA_df, 
                                           AVG_B > quantile(AVG_B,prob = 1 - top_n_perc/100)) 
  }
  
  # Output the number of resulting LSOAs and OAs
  df.g$vertex_metadata_OA_df <- subset(df.g$vertex_metadata_OA_df, LSOA %in% df.g$vertex_metadata_LSOA_df$VERTEX)
  
  remaining_LSOAs <- length(unique(df.g$vertex_metadata_LSOA_df$VERTEX))
  remaining_OAs <- length(unique(df.g$vertex_metadata_OA_df$VERTEX))
  
  message(remaining_LSOAs, " remaining LSOAs and ", 
          remaining_OAs, " remaining OAs after S2 optimisation!")
  
  return(df.g)
}

drivetime <- function(origin_long, origin_lat, destination_long, destination_lat){
  
  route2 <- osrmRoute(src = c("A", origin_long, origin_lat),
                      dst = c("B", destination_long, destination_lat),
                      returnclass = "sf")
  
  return(route2$duration)
}

calc_new_drive_time_matrix_and_store_list <- function(store_name, store_lat, store_long, LSOA_centroids_df,
                        existing_store_centroid_df, Drive_time_matrix_adf, 
                        Store_list_df, attractiveness_fun, attractiveness_fun_arg, 
                        constants_df, LSOA_demand_surface_df, max_LSOA_dist_km, 
                        use_drive_time_list=FALSE, Drive_time_list_df){
  
  OA <- store_name
  CENTROID_LAT <- store_lat
  CENTROID_LONG <- store_long
  origin_centroids_df <- data.frame("long" = CENTROID_LONG, "lat" = CENTROID_LAT)
  
  #get the closest stores within max distance
  stores <- calculateClosest(origin_centroids_df, existing_store_centroid_df, max_LSOA_dist_km)
  stores <- stores[["store"]]
  
  if (use_drive_time_list){
    
    #get the LSOAs from the drive time list
    LSOAs <- Drive_time_list_df[which(Drive_time_list_df$OA == OA), ]
    
  } else {
    
    # get the closest LSOAs within max distance
    LSOAs <- calculateClosest(origin_centroids_df, LSOA_centroids_df, max_LSOA_dist_km)
    
    #loop through all LSOAs and get the drivetime
    LSOAs[["drive_time"]] <- apply(LSOAs[,c(2,3)], 1, function(x)
      drivetime(CENTROID_LONG, CENTROID_LAT, x['long'], x['lat']))
    
    
  }
  
  #add a new store with default store id OA
  LSOAs_d <- subset(LSOAs, select=c("LSOA", "drive_time"))
  setnames(LSOAs_d, "drive_time", OA)
  Drive_time_matrix_adf_n<- merge(Drive_time_matrix_adf, LSOAs_d, by.x="row.names", by.y="LSOA", all.x=TRUE)
  rownames(Drive_time_matrix_adf_n) <- Drive_time_matrix_adf_n$Row.names
  Drive_time_matrix_adf_n$Row.names <- NULL
  
  #convert dataframe back to matrix
  Drive_time_matrix_adf_n <- data.matrix(Drive_time_matrix_adf_n)
  
  #add new store to store list
  n_store_df <- attractiveness_fun(OA, CENTROID_LAT, CENTROID_LONG, OA, attractiveness_fun_arg)
  Store_list_df_n <- rbind(Store_list_df, n_store_df)
  
  #add new store centroid to centroid list
  n_centroid_df <- data.frame("store" = c(store_name), 
                  "lat" = c(store_lat), 
                  "long" = c(store_long))
  existing_store_centroid_df_n <- rbind(existing_store_centroid_df, n_centroid_df)
  
  #return the new drive time matrix and store list, and existing store centroid
  retList <- list("Drive_time_matrix_adf_n" = Drive_time_matrix_adf_n,
                  "Store_list_df_n" = Store_list_df_n, 
                  "existing_store_centroid_df_n" = existing_store_centroid_df_n)
  return(retList)
}


calc_demand <- function(store_name, store_lat, store_long, LSOA_centroids_df,
                        existing_store_centroid_df, Drive_time_matrix_adf, 
                        Store_list_df, attractiveness_fun, attractiveness_fun_arg, 
                        constants_df, LSOA_demand_surface_df, max_LSOA_dist_km, 
                        use_drive_time_list=FALSE, Drive_time_list_df){
  
  OA <- store_name
  CENTROID_LAT <- store_lat
  CENTROID_LONG <- store_long
  origin_centroids_df <- data.frame("long" = CENTROID_LONG, "lat" = CENTROID_LAT)

  #get the closest stores within max distance
  stores <- calculateClosest(origin_centroids_df, existing_store_centroid_df, max_LSOA_dist_km)
  stores <- stores[["store"]]
  
  if (use_drive_time_list){
    
    #get the LSOAs from the drive time list
    LSOAs <- Drive_time_list_df[which(Drive_time_list_df$OA == OA), ]
    
    if(dim(LSOAs)[1] == 0){
      
      message("Missing OA to LSOA drive time in drive time list!")
      message("Returning 0 store prediction!")
      s_pred <- data.frame("store" = c(store_name), 
                      "demand" = c(0), 
                      "lat" = c(CENTROID_LAT),
                      "long" = c(CENTROID_LONG))
      return(s_pred)
      #stop('Terminating optimisation')
      
    }

  } else {
    
    # get the closest LSOAs within max distance
    LSOAs <- calculateClosest(origin_centroids_df, LSOA_centroids_df, max_LSOA_dist_km)
    
    #loop through all LSOAs and get the drivetime
    LSOAs[["drive_time"]] <- apply(LSOAs[,c(2,3)], 1, function(x)
      drivetime(CENTROID_LONG, CENTROID_LAT, x['long'], x['lat']))
    
    
  }
  
  #add drivetime to the drivetime matrix
  #first, we remove all LSOAs in drive time matrix not in list of LSOAs
  #and we remove all stores in drive time matrix not in list of stores
  store_mat <- matrix(as.character(stores), ncol = 1)
  Drive_time_matrix_adf_n <- Drive_time_matrix_adf[, store_mat[, 1], drop=FALSE]
  
  LSOA_l <- LSOAs[["LSOA"]]
  LSOA_mat <- matrix(as.character(LSOA_l), ncol = 1)
  Drive_time_matrix_adf_n <- Drive_time_matrix_adf_n[LSOA_mat[, 1],, drop=FALSE]
  
  #remove all NA rows and cols from Drive time matrix
  Drive_time_matrix_adf_n <- Drive_time_matrix_adf_n[complete.cases(Drive_time_matrix_adf_n),
                                                     complete.cases(t(Drive_time_matrix_adf_n))]
  
  #add a new store with default store id OA
  LSOAs_d <- subset(LSOAs, select=c("LSOA", "drive_time"))
  setnames(LSOAs_d, "drive_time", OA)
  Drive_time_matrix_adf_n <- merge(Drive_time_matrix_adf_n, LSOAs_d, by.x="row.names", by.y="LSOA")
  rownames(Drive_time_matrix_adf_n) <- Drive_time_matrix_adf_n$Row.names
  Drive_time_matrix_adf_n$Row.names <- NULL
  
  #convert dataframe back to matrix
  Drive_time_matrix_adf_n <- data.matrix(Drive_time_matrix_adf_n)
  
  #add new store to store list
  n_store_df <- attractiveness_fun(OA, CENTROID_LAT, CENTROID_LONG, OA, attractiveness_fun_arg)
  Store_list_df_n <- rbind(Store_list_df, n_store_df)
  
  #run the model
  result <- modelfunc(Store_list_df_n, constants_df, LSOA_demand_surface_df, Drive_time_matrix_adf_n)
  s_pred <- result[["Gravity_model_store_predictions"]][which(result[["Gravity_model_store_predictions"]]$store %in% OA),]
  s_pred$lat <- CENTROID_LAT
  s_pred$long <- CENTROID_LONG
  return(s_pred)
}

initOptimisation_s3 <- function(df.g, 
                                type='greedy',
                                LSOA_centroids_df,
                                existing_store_centroid_df,
                                Drive_time_matrix_adf,
                                Store_list_df,
                                attractiveness_fun,
                                attractiveness_fun_arg,
                                constants_df,
                                LSOA_demand_surface_df,
                                max_LSOA_dist_km=10,
                                Drive_time_list_df,
                                top_stores=1,
                                apply_rules,
                                apply_min_turnover){
  
  if (type=="greedy"){
    
    
    #keep a counter of all stores which are added to top list
    t_stores <- data.frame("store" = character(0))
    
    #Loop through top_stores
    for (i in 1:top_stores){
      
      #keep a counter of model store demand predictions
      res_pred <- data.frame( "store" = character(0), "demand" = double(0), 
                              "lat" = double(0), "long" = double(0))
    
      #Loop through all OAs 
      for (row in 1:nrow(df.g$vertex_metadata_OA_df)) {
        
        
        message("Evaluating new row: ", row)
        
        #check if vertex is already in t_store list - continue if so
        if (df.g$vertex_metadata_OA_df[row, "VERTEX"] %in% t_stores$store){
          
          next
          
        }
        
        #calculate the store demand given the drive time list to 
        #return the OA and its gravity model store predictions only
        s_pred <- calc_demand(as.character(df.g$vertex_metadata_OA_df[row, "VERTEX"]), 
                              df.g$vertex_metadata_OA_df[row, "CENTROID_LAT"],
                              df.g$vertex_metadata_OA_df[row, "CENTROID_LONG"],
                              LSOA_centroids_df,
                              existing_store_centroid_df, Drive_time_matrix_adf, 
                              Store_list_df, attractiveness_fun, attractiveness_fun_arg, 
                              constants_df, LSOA_demand_surface_df, max_LSOA_dist_km, 
                              use_drive_time_list = TRUE, 
                              Drive_time_list_df=Drive_time_list_df)
        
        
        res_pred <- rbind(res_pred, s_pred)
      }
      
      #return top 1 stores
      res_pred <- res_pred[which.max(res_pred$demand),]
      
      #check if store meets min demand conditions
      if (apply_rules){
        
        if (res_pred$demand < apply_min_turnover){
          
          ## break from current loop
          message("Further stores in optimisation routine do not meet required demand!")
          break
          
        }
        
      }
      
      #add store to t_stores
      next_top_store <- data.frame("store" = as.character(res_pred$store))
      
      #calculate the full new store list and drive time matrix with this new store
      new_m <- calc_new_drive_time_matrix_and_store_list(as.character(res_pred$store), res_pred$lat, res_pred$long, LSOA_centroids_df,
                                                            existing_store_centroid_df, Drive_time_matrix_adf, 
                                                            Store_list_df, attractiveness_fun, attractiveness_fun_arg, 
                                                            constants_df, LSOA_demand_surface_df, max_LSOA_dist_km, 
                                                            use_drive_time_list=TRUE, Drive_time_list_df)
      
      Drive_time_matrix_adf <- new_m$Drive_time_matrix_adf_n
      Store_list_df <- new_m$Store_list_df_n
      existing_store_centroid_df <- new_m$existing_store_centroid_df_n
      
      t_stores <- rbind(t_stores, next_top_store)

    }
    
    #Once all optimal stores have been added, calculate the full demand matrix again
    result <- modelfunc(Store_list_df, constants_df, LSOA_demand_surface_df, Drive_time_matrix_adf)
    
  
  }
  
  #return the result
  return(result)
}

optimise <- function(input_file_path, 
                     use_distance_estimate_measure=FALSE,
                     max_dist_LSOA_km=25,
                     max_dist_store_LSOA_km=15,
                     min_demand=50000,
                     top_n_perc=10, 
                     type_S2='AVG',
                     type_S3='greedy',
                     apply_rules=FALSE, 
                     apply_min_turnover=15000, 
                     apply_store_filter=NA, 
                     competition_distance_KM=1,
                     max_LSOA_dist_km=5,
                     top_stores=1){
  
  ##Preprocess all data
  message("Preprocessing data...")
  preproc <- preprocess_optimisation(input_file_path, use_distance_estimate_measure, apply_rules, apply_min_turnover, apply_store_filter)
  message("Preprocessing data completed...")
  
  
  ##save all results for each region in a list
  overall_results <- list()
  
  ##Loop through unique regions
  message("Optimising new store locations in each region...")
  for(rgn in unique(preproc$location_hierarchy_df$RGN))
  {
    
    regional_hierarchy_df <- preproc$location_hierarchy_df[preproc$location_hierarchy_df$RGN == rgn,]
    
    # Output the number of unique LSOAs, and OAs in scope
    rgn_name <- unique(regional_hierarchy_df$RGN_NAME)[1]
    message("Running optimisation on region ",rgn_name)
    
    ## Lets find the 1st stage optimisation results
    message("Completing stage 1 of the optimisation...")
    
    s1_results <- initOptimisation_s1(location_hierarchy_df=regional_hierarchy_df, 
                                      LSOA_demand_surface_df=preproc$LSOA_demand_surface_df,
                                      centroids_LSOA_df=preproc$LSOA_centroids_df,
                                      centroids_OA_df=preproc$OA_centroids_df,
                                      existing_store_centroid_df=preproc$Store_centroids_df,
                                      land_area_LSOA_df=preproc$LSOA_land_area_df,
                                      land_area_OA_df=preproc$OA_land_area_df,
                                      use_distance_estimate_measure=use_distance_estimate_measure,
                                      max_dist_LSOA_km=max_dist_LSOA_km,
                                      max_dist_store_LSOA_km=max_dist_store_LSOA_km,
                                      min_demand=min_demand)
    
    #plot distribution of upper bound, lower bound and avg
    # Kernel Density Plot
    # ub_density <- density(s1_results$vertex_metadata_LSOA_df$UB) 
    # lb_density <- density(s1_results$vertex_metadata_LSOA_df$LB) 
    # avg_b_density <- density(s1_results$vertex_metadata_LSOA_df$AVG_B) 
    # plot(avg_b_density) # plots the results
    
    message("Stage 1 completed...")
    
    
    #Stage 2 optimisation
    message("Completing stage 2 of the optimisation...")
    s2_results <- initOptimisation_s2(s1_results,top_n_perc=top_n_perc,type=type_S2)
    message("Stage 2 completed...")
    
    
    
    #plot distribution of upper bound, lower bound and avg
    # Kernel Density Plot
    # ub_density <- density(s2_results$vertex_metadata_LSOA_df$UB) 
    # lb_density <- density(s2_results$vertex_metadata_LSOA_df$LB) 
    # avg_b_density <- density(s2_results$vertex_metadata_LSOA_df$AVG_B) 
    # plot(avg_b_density) # plots the results
    
    
    #Stage 3 optimisation
    #Create list for arguments of attractiveness function
    attractiveness_fun_arg <- list("using_opt" = TRUE,
                    "retail_centre_type" = "NA",
                    "competition_restaurants_df" = preproc$competition_restaurants_df,
                    "ten_mon_footfall_poly_df" = preproc$ten_mon_footfall_poly_df,
                    "ten_mon_footfall_OAs_df" = preproc$ten_mon_footfall_OAs_df,
                    "s.london_poly_df" = preproc$s.london_poly_df,
                    "competition_distance_KM" = competition_distance_KM,
                    "oa_class_df" = preproc$oa_class_df,
                    "OA_centroids_df" = preproc$OA_centroids_df,
                    "store_size" = 16380)
    
    message("Completing stage 3 (final) of the optimisation...")
    
    s3_results <- initOptimisation_s3(s2_results,
                                      type=type_S3,
                                      LSOA_centroids_df=preproc$LSOA_centroids_df,
                                      existing_store_centroid_df=preproc$Store_centroids_df,
                                      preproc$Drive_time_matrix_adf,
                                      preproc$Store_list_df,
                                      calculateAttractivenessAndClass,
                                      attractiveness_fun_arg=attractiveness_fun_arg,
                                      preproc$constants_df,
                                      preproc$LSOA_demand_surface_df,
                                      max_LSOA_dist_km=max_LSOA_dist_km,
                                      Drive_time_list_df=preproc$Drive_time_list_df,
                                      top_stores,
                                      apply_rules,
                                      apply_min_turnover)
    message("Stage 3 completed...")
    
    
    overall_results[[rgn]] <- s3_results 
  }
  
  return(overall_results)
}

calc_non_opt_demand <- function(input_file_path,
                                apply_rules=FALSE, 
                                apply_min_turnover=15000, 
                                apply_store_filter=NA, 
                                competition_distance_KM=1,
                                max_LSOA_dist_km=5,
                                store_name, store_lat, store_long, 
                                retail_centre_type, store_size){
  
  ##Preprocess all data
  message("Preprocessing data...")
  preproc <- preprocess_optimisation(input_file_path, use_distance_estimate_measure=FALSE, apply_rules, apply_min_turnover, apply_store_filter)
  message("Preprocessing data completed...")
  
  ##Calculate demand for new location
  attractiveness_fun_arg <- list("using_opt" = FALSE,
                                 "retail_centre_type" = retail_centre_type,
                                 "competition_restaurants_df" = preproc$competition_restaurants_df,
                                 "ten_mon_footfall_poly_df" = preproc$ten_mon_footfall_poly_df,
                                 "ten_mon_footfall_OAs_df" = preproc$ten_mon_footfall_OAs_df,
                                 "s.london_poly_df" = preproc$s.london_poly_df,
                                 "competition_distance_KM" = competition_distance_KM,
                                 "oa_class_df" = preproc$oa_class_df,
                                 "OA_centroids_df" = preproc$OA_centroids_df,
                                 "store_size" = store_size)
  
  message("Calculating drive times for new store location...")
  s_pred <- calc_demand(store_name = store_name, 
                        store_lat = store_lat,
                        store_long = store_long,
                        preproc$LSOA_centroids_df,
                        preproc$Store_centroids_df, preproc$Drive_time_matrix_adf, 
                        preproc$Store_list_df, calculateAttractivenessAndClass, attractiveness_fun_arg, 
                        preproc$constants_df, preproc$LSOA_demand_surface_df, max_LSOA_dist_km=max_LSOA_dist_km, 
                        use_drive_time_list = FALSE, 
                        Drive_time_list_df=preproc$Drive_time_list_df)
  message("Completed calculating drive times for new store location...")
  
  #check if store meets min demand conditions
  if (apply_rules){
    
    if (s_pred$demand < apply_min_turnover){
      
      ## break from current loop
      message("Store at new location does not meet required demand conditions!")
      stop("Terminating further calculation!")
      
    }
    
  }
  
  #Add store to store list and drivetime matrix adf for full calculation
  message("Adding store to the store list and drive time matrix...")
  new_m <- calc_new_drive_time_matrix_and_store_list(as.character(s_pred$store), s_pred$lat, s_pred$long, preproc$LSOA_centroids_df,
                                                     preproc$Store_centroids_df, preproc$Drive_time_matrix_adf, 
                                                     preproc$Store_list_df, calculateAttractivenessAndClass, attractiveness_fun_arg, 
                                                     preproc$constants_df, preproc$LSOA_demand_surface_df, max_LSOA_dist_km=max_LSOA_dist_km, 
                                                     use_drive_time_list=FALSE, preproc$Drive_time_list_df)
  
  
  
  #Once all optimal stores have been added, calculate the full demand matrix again
  message("Calculating demand for new store location...")
  result <- modelfunc(new_m$Store_list_df_n, preproc$constants_df, 
                      preproc$LSOA_demand_surface_df, new_m$Drive_time_matrix_adf_n)
  
  return(result)
  
}