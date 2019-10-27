
nearestPoint <- function(origin_centroids_df, dest_centroids_df){
  
  
  ##convert origin centroids into spatial dataframe
  origin_coords_sf = st_as_sf(origin_centroids_df, coords = c("long", "lat"), 
                              crs = 4326)
  
  ##convert destination centroids into spatial dataframe
  dest_coords_sf = st_as_sf(dest_centroids_df, coords = c("long", "lat"), 
                            crs = 4326)
  
  # get coordinate matrices
  origin_coords <- do.call(rbind, st_geometry(origin_coords_sf))
  graph_coords <- do.call(rbind, st_geometry(dest_coords_sf))
  distVec <- spDistsN1(graph_coords,origin_coords,longlat = TRUE)
  
  # get min index
  idx <- which.min(distVec)
  return(dest_centroids_df[idx,])
  
}


evaluate_polygon <- function(polygon_df, store_name, 
                             store_lat, store_long,
                             step_size=10000){
  
  #create a data table
  store_coords_df <- data.table(
    store=c(store_name),
    longitude=c(store_long),
    latitude=c(store_lat))
  
  store_coords_sf = st_as_sf(store_coords_df, coords = c("longitude", "latitude"), 
                             crs = 4326)
  
  
  for(i in seq(from=1, to=nrow(polygon_df), by=step_size)){
    j<-min(i+10000,nrow(polygon_df))
    #message("Evaluating polygon ", i, " to polygon ", j)
    sp1<-polygon_df[i:j,]
    result <- over(as(sp1, "Spatial"), as(store_coords_sf, "Spatial"))
    
    #check all non-NA values
    if (!all(is.na(result))){
      
      #get the location index
      NonNAindex <- which(!is.na(result))
      idx <- min(NonNAindex)
      
      return(polygon_df[idx+i-1,])
    }
    
  }
  # no location found, return error
  message('Input latitude and longitudes do not exist in existing polygon centroids')
  return(NULL)
}


calculateClosest <- function(origin_centroids_df, dest_centroids_df, distance_KM){
  
  # calculate the closest LADs for each origin_centroid
  #expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))
  #all_perm_df <- expand.grid.df(origin_centroids_df, LSOA_centroids_df)
  
  # Use the geosphere package to calculate the lat and long bounding boxes 
  # for specific distance and starting coordinates
  
  # Starting longitude and latitude:
  coords <- c(origin_centroids_df$long[[1]], origin_centroids_df$lat[[1]])
  
  # Distance in meters:
  distance <- distance_KM*1000
  
  # North East coords
  ne.coords <- c(destPoint(p = coords, b = 90, d = distance)[1],
                 destPoint(p = coords, b = 0,  d = distance)[2])
  
  # South West coords
  sw.coords <- c(destPoint(p = coords, b = 90, d = -distance)[1],
                 destPoint(p = coords, b = 0,  d = -distance)[2])
  
  # Find all LSOAs with centroids in the bounding box
  dest_filtered_df <- subset(dest_centroids_df , (long > sw.coords[1]) & 
                               (long < ne.coords[1]) &
                               (lat > sw.coords[2]) & 
                               (lat < ne.coords[2]))
  
  return(dest_filtered_df)
}