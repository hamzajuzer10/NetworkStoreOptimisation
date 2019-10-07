# Load the packages
if (!require('rgdal')) install.packages('rgdal'); library('rgdal')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('data.table')) install.packages('data.table'); library('data.table')


# Variables for holding the coordinate system types (see:
# http://www.epsg.org/ for details)
ukgrid = "+init=epsg:27700"
latlong = "+init=epsg:4326"

# initialise batch convert 
batchConvert <- function(coords_df_origin, geospatial_id, convert_from="EN", convert_to = "latlong"){
  
  # ensure csv file has cols X, Y and geospatial id
  if(!("X" %in% colnames(coords_df_origin)) & 
     !("Y" %in% colnames(coords_df_origin)) & 
     !(geospatial_id %in% colnames(coords_df_origin)))
  {
    message(paste('Expecting \'X\' (Easting),\'Y\' (Northing), and \'', geospatial_id, '\' as col names but none exist\n'))
    stop('Terminating batch convert')
  }
  
  # Remove those entries with missing Eastings or Northings
  coords_df_origin <- subset(coords_df_origin, X != "" | Y != "")
  
  # Create a unique ID for each location (row)
  coords_df_origin$Loc_ID <- 1:nrow(coords_df_origin)
  
  # Create coordinates variable
  coords <- cbind(X = as.numeric(as.character(coords_df_origin$X)),
                  Y = as.numeric(as.character(coords_df_origin$Y)))
  
  # Create the SpatialPointsDataFrame
  coords_SP <- SpatialPointsDataFrame(coords, data = data.frame(coords_df_origin[,geospatial_id],
                                                                coords_df_origin$Loc_ID), proj4string = CRS("+init=epsg:27700"))
  
  # coords_SP is now a spatial data frame. 
  
  # Convert from Eastings and Northings to Latitude and Longitude
  coords_SP_LL <- spTransform(coords_SP, CRS(latlong))
  
  # we also need to rename the columns
  colnames(coords_SP_LL@coords)[colnames(coords_SP_LL@coords) == "X"] <- "Long"
  colnames(coords_SP_LL@coords)[colnames(coords_SP_LL@coords) == "Y"] <- "Lat"
  
  # And then convert it (back) to a data.frame
  coords_df_origin_latlong <- as.data.frame(coords_SP_LL)
  
  # Find all cols with geospatial ID
  col_to_rename <- names(coords_df_origin_latlong)[grepl( "geospatial_id" , names(coords_df_origin_latlong) )]
  setnames(coords_df_origin_latlong, col_to_rename, geospatial_id)
  coords_df_origin_latlong <- coords_df_origin_latlong %>% select(geospatial_id, "Long", "Lat")
  
  #coords_df_origin_latlong$Lat <- as.numeric(as.character(coords_df_origin_latlong$Lat))
  #coords_df_origin_latlong$Long <- as.numeric(as.character(coords_df_origin_latlong$Long))
  
  
  return(coords_df_origin_latlong)
}