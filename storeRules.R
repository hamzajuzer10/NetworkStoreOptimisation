

applyStoreRules <- function(Store_list_df, Gravity_model_store_predictions, min_turnover, store=NA, ...)
  {
  
  #Filter Pizza Express stores which do not pass rules
  message("Applying store rules...")
  filter_stores <- Gravity_model_store_predictions[Gravity_model_store_predictions$demand <= min_turnover, ]
  filter_stores <- merge(filter_stores, Store_list_df, by.x = "store", by.y = "store")
  filter_stores <- filter_stores[filter_stores$Fascia == "Pizza Express", ]
  filter_stores <- subset(filter_stores, select=c("store"))
  if(!is.na(store)){
    store <- as.data.frame(store)
    filter_stores <- rbind(filter_stores, store)
  }
  filter_stores <- subset(filter_stores, !duplicated(subset(filter_stores, select=c(store))))
  n_filtered_stores <- dim(filter_stores)[1]
  
  # if filter_stores is not empty
  if (n_filtered_stores > 0) {
    
    message("Filtering ", n_filtered_stores, " stores due to rules being applied!")
    Store_list_df <- subset(Store_list_df, !(store %in% filter_stores$store))
    
  } else {
    
    message("No stores filtered...")
  }
  
  return(Store_list_df)
}