if (!require('tidyr')) install.packages('tidyr'); library('tidyr')
if (!require('reshape2')) install.packages('reshape2'); library('reshape2')
if (!require('data.table')) install.packages('data.table'); library('data.table')


modelfunc <- function(Store_list_df, constants_df, LSOA_demand_surface_df, Drive_time_matrix_adf, output_csv=FALSE, output_spec=NULL, scaling_factor=1.0){

  #Create drive time matrix and attractiveness vector
  # Calculate drive time once! #Drive_time_matrix = acast(Drive_Time, geography_id ~ store_id, value.var = "drive_time")
  Attractiveness = acast(Store_list_df, "store", value.var = "Attractiveness")
  Attractiveness = Attractiveness[colnames(Drive_time_matrix_adf)]
  Demand_surface = acast(LSOA_demand_surface_df, "LSOA", value.var = "DEMAND")
  Demand_surface = Demand_surface[rownames(Drive_time_matrix_adf)]
  
  #create vectors for the beta and alpha for each store
  beta = acast(merge(Store_list_df, constants_df, by = "class"), "store", value.var = "beta")
  beta = beta[colnames(Drive_time_matrix_adf)]
  alpha = acast(merge(Store_list_df, constants_df, by = "class"), "store", value.var = "alpha")
  alpha = alpha[colnames(Drive_time_matrix_adf)]
  
  # caclculate distance decay matrix
  Grm_temp = exp(t(Drive_time_matrix_adf)*c(-beta))*c(alpha*Attractiveness)
  Grm_temp = t(Grm_temp)

  # Normalise with the sum of each row
  Grm_pr = Grm_temp/rowSums(Grm_temp, na.rm = TRUE) #check NA behaviour
  rm(Grm_temp)

  #calculate the demand from each geography to each store
  Grm_dem = Grm_pr*c(Demand_surface)
  
  #scale the demand with a scaling factor
  Grm_dem = Grm_dem*scaling_factor
  
  #convert to long tables
  Gravity_model_probability = melt(Grm_pr, varnames = c("LSOA", "store"), value.name = "probability")
  rm(Grm_pr)
  
  Gravity_model_demand = melt(Grm_dem, varnames = c("LSOA", "store"), value.name = "demand")
  rm(Grm_dem)
  
  Gravity_model_output = cbind(Gravity_model_probability, demand = Gravity_model_demand$demand)
  rm(Gravity_model_probability, Gravity_model_demand)
  
  Gravity_model_store_predictions = aggregate(demand~store, Gravity_model_output, FUN = sum) #, na.rm = TRUE) #check NA behaviour
  
  if (output_csv)
  {
    if(!is.null(output_spec)){
      
      fwrite(Gravity_model_store_predictions, file = paste(output_spec,"Store_demand",".csv"), row.names=FALSE)
      fwrite(Gravity_model_output, file = paste(output_spec,"Output",".csv"), row.names=FALSE)
      
    } else {
      
      message('No output specifier provided - csv files may be overwritten!')
      fwrite(Gravity_model_store_predictions, file = paste("Store_demand",".csv"), row.names=FALSE)
      fwrite(Gravity_model_output, file = paste("Output",".csv"), row.names=FALSE)
      
    }
  }
  
  ## Must keep this output structure for the optimisation!!
  retList <- list("Gravity_model_store_predictions" = Gravity_model_store_predictions)
  ## Note, do not return Gravity model output to save memory
  
  return(retList)
}