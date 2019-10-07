##main script
# Remove all the objects we created so far.
rm(list = ls())

#source other files
source("optimisation.R")

#get base model
base <- build_base(input_file_path = "C:\\Users\\hamzajuzer\\Documents\\PD\\LocationEdge\\main\\", 
                   output_csv=TRUE, output_spec=NULL, 
                   apply_rules=TRUE, apply_min_turnover=15000, apply_store_filter=NA)

#run optimisation
opt_res <- optimise(input_file_path = "C:\\Users\\hamzajuzer\\Documents\\PD\\LocationEdge\\main\\", 
                     use_distance_estimate_measure=TRUE,
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
                     top_stores=1)

#run individual scenario
non_opt_res <- calc_non_opt_demand(input_file_path = "C:\\Users\\hamzajuzer\\Documents\\PD\\LocationEdge\\main\\",
                    apply_rules=FALSE, 
                    apply_min_turnover=15000, 
                    apply_store_filter=NA, 
                    competition_distance_KM=1,
                    max_LSOA_dist_km=5,
                    "new_store", 
                    store_lat=52.44819, 
                    store_long=-1.822393)

