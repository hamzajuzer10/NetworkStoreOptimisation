# NetworkStoreOptimisation
Network Store Optimisation (R)

Steps to clone the repository to your local drive:

(2 methods)

Method 1:

1) Download and install git and git bash (If not done so)
2) Open git bash and access the directory to clone the repo to
3) Type git clone https://github.com/hamzajuzer10/NetworkStoreOptimisation.git into git bash terminal to clone your repo
4) Access K:\Consulting\London\Clients\P\PizzaExpress\11. Data for Hamza\Optimisation Model Input Files to source all input files to save in your designated input location - it is advisable to copy these files into the same drive where your code will be run as the code will read from them

Method 2: 

1) Select Clone or Download (green button on top right hand corner of the screen) and download the zip file which contains the code

Steps to run the model:

1) Access the cloned repository on your local drive
2) Select main R project to open RStudio
3) Open main.R in RStudio
4) Run the first few lines of code

       rm(list = ls())
       source("optimisation.R")

There should be four main functions that you will see (you do not need to run them in order):

1) build_base: This function will calculate the expected demand for existing stores 

Arguments:

    a) input_file_path: "C:\\Users\\..\\" path to your input files - make sure you append an \\ to the end (see step 4 in cloning the repository),

    b) output_csv: (TRUE/FALSE) writes the expected demand for existing stores into the directory of your choice (see output_spec) - demand data is stored in 'Store_demand.csv', 

    c) output_spec: "C:\\Users\\.." specifies either the directory or a prefix to the expected demand output file to be written, 

    d) apply_rules: (TRUE/FALSE) applies store filtering rules (either by specifying the min turnover) or the names of the stores, 

    e) apply_min_turnover: 15000 applies the min turnover to filter existing stores based on the expected demand, 

    f) apply_store_filter: c(3,4,5) applies filtering based on existing store ids - accepts numeric store ids
    
    g) scaling_factor: 0.14 applies a scaling factor to the output demand turnover prediction

  The output of this function is a list with 1 key/value: key: "Gravity_model_store_predictions" and it holds a dataframe with cols: store, demand

2) optimise: This function will optimise network store locations for each region. It will calculate the top n new stores in each region based on expected demand

The region code to name mapping is as follows:

    Region_Code: Region_Name

    E12000005: West Midlands

    E12000004: East Midlands

    E12000001: North East

    E12000002: North West

    E12000003: Yorkshire and The Humber

    E12000009: South West

    E12000006: East of England

    E12000008: South East

    E12000007: London

    S92000003: Scotland

    W92000004: Wales

Arguments:

    a) input_file_path: "C:\\Users\\..\\" path to your input files - make sure you append an \\ to the end (see step 4 in cloning the repository), 

    b) use_distance_estimate_measure: (TRUE/FALSE) uses an estimate of LSOA land area size to filter potential new store sites (used in stage 1 optimisation),

    c) max_dist_LSOA_km: 25 Bounding box size for LSOAs to calculate the max demand per LSOA (used in stage 1 optimisation),

    d) max_dist_store_LSOA_km: 15 Bounding box size for number of competitor stores to calculate min demand per LSOA (used in stage 1 optimisation),

    e) min_demand: 50000 Minimum demand to filter out non-feasible LSOAs (used in stage 1 optimisation),

    f) top_n_perc: 10 Take top x% of LSOAs & OAs to calculate full gravity model prediction (used in stage 2 optimisation), 

    g) type_S2: ('AVG', 'UB') Take top x% of LSOAs based on either average of upper and lower bounds calculated in stage 1 optimisation or take top x% of LSOAs based on upper bound only,

    h) type_S3: 'non-greedy' Use a non-greedy approach to calculate best stores (used in stage 3 optimisation),

    i) apply_rules: (TRUE/FALSE) applies store filtering rules (either by specifying the min turnover) or the names of the stores,

    j) apply_min_turnover: 15000 applies the min turnover to filter existing stores based on the expected demand, 

    k) apply_store_filter: c(3,4,5) applies filtering based on existing store ids - accepts numeric store ids

    l) competition_distance_KM: 1 Calculates attractiveness scores based on competition range,

    m) max_LSOA_dist_km: 5 Calculates the LSOAs to be included in the drivetime matrix for potential OA locations (only used if OSRM is used),

    n) top_stores: 1 Calculates the top n new stores to be opened based on predicted demand. Takes store cannibalisation into account. 
    
    o) scaling_factor: 0.14 applies a scaling factor to the output demand turnover prediction

   The output of this function is a list with keys representing the region codes. The value for each region code key is a list with 1 key/value: key: "Gravity_model_store_predictions" and it holds a dataframe with cols: store, demand

3) calc_non_opt_demand: This function will calculate the predicted demand for an input store location

Arguments:

    a) input_file_path: "C:\\Users\\..\\" path to your input files - make sure you append an \\ to the end (see step 4 in cloning the repository), 
    
    b) apply_rules: (TRUE/FALSE) applies store filtering rules (either by specifying the min turnover) or the names of the stores,

    c) apply_min_turnover: 15000 applies the min turnover to filter existing stores based on the expected demand, 

    d) apply_store_filter: c(3,4,5) applies filtering based on existing store ids - accepts numeric store ids

    e) competition_distance_KM: 1 Calculates attractiveness scores based on competition range,

    f) max_LSOA_dist_km: 5 Calculates the LSOAs to be included in the drivetime matrix for potential OA locations (only used if OSRM is used - this is now a legacy feature),

    g) store_name: "new_store" name of the new store (which will appear in the output store list)
    
    h) store_lat: 51.6 Latitude of the new store
    
    i) store_long: -2.4 Longiture of the new store
    
    j) retail_centre_type: "Major Mall" Used to calculate attractiveness (see predefined list)
    
    k) store_size: 20000 Used to calculate attractiveness
    
    l) scaling_factor: 0.14 applies a scaling factor to the output demand turnover prediction
    
    m) output_csv=(TRUE/FALSE) writes the expected demand and parameters for existing and new stores into the directory of your choice (see output_spec) - demand data is stored in 'New_store_demand.csv' and new store parameters are stored in 'New_store_parameters.csv', 
    
    n) output_spec= "C:\\Users\\.." specifies either the directory or a prefix to the expected demand and parameters output file to be written, 
    
    o) run=(0/1) Specifies whether the function should run - 1 runs the function, 0 merely returns a placeholder value 

   The output of this function is a successful run, 0,  or not run ,1, flag

4) compare_scenarios: This function will compare the output of the previous 3 functions to each other and return the demand difference for similar stores

Arguments:

    a) arg_x: Output of build_base/ calc_non_opt_demand or the output of optimise but referencing the appropriate region code (i.e. output_optimise$region_code)
    
    b) arg_y: Output of build_base/ calc_non_opt_demand or the output of optimise but referencing the appropriate region code (i.e. output_optimise$region_code)
    
   The output of this function is a dataframe with 4 cols: store (which contains all the similar stores in arg_x, arg_y), demand.x, demand.y and demand_difference (demand.x-demand.y) 




