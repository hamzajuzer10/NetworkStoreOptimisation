# NetworkStoreOptimisation
Network Store Optimisation (R)

Steps to clone the repository to your local drive:

1) Download and install git and git bash (If not done so)
2) Open git bash and access the directory to clone the repo
3) Type git clone https://github.com/hamzajuzer10/NetworkStoreOptimisation.git into git bash terminal to clone your repo
4) Access K:\Consulting\London\Clients\P\PizzaExpress\11. Data for Hamza\Optimisation Model Input Files to source all input files to save in your designated input location - it is advisable to copy these files into the same drive where your code will be run as the code will read from them

Steps to run the model:

1) Access the cloned repository on your local drive
2) Select main R project to open RStudio
3) Open main.R in RStudio

There should be four main functions that you will see:

1) build_base: This function will calculate the expected demand for existing stores 

Arguments:

    a) input_file_path: "C:\\Users\\..\\" path to your input files - make sure you append an \\ to the end (see step 4 in cloning the repository),

    b) output_csv: (TRUE/FALSE) writes the expected demand and output files for existing stores into the directory of your choice (see output_spec), 

    c) output_spec= "C:\\Users\\.." specifies either the directory or a specifier prepended to the expected demand and output files to be written, 

    d) apply_rules=(TRUE/FALSE) applies store filtering rules (either by specifying the min turnover) or the names of the stores, 

    e) apply_min_turnover=15000 applies the min turnover to filter existing stores based on the expected demand, 

    f) apply_store_filter=c(3,4,5) applies filtering based on existing store ids - accepts numeric store ids

  The output of this function is a list with 1 key/value: key: "Gravity_model_store_predictions" and it holds a dataframe with cols: store, demand

2) optimise: This function will optimise network store locations for each region. It will calculate the top n new stores in each region based on expected demand

Arguments:

    a) input_file_path: "C:\\Users\\..\\" path to your input files - make sure you append an \\ to the end (see step 4 in cloning the repository), 

    b) use_distance_estimate_measure: (TRUE/FALSE) uses an estimate of LSOA land area size to filter potential new store sites (used in stage 1 optimisation),

    c) max_dist_LSOA_km: 25 Bounding box size for LSOAs to calculate the max demand per LSOA (used in stage 1 optimisation),

    d) max_dist_store_LSOA_km=15 Bounding box size for number of competitor stores to calculate min demand per LSOA (used in stage 1 optimisation),

    e) min_demand=50000,

    f) top_n_perc=10, 

    g) type_S2='AVG',

    h) type_S3='greedy',

    i) apply_rules=FALSE, 

    j) apply_min_turnover=15000, 

    k) apply_store_filter=NA, 

    l) competition_distance_KM=1,

    m) max_LSOA_dist_km=5,

    n) top_stores=1


