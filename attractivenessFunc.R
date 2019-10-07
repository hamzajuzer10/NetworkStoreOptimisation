# define new store attractiveness
attractiveness_new_store <- function(store_name, retail_centre_type="NA", footfall, 
                                     competition_one_km, competition_sq_feet_one_km,
                                     in_london, class, store_size){
  
  #calculate base attractiveness based on log curve
  attractiveness <- 7469.1*log(store_size)-40377
  uplift <- 1
  
  if(retail_centre_type=="Major Mall"){
    
    uplift<- uplift*2.5
    
  } else if (retail_centre_type=="Major Designer Outlet"){
    
    
    uplift<- uplift*6
    
  } else if (retail_centre_type=="Minor Designer Outlet"){
    
    
    uplift<- uplift*2.5
    
  } else if (retail_centre_type=="Tourism"){
    
    
    uplift<- uplift*2.2
    
  } else if (retail_centre_type=="Leisure"){
    
    
    uplift<- uplift*1.7
    
  } else if (retail_centre_type=="Major Leisure"){
    
    
    uplift<- uplift*4
    
  } else if (retail_centre_type=="Major Retail Park"){
    
    
    uplift<- uplift*5
    
  } else if (retail_centre_type=="Minor Retail Park"){
    
    
    uplift<- uplift*1.5
    
  } else if (retail_centre_type=="Leisure Retail Park"){
    
    
    uplift<- uplift*2.8
    
  } else if (retail_centre_type=="Transport"){
    
    
    uplift<- uplift*1.7
    
  } 
  
  footfall_per_competition <- footfall/competition_one_km
  
  if (in_london){
    
    if (competition_one_km>290){
      
      
      uplift<- uplift*0.7
    }
    
    
    
    if (footfall_per_competition>=0 & footfall_per_competition<46){
      
      uplift<- uplift*0.6
      
    } else if (footfall_per_competition>=46 & footfall_per_competition<80){
      
      uplift<- uplift*0.7
      
    } else if (footfall_per_competition>=80 & footfall_per_competition<116.2){
      
      uplift<- uplift*0.9
      
    } else if (footfall_per_competition>=116.2 & footfall_per_competition<217){
      
      uplift<- uplift*1
      
    } else if (footfall_per_competition>=217){
      
      uplift<- uplift*1.5
      
    }
  } else {
    
    if (footfall_per_competition>=619.9){
      
      uplift<- uplift*1.7
      
    }
  }
  
  if(competition_one_km<8){
    
    uplift<- uplift*2.2
    
  }
  
  if(competition_sq_feet_one_km>117000){
    
    uplift<- uplift*0.6
    
  }
  
  attractiveness <- attractiveness*uplift
  
  message("Attractiveness calculated to be ", attractiveness)
  message("Class calculated to be ", class)
  
  #default attractiveness
  x <- data.frame("store" = c(store_name), 
                  "Attractiveness" = c(attractiveness), 
                  "class" = c(class),
                  "Fascia" = c("Pizza Express"))
  return(x)
}
