Import packages + wd

``` r
library(pacman)
p_load(tidyverse)
p_load(sf, lwgeom)
p_load(rgdal)
p_load(doBy) #which.minn
p_load(plyr) #empty()
p_load(system.time) #Get runtime
```

    ## Installing package into 'C:/Users/stine/OneDrive/Dokumenter/R/win-library/4.0'
    ## (as 'lib' is unspecified)

    ## Warning: package 'system.time' is not available (for R version 4.0.2)

    ## Warning: unable to access index for repository http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/4.0:
    ##   kan ikke åbne adresse 'http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/4.0/PACKAGES'

    ## Warning: 'BiocManager' not available.  Could not check Bioconductor.
    ## 
    ## Please use `install.packages('BiocManager')` and then retry.

    ## Warning in p_install(package, character.only = TRUE, ...):

    ## Warning in library(package, lib.loc = lib.loc, character.only = TRUE,
    ## logical.return = TRUE, : there is no package called 'system.time'

    ## Warning in p_load(system.time): Failed to install/load:
    ## system.time

``` r
setwd("C:/Users/stine/OneDrive/Cognitive Science/5th_Semester/bachelor/ABM_Aarhus")
```

########################### Agents Choose Preferred Home

Preferred postcodes overall + functions:

``` r
overall_k_h <- read_csv("overall_k_h.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   Postalcodes = col_double(),
    ##   Names = col_character(),
    ##   Green_area_distance = col_double(),
    ##   Public_transport_distance = col_double(),
    ##   Min_Mall_distance = col_double(),
    ##   School_rating = col_double(),
    ##   Owner_oc_housing_m2 = col_double(),
    ##   Perc_non_western_im_desc = col_double(),
    ##   Almenbolig_sd_rent = col_double()
    ## )

``` r
overall_k_l <- read_csv("overall_k_l.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   Postalcodes = col_double(),
    ##   Names = col_character(),
    ##   Green_area_distance = col_double(),
    ##   Public_transport_distance = col_double(),
    ##   Min_Mall_distance = col_double(),
    ##   School_rating = col_double(),
    ##   Owner_oc_housing_m2 = col_double(),
    ##   Perc_non_western_im_desc = col_double(),
    ##   Almenbolig_sd_rent = col_double()
    ## )

``` r
overall_n_h <- read_csv("overall_n_h.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   Postalcodes = col_double(),
    ##   Names = col_character(),
    ##   Green_area_distance = col_double(),
    ##   Public_transport_distance = col_double(),
    ##   Min_Mall_distance = col_double(),
    ##   School_rating = col_double(),
    ##   Owner_oc_housing_m2 = col_double(),
    ##   Perc_non_western_im_desc = col_double(),
    ##   Almenbolig_sd_rent = col_double()
    ## )

``` r
overall_n_l <- read_csv("overall_n_l.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   Postalcodes = col_double(),
    ##   Names = col_character(),
    ##   Green_area_distance = col_double(),
    ##   Public_transport_distance = col_double(),
    ##   Min_Mall_distance = col_double(),
    ##   School_rating = col_double(),
    ##   Owner_oc_housing_m2 = col_double(),
    ##   Perc_non_western_im_desc = col_double(),
    ##   Almenbolig_sd_rent = col_double()
    ## )

``` r
#Find the best postcode for type:

#Import cheap postcodes: 
cheap_postcodes <- read_csv("cheapest_postcodes.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   Average = col_double(),
    ##   Postcodes = col_double()
    ## )

``` r
################################### kids_low_economy #######################################
pref_kids_low <- function(environment){
  df <- filter(environment, environment$Postalcodes %in% cheap_postcodes$Postcodes |
                 environment$Postalcodes %in% c(8220))

  if (length(df$Postalcodes)>1){
    df <- filter(df, df$Perc_non_western_im_desc > 5)
  } 
  if (length(df$Postalcodes)>1){
    small <- which.minn(df$Green_area_distance, n = 5)
    df <- filter(df, df$Green_area_distance %in% df$Green_area_distance[small])
  } 
  if (length(df$Postalcodes)>1) {
    small <- which.minn(df$School_rating, n = 3)
    df <- filter(df, df$School_rating %in% df$School_rating[small])
  } 
  if (length(df$Postalcodes)>1) {
    small <- which.minn(df$Public_transport_distance, n = 2)
    df <- filter(df, df$Public_transport_distance %in% df$Public_transport_distance[small])
  } 
  if (length(df$Postalcodes)>1) {
    small <- which.minn(df$Min_Mall_distance, n = 1)
    df <- filter(df, df$Min_Mall_distance %in% df$Min_Mall_distance[small])
  }
  return(df)
}



##################################### kids_high_economy #############################
pref_kids_high <- function(environment){
  df <- filter(environment, environment$Owner_oc_housing_m2 >=                               mean(environment$Owner_oc_housing_m2)-(sd(environment$Owner_oc_housing_m2)/2))

  if (length(df$Postalcodes)>1){
    df <- filter(df, df$Perc_non_western_im_desc > 5)
  } 
  if (length(df$Postalcodes)>1){
    small <- which.minn(df$Green_area_distance, n = 5)
    df <- filter(df, df$Green_area_distance %in% df$Green_area_distance[small])
  } 
  if (length(df$Postalcodes)>1) {
    small <- which.minn(df$School_rating, n = 3)
    df <- filter(df, df$School_rating %in% df$School_rating[small])
  } 
  if (length(df$Postalcodes)>1) {
    small <- which.minn(df$Public_transport_distance, n = 2)
    df <- filter(df, df$Public_transport_distance %in% df$Public_transport_distance[small])
  } 
  if (length(df$Postalcodes)>1) {
    small <- which.minn(df$Min_Mall_distance, n = 1)
    df <- filter(df, df$Min_Mall_distance %in% df$Min_Mall_distance[small])
  }
  return(df)
}



##################################### no_kids_low_economy ####################################
pref_no_kids_low <- function(environment){
  df <- filter(environment, environment$Postalcodes %in% cheap_postcodes$Postcodes |
                 environment$Postalcodes %in% c(8220))
  
  if (length(df$Postalcodes)>1){
    df <- filter(df, df$Perc_non_western_im_desc > 5)
    
  } 
  if (length(df$Postalcodes)>1){
    small <- which.minn(df$Green_area_distance, n = 5)
    df <- filter(df, df$Green_area_distance %in% df$Green_area_distance[small])
  }
  if (length(df$Postalcodes)>1) {
    small <- which.minn(df$Public_transport_distance, n = 2)
    df <- filter(df, df$Public_transport_distance %in% df$Public_transport_distance[small])
  } 
  if (length(df$Postalcodes)>1) {
    small <- which.minn(df$Min_Mall_distance, n = 1)
    df <- filter(df, df$Min_Mall_distance %in% df$Min_Mall_distance[small])
  }
  return(df)
}



################################# no_kids_high_economy ###################################
pref_no_kids_high <- function(environment){
  df <- filter(environment, environment$Owner_oc_housing_m2 >=                               mean(environment$Owner_oc_housing_m2)-(sd(environment$Owner_oc_housing_m2)/2))

  if (length(df$Postalcodes)>1){
    df <- filter(df, df$Perc_non_western_im_desc > 5)
  } 
  if (length(df$Postalcodes)>1){
    small <- which.minn(df$Green_area_distance, n = 5)
    df <- filter(df, df$Green_area_distance %in% df$Green_area_distance[small])
  }
  if (length(df$Postalcodes)>1) {
    small <- which.minn(df$Public_transport_distance, n = 2)
    df <- filter(df, df$Public_transport_distance %in% df$Public_transport_distance[small])
  } 
  if (length(df$Postalcodes)>1) {
    small <- which.minn(df$Min_Mall_distance, n = 1)
    df <- filter(df, df$Min_Mall_distance %in% df$Min_Mall_distance[small])
  }
  return(df)
}
```

Agents:

``` r
# Assuming having kids and once economic level are independent these are the following probabilities of belonging to each group
# kids_low_economy 0.5*0.35=17.5 % rond up 
# kids_high_economy 0.5*0.35=17.5 % 
# no_kids_low_economy 0.5*0.65 =32.5% round up 
# no_kids_high_economy 0.5*0.65 =32.5%

envi <- read_csv2("environment_aarhus.csv")
```

    ## i Using ',' as decimal and '.' as grouping mark. Use `read_delim()` for more control.

    ## Warning: Missing column names filled in: 'X1' [1]

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   X1 = col_double(),
    ##   Postalcodes = col_double(),
    ##   Names = col_character(),
    ##   latitude = col_double(),
    ##   longitude = col_double(),
    ##   Green_area_distance = col_double(),
    ##   Public_transport_distance = col_double(),
    ##   Min_Mall_distance = col_double(),
    ##   School_rating = col_double(),
    ##   Owner_oc_housing_m2 = col_double(),
    ##   Perc_non_western_im_desc = col_double(),
    ##   Almenbolig_sd_rent = col_double()
    ## )

``` r
envi <- envi[,-c(1, 4:5)] # Dropping unnecessary columns

n= 100 # Number of households set to 16656 

kids_low_economy <- rep("kids_low_economy", round(n*0.175))
kids_high_economy <- rep("kids_high_economy", round(n*0.175))
no_kids_low_economy <- rep("no_kids_low_economy", round(n*0.325))
no_kids_high_economy <- rep("no_kids_high_economy", floor(n*0.325))

pref <- data.frame(matrix(ncol = 4, nrow = n)) # Empty dataframe to put in the preferences of people 
colnames(pref)<- c("agent_type", "equally_good_1", "original_postcode", "move_1")

pref$agent_type <- c(kids_low_economy, kids_high_economy, no_kids_low_economy, no_kids_high_economy)
pref$equally_good_1 <- 0

#REMEMBER TO SET SEED 
## Place the types randomly in a postcode
set.seed(3728) # 487 # 8376 # 3728
pref$original_postcode <- sample(envi$Postalcodes, length(pref$agent_type), replace = TRUE)

pref$move_1 <- 0 # Has to have something different from NA in the first column before beginning
```

Loop for finding preferred postcode

``` r
#Import environment 
envi <- read_csv2("environment_aarhus.csv")
```

    ## i Using ',' as decimal and '.' as grouping mark. Use `read_delim()` for more control.

    ## Warning: Missing column names filled in: 'X1' [1]

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   X1 = col_double(),
    ##   Postalcodes = col_double(),
    ##   Names = col_character(),
    ##   latitude = col_double(),
    ##   longitude = col_double(),
    ##   Green_area_distance = col_double(),
    ##   Public_transport_distance = col_double(),
    ##   Min_Mall_distance = col_double(),
    ##   School_rating = col_double(),
    ##   Owner_oc_housing_m2 = col_double(),
    ##   Perc_non_western_im_desc = col_double(),
    ##   Almenbolig_sd_rent = col_double()
    ## )

``` r
envi <- envi[,-c(1, 4:5)] # Dropping unnecessary columns

#Import neighbouring postcodes 
neigh <- read_csv2("neighbouring_postcodes.csv")
```

    ## i Using ',' as decimal and '.' as grouping mark. Use `read_delim()` for more control.

    ## Warning: Missing column names filled in: 'X2' [2], 'X3' [3], 'X4' [4], 'X5' [5],
    ## 'X6' [6], 'X7' [7], 'X8' [8], 'X9' [9]

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   Postalcodes = col_double(),
    ##   X2 = col_double(),
    ##   X3 = col_double(),
    ##   X4 = col_double(),
    ##   X5 = col_double(),
    ##   X6 = col_double(),
    ##   X7 = col_double(),
    ##   X8 = col_double(),
    ##   X9 = col_double()
    ## )

``` r
emp <- as.data.frame(matrix(10000, nrow = 1, ncol = 9))
colnames(emp) <- colnames(envi)

emp_high <- emp
emp_high$Almenbolig_sd_rent <- 0

i=4
j=1

##################################### Simulation FUNCTION #####################################

simulation <- function(preferences, overall_kids_low, overall_kids_high, overall_no_low, overall_no_high, environment, neigh_postcodes, empty, empty_high){
 
  while (sum(is.na(preferences[,i])) < length(preferences$agent_type)){ # Continue while at least one agent is still moving
    t=i-2
    preferences[[paste0("move_",t)]] <- NA#Add new columns as necessary 
    
    for (i in 4:ncol(preferences)){ # Looping over newly created columns
      
      for (j in 1:length(preferences$agent_type)){ # Looping over all agents 
        
      if (is.na(preferences[j,i-1])){
        next
      } 
###################################### Kids Low ########################################
        if (preferences$agent_type[j] == "kids_low_economy"){ 
          
          # Finding the nearby postcodes for the postcode the agent is currently living in 
          near <- filter(neigh_postcodes, neigh_postcodes$Postalcodes == preferences[j,i-1]) 
          # Finding preferred postcode among surrounding neighborhoods 
          pref_near <- filter(environment, environment$Postalcodes %in% near[2:9]) %>% pref_kids_low()
          
          # compare current, surrounding, and all-time preferred postcode
          # Need to stay if neither the all preferred nor the surrounding is better on multiple counts
  
          #Check that pref_near has data. If not set row three = 0 
          ifelse(empty(pref_near), 
                 prefered <- rbind(filter(environment, environment$Postalcodes %in% 
                                     preferences[j,i-1]), overall_k_l, empty), 
                 prefered <- rbind(filter(environment, environment$Postalcodes %in% 
                                     preferences[j,i-1]), overall_k_l, pref_near))
           
          #Prefered neighbouring postcode Vs. Current postcode 
         if(sum(prefered$Almenbolig_sd_rent[[3]] < prefered$Almenbolig_sd_rent[[1]], 
             prefered$Green_area_distance[[3]] < prefered$Green_area_distance[[1]])>=1){
              
              preferences[j,i] <- prefered$Postalcodes[3]
            
          #If household has moved back and forth between two postcodes then choose one at random
           if(i>=6 && !any(is.na(preferences[j,(i-3):i])) && 
              sum(preferences[j,(i-3):(i-2)] == preferences[j,(i-1):(i)])==2){
              preferences[j,i-1] <- sample(preferences[j,(i-1):i], 1, prob = c(0.5, 0.5))
              preferences[j,i] <- NA
              preferences[j,2] <- 1
            }   
              
            #Overall prefered vs. Current postcode   
          } else if (sum(prefered$Almenbolig_sd_rent[[2]] < prefered$Almenbolig_sd_rent[[1]],
          prefered$Green_area_distance[[2]] < prefered$Green_area_distance[[1]],
          prefered$School_rating[[2]] < prefered$School_rating[[1]], 
          prefered$Public_transport_distance[[2]] < prefered$Public_transport_distance[[1]])>=3
          & is.na(preferences[j,i])){ # Larger than 3 is too harsh 
            
            preferences[j,i] <- prefered$Postalcodes[2]
             
          #If household has moved back and forth between two postcodes then choose one at random
            if(i>=6 && !any(is.na(preferences[j,(i-3):i])) && 
              sum(preferences[j,(i-3):(i-2)] == preferences[j,(i-1):(i)])==2){
              preferences[j,i-1] <- sample(preferences[j,(i-1):i], 1, prob = c(0.5, 0.5))
              preferences[j,i] <- NA
              preferences[j,2] <- 1
            }
            
          } else {
            preferences[j,i] <- NA

          }

########################################## KIDS HIGH ########################################
        } else if (preferences$agent_type[j] == "kids_high_economy") {
          
          # Finding the nearby postcodes for the postcode the agent is currently living in 
          near <- filter(neigh_postcodes, neigh_postcodes$Postalcodes == preferences[j,i-1]) 
          # Finding preferred postcode among surrounding neighborhoods 
          pref_near <- filter(environment, environment$Postalcodes %in% near[2:9]) %>% pref_kids_high()
          
        # compare current, all-time preferred postcode, and prefered surrounding
          # Need to stay if neither the all preferred nor the surrounding is better on multiple counts
  
          #Check that pref_near has data. If not set row three = 0
          ifelse(empty(pref_near), 
                 prefered <- rbind(filter(environment, environment$Postalcodes %in% 
                                     preferences[j,i-1]), overall_k_h, empty_high), 
                 prefered <- rbind(filter(environment, environment$Postalcodes %in% 
                                     preferences[j,i-1]), overall_k_h, pref_near))
           
          # Prefered Neighbouring postcode vs. current postcode
         if(sum(prefered$Owner_oc_housing_m2 [[3]] > prefered$Owner_oc_housing_m2[[1]], 
             prefered$Green_area_distance[[3]] < prefered$Green_area_distance[[1]])>=1){
              
              preferences[j,i] <- prefered$Postalcodes[3]
            
          #If household has moved back and forth between two postcodes then choose one at random
            if(i>=6 && !any(is.na(preferences[j,(i-3):i])) && 
              sum(preferences[j,(i-3):(i-2)] == preferences[j,(i-1):(i)])==2){
              preferences[j,i-1] <- sample(preferences[j,(i-1):i], 1, prob = c(0.5, 0.5))
              preferences[j,i] <- NA
              preferences[j,2] <- 1
            }
              # Overall prefered postcode vs. current postcode 
          } else if (sum(prefered$Owner_oc_housing_m2[[2]] > prefered$Owner_oc_housing_m2[[1]],
          prefered$Green_area_distance[[2]] < prefered$Green_area_distance[[1]],
          prefered$School_rating[[2]] < prefered$School_rating[[1]], 
          prefered$Public_transport_distance[[2]] < prefered$Public_transport_distance[[1]])>=3
          & is.na(preferences[j,i])){ # Larger than 3 is too harsh 
            
            preferences[j,i] <- prefered$Postalcodes[2]
             
          #If household has moved back and forth between two postcodes then choose one at random
            if(i>=6 && !any(is.na(preferences[j,(i-3):i])) && 
              sum(preferences[j,(i-3):(i-2)] == preferences[j,(i-1):(i)])==2){
              preferences[j,i-1] <- sample(preferences[j,(i-1):i], 1, prob = c(0.5, 0.5))
              preferences[j,i] <- NA
              preferences[j,2] <- 1
            }
            
          } else {
            preferences[j,i] <- NA

          }
  
########################################## NO KIDS LOW #######################################
        } else if (preferences$agent_type[j] == "no_kids_low_economy") {
          
          # Finding the nearby postcodes for the postcode the agent is currently living in 
          near <- filter(neigh_postcodes, neigh_postcodes$Postalcodes == preferences[j,i-1]) 
          # Finding preferred postcode among surrounding neighborhoods 
          pref_near <- filter(environment, environment$Postalcodes %in% near[2:9]) %>% pref_no_kids_low()
          
          # compare current, surrounding, and all-time preferred postcode
          # Need to stay if neither the all preferred nor the surrounding is better on multiple counts
  
          #Check that pref_near has data. If not set row three = 0
          ifelse(empty(pref_near), 
                 prefered <- rbind(filter(environment, environment$Postalcodes %in% 
                                     preferences[j,i-1]), overall_n_l, empty), 
                 prefered <- rbind(filter(environment, environment$Postalcodes %in% 
                                     preferences[j,i-1]), overall_n_l, pref_near))
           
          # Prefered neighbouring postcode VS. current postcode 
         if(sum(prefered$Almenbolig_sd_rent[[3]] < prefered$Almenbolig_sd_rent[[1]], 
             prefered$Green_area_distance[[3]] < prefered$Green_area_distance[[1]])>=1){
              
              preferences[j,i] <- prefered$Postalcodes[3]
            
          #If household has moved back and forth between two postcodes then choose one at random
            if(i>=6 && !any(is.na(preferences[j,(i-3):i])) && 
              sum(preferences[j,(i-3):(i-2)] == preferences[j,(i-1):(i)])==2){
              preferences[j,i-1] <- sample(preferences[j,(i-1):i], 1, prob = c(0.5, 0.5))
              preferences[j,i] <- NA
              preferences[j,2] <- 1
            }
          #Overall prefered postcode Vs. Current postcode 
          } else if (sum(prefered$Almenbolig_sd_rent[[2]] < prefered$Almenbolig_sd_rent[[1]],
          prefered$Green_area_distance[[2]] < prefered$Green_area_distance[[1]],
          prefered$Public_transport_distance[[2]] < prefered$Public_transport_distance[[1]])>=3
          & is.na(preferences[j,i])){ # Larger than 3 is too harsh 
            
            preferences[j,i] <- prefered$Postalcodes[2]
             
            if(i>=6 && !any(is.na(preferences[j,(i-3):i])) && 
              sum(preferences[j,(i-3):(i-2)] == preferences[j,(i-1):(i)])==2){
              preferences[j,i-1] <- sample(preferences[j,(i-1):i], 1, prob = c(0.5, 0.5))
              preferences[j,i] <- NA
              preferences[j,2] <- 1
            }
            
          } else {
            preferences[j,i] <- NA

          }
  
####################################### NO KIDS HIGH #########################################
        } else if (preferences$agent_type[j] == "no_kids_high_economy") {
          
          # Finding the nearby postcodes for the postcode the agent is currently living in 
          near <- filter(neigh_postcodes, neigh_postcodes$Postalcodes == preferences[j,i-1]) 
          # Finding preferred postcode among surrounding neighborhoods 
          pref_near <- filter(environment, environment$Postalcodes %in% near[2:9]) %>% pref_no_kids_high()
          
          # compare current, surrounding, and all-time preferred postcode
          # Need to stay if neither the all preferred nor the surrounding is better on multiple counts
  
          #Check that pref_near has data. If not set row three = 0
          ifelse(empty(pref_near), 
                 prefered <- rbind(filter(environment, environment$Postalcodes %in% 
                                     preferences[j,i-1]), overall_n_h, empty_high), 
                 prefered <- rbind(filter(environment, environment$Postalcodes %in% 
                                     preferences[j,i-1]), overall_n_h, pref_near))
           
          # Prefered neighbouring postcode VS. current postcode 
         if(sum(prefered$Owner_oc_housing_m2[[3]] > prefered$Owner_oc_housing_m2[[1]], 
             prefered$Green_area_distance[[3]] < prefered$Green_area_distance[[1]])>=1){
              
              preferences[j,i] <- prefered$Postalcodes[3]
          #If household has moved back and forth between two postcodes then choose one at random
            if(i>=6 && !any(is.na(preferences[j,(i-3):i])) && 
              sum(preferences[j,(i-3):(i-2)] == preferences[j,(i-1):(i)])==2){
              preferences[j,i-1] <- sample(preferences[j,(i-1):i], 1, prob = c(0.5, 0.5))
              preferences[j,i] <- NA
              preferences[j,2] <- 1
            }
              # Overall prefered  VS. Current postcode 
          } else if (sum(prefered$Owner_oc_housing_m2[[2]] > prefered$Owner_oc_housing_m2[[1]],
          prefered$Green_area_distance[[2]] < prefered$Green_area_distance[[1]],
          prefered$Public_transport_distance[[2]] < prefered$Public_transport_distance[[1]])>=3
          & is.na(preferences[j,i])){ # Larger than 3 is too harsh 
            
            preferences[j,i] <- prefered$Postalcodes[2]
           
          #If household has moved back and forth between two postcodes then choose one at random
            if(i>=6 && !any(is.na(preferences[j,(i-3):i])) && 
              sum(preferences[j,(i-3):(i-2)] == preferences[j,(i-1):(i)])==2){
              preferences[j,i-1] <- sample(preferences[j,(i-1):i], 1, prob = c(0.5, 0.5))
              preferences[j,i] <- NA
              preferences[j,2] <- 1
            }
            
          } else {
            preferences[j,i] <- NA

          }
          
        }
        
      }
    }
  }

  return(preferences)
}
```

Simmuleringer:

``` r
# Base sim 1 
system.time({base_sim_1 <-simulation(pref, overall_k_l, overall_k_h, overall_n_l, overall_n_h, envi, neigh, emp, emp_high)})
```

    ##    user  system elapsed 
    ##   16.87    0.18   17.07

``` r
write.csv(base_sim_1, "base_sim_1.csv", row.names = FALSE)

#base sim 2
system.time({base_sim_2 <-simulation(pref, overall_k_l, overall_k_h, overall_n_l, overall_n_h, envi, neigh, emp, emp_high)})
```

    ##    user  system elapsed 
    ##   17.34    0.21   17.55

``` r
write.csv(base_sim_2, "base_sim_2.csv", row.names = FALSE)

#Base sim 3
system.time({base_sim_3 <-simulation(pref, overall_k_l, overall_k_h, overall_n_l, overall_n_h, envi, neigh, emp, emp_high)})
```

    ##    user  system elapsed 
    ##   28.19    0.32   30.72

``` r
write.csv(base_sim_3, "base_sim_3.csv", row.names = FALSE)
```
