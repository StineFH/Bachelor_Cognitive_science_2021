######################################## ENVIRONMENT

``` r
environment <- data.frame(
  Postalcodes = c(8000, 8200, 8210, 8220, 8230, 8240, 8250, 8260, 8270, 8310, 8320, 
                   8330, 8340, 8355, 8361, 8380, 8381, 8462, 8471, 8520, 8530, 8541),
  Names = c("Aarhus C", "Aarhus N", "Aarhus V", "Brabrand", "Åbyhøj", "Risskov", "Egå", "Viby J",
             "Højbjerg", "Tranbjerg J", "Mårslet", "Beder", "Malling", "Solbjerg", "Hasselager", 
             "Trige", "Tilst", "Harlev J", "Sabro", "Lystrup", "Hjortshøj", "Skødstrup"),
  latitude = c(56.158150,56.181770, 56.173270, 56.158006, 56.152103, 56.202287, 56.226445, 56.130454, 56.109230, 56.089168, 56.072327, 56.061988, 56.032058, 56.031941, 56.102350, 56.268446, 56.188110, 56.144226, 56.215710, 56.236540, 56.278371, 56.270697),
  longitude = c(10.211870, 10.209010, 10.160669, 10.088876, 10.161023, 10.228380, 10.287823, 10.117336, 10.209614, 10.133464, 10.170561, 10.234584, 10.200015, 10.092101, 10.089191, 10.156009, 10.093488, 10.005717, 10.012770, 10.219074, 10.231889, 10.324122),
  Green_area_distance = NA,
  Public_transport_distance = NA,
  Min_Mall_distance = NA,
  School_rating = NA,
  Almenbolig_sd_rent = NA,
  Owner_oc_housing_m2 = NA,
  Perc_non_western_im_desc = NA
) 


#write.csv2(environment, "environment_aarhus.csv")
```

Add minimum distance to mall for each postcode

``` r
library(sf)
```

    ## Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1

``` r
stores_lat_long <- data.frame(
  mall = c("Storcenter Nord", "Veri Center", "Viby Center", "City Vest", "Bruuns Galleri", "Magasin", "Salling"),
  postal_code = c(8200, 8245, 8260, 8220, 8000, 8000, 8000),
  latitude = c(56.1706300000, 56.1890020000, 56.1294760000, 56.1543990000, 56.1492560000, 56.1569710000, 56.1544470000),
  longitude = c(10.1884980000, 10.2149350000, 10.1622630000, 10.1349250000, 10.2048710000, 10.2066400000, 10.2069360000)
)

#Calculate distances between postcodes and malls
postalcode_geom <- st_as_sf(environment, coords = c("latitude", "longitude"), crs = 4094)
stores_lat_long <- st_as_sf(stores_lat_long, coords = c("latitude", "longitude"), crs = 4094)
stores_distance <- st_distance(stores_lat_long, postalcode_geom)

stores_distance <- as.data.frame(stores_distance)

#Add the distance to the closest mall to each postecode 
environment$Min_Mall_distance <- apply(stores_distance, 2, min)
```

Add minimum distance to green area for each postcode

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.4     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.0

    ## Warning: package 'tibble' was built under R version 4.0.3

    ## Warning: package 'tidyr' was built under R version 4.0.3

    ## Warning: package 'readr' was built under R version 4.0.3

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
green_coord <- read_csv2("C:/Users/stine/OneDrive/Cognitive Science/5th_Semester/bachelor/ABM_Aarhus/green_areas_aarhus.csv")
```

    ## i Using ',' as decimal and '.' as grouping mark. Use `read_delim()` for more control.

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   Name = col_character(),
    ##   Type = col_character(),
    ##   Latitude = col_number(),
    ##   Longitude = col_number()
    ## )

``` r
#Add punctuation to have the correct format of coordinates
green_coord$Latitude <- gsub('^(.{2})(.*)$', '\\1.\\2', green_coord$Latitude)
green_coord$Longitude <- gsub('^(.{2})(.*)$', '\\1.\\2', green_coord$Longitude)

#Calculate distance from closest green area to each postcode
postalcode_geom <- st_as_sf(environment, coords = c("latitude", "longitude"), crs = 4094)
green_geom <- st_as_sf(green_coord, coords = c("Latitude", "Longitude"), crs = 4094)
green_distance <- st_distance(green_geom, postalcode_geom)

green_distance <- as.data.frame(green_distance)
#Add the distance to the closest mall to each postecode 
environment$Green_area_distance <- apply(green_distance, 2, min)
```

Add distance to public transport:

``` r
# Get distance between every postcode and 8000. Then rank them accordingly with the furthest away getting the highest number (22) and 8000 getting 0. 
postalcode_geom <- st_as_sf(environment, coords = c("latitude", "longitude"), crs = 4094)
transport_distance <- st_distance(postalcode_geom)
transport_distance <- transport_distance[,1]
transport_distance <- as.data.frame(transport_distance)
transport_distance$postcode <- environment$Postalcodes

transport_distance <- transport_distance[order(transport_distance$transport_distance),]
transport_distance$rank <- seq(1:22)

# Put the ranking in at the right positions 
i=1
j=1
for (i in 1:length(environment$Postalcodes)){
  for (j in 1:length(transport_distance$postcode)){
    if(environment[i,1]==transport_distance[j,2]){
      environment$Public_transport_distance[i] = transport_distance$rank[j]
    }
  }
}
```

Add ranking of schools (average ranking when more than 1 per district)

``` r
#import all school rankings
schools_ranked <- read_csv2("C:/Users/stine/OneDrive/Cognitive Science/5th_Semester/bachelor/ABM_Aarhus/schools_ranked.csv")
```

    ## i Using ',' as decimal and '.' as grouping mark. Use `read_delim()` for more control.

    ## Warning: Missing column names filled in: 'X5' [5]

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   Postalcode = col_double(),
    ##   Ranking = col_double(),
    ##   School = col_character(),
    ##   Notes = col_character(),
    ##   X5 = col_logical()
    ## )

``` r
schools_ranked$Postalcode <- as.character(schools_ranked$Postalcode)
schools_ranked$Postalcode <- as.factor(schools_ranked$Postalcode)

#Calculate mean for each district 
mean_ranking <-  aggregate(schools_ranked[, 2], list(schools_ranked$Postalcode), mean)
environment$School_rating <- mean_ranking$Ranking
```

Add price of housing

``` r
#Read all the files from Aarhusbolig
temp = list.files(pattern="a*.csv")
myfiles = lapply(temp, read.csv)

#Removing all "kr." 
res <- lapply(myfiles, function(x) gsub("[^0-9-]", "", x[, ncol(x)]))
#Setting to numeric
res <- lapply(res, function(x) as.numeric(x))

#Calculating the mean 
sd <- lapply(res, function(x) sd(x)) # Best measure because of big price differences within each postcode


sd <- as.data.frame(t(sd))

colnames(sd) <- "sd_rent"
sd$sd_rent <- as.numeric(sd$sd_rent)
sd$sd_rent <- round(sd$sd_rent, 3)


housing_prices <- data.frame(
  Postalcodes = c(8000, 8200, 8210, 8220, 8230, 8240, 8250, 8260, 8270, 8310, 8320, 
                   8330, 8340, 8355, 8361, 8380, 8381, 8462, 8471, 8520, 8530, 8541),
  Names = c("Aarhus C", "Aarhus N", "Aarhus V", "Brabrand", "Åbyhøj", "Risskov", "Egå", "Viby J",
             "Højbjerg", "Tranbjerg J", "Mårslet", "Beder", "Malling", "Solbjerg", "Hasselager", 
             "Trige", "Tilst", "Harlev J", "Sabro", "Lystrup", "Hjortshøj", "Skødstrup"),
  Almenbolig = sd,
  Avg_owner_oc_housing = c(42790, 25802, 23518, 21630, 30097, 37844, 26325, 25066, 32022, 18620, 21284, 22775, 18807, 15588, 18827, 14237, 18018, 16614, 14292, 20422, 16321, 20143)

)

#Beder Avg_owner_oc_housing set as average of the others 
# Avg_owner_oc_housing numbers from boliga's list over housing prices 

#Adding prices to df
environment$Almenbolig_sd_rent <- housing_prices$sd_rent
environment$Owner_oc_housing_m2 <- housing_prices$Avg_owner_oc_housing
```

Add diversity level

``` r
under_18 <- read.csv2("C:/Users/stine/OneDrive/Cognitive Science/5th_Semester/bachelor/ABM_Aarhus/indvandrere_aarhus_under_18.csv")
over_18 <- read.csv2("C:/Users/stine/OneDrive/Cognitive Science/5th_Semester/bachelor/ABM_Aarhus/indvandrere_aarhus_over_18.csv")

#Diversity level = ((descendants + non-western immigrants)/total number of residents in each postcode)*100
total <- under_18[,3:7] + over_18[,3:7] # Create dataframe with total amount 
total <- cbind(under_18[,1:2], total)

total$total <- total[,3]+total[,4]+total[,5]+total[,6]+total[,7] # total number of people in each district

total$diversity_level <- ((total$Efterkommere.fra.ikke.vestlige.lande + total$Indvandrere.fra.ikke.vestlige.lande)/total$total)*100 # Percentage of immigrant and descendants of non-western countries

total$diversity_vestlig <- ((total$Efterkommere.fra.vestlige.lande + total$Indvandrere.fra.vestlige.lande)/total$total)*100 # Percentage of immigrant and descendants of western countries

total$diversity_dansk <- ((total$Dansk.oprindelse)/total$total)*100 # Percentage of ethnic Danes 


environment$Perc_non_western_im_desc <- total[1:22,9]
```
