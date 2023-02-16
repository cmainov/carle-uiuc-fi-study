##------------------------------------------------------------
###   01-DATA-WRANGLING, DIET SCORES, AND OTHER SCALES
###------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# In this script, we will import The raw data, fix the income variable and then
# compute the diet scores from the variables in the NCI Multifactor Screener  
# (see resources below). We will scrape the HTML tables from their website, containing
# the adjustment multipliers and use them for the score formulations.
#
# Additionally, we will compute the FACT-G subscales and total scale (See documentation link below),
# recode food insecurity variables/status, and compute the CHAOS score (See documentation link below).
#
# INPUT DATA FILES: 
# i. "../01-data-raw/raw_data.csv"
# ii. HTML tables scraped from link in `Resources` below this (these will be stored
# in a list names `df.list` and will be accessed regularly throughout the script)
#
#
# OUTPUT DATA FILE: "../02-data-wrangled/01-diet-scores.rds"
#
# **A Special Note**: The data is not being hosted on this GitHub repository given privacy concerns
# Relative paths are used for obtaining the data from a local folder on my machine.
#
#
# Resources (Accessed 12 February 2023): 
# i. NCI Multifactor Screener info, tables, and SAS code: https://epi.grants.cancer.gov/past-initiatives/open/multifactor/scoring.html
# ii. FACT-G info and documentation: https://www.facit.org/measures/FACT-G 
# iii. USDA 6-item FI screener info and documentation: https://www.ers.usda.gov/media/8282/short2012.pdf 
# iv. CHAOS scale, original paper: https://doi.org/10.1016/0193-3973(95)90028-4
# v. Financial skills index (Gundersen et al.): https://doi.org/10.3945/jn.112.162214
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


library( tidyverse )
library( rvest )      # for webscraping html tables

source( "R/utils.R")

### (0.0) Read-in Raw Data ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

d.raw <- read.csv( "../01-data-raw/raw_data.csv")

# Again, note data are not hosted publicly on the repository. They are stored on my
# local machine.

# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### (1.0) Some Data Wrangling ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

## (1.1) Change all " '' " to NAs

d.raw[ d.raw == "" ] <- NA

  ## --------- End Subsection --------- ##


## (1.2) Fix Income Variable (i.e., recode old survey version levels to levels on new survey) ##

table( d.raw$pat_income)
table( d.raw$pat_income_new)

old.inc <- levels( as.factor( d.raw$pat_income ) )
new.inc <- levels( as.factor( d.raw$pat_income_new ) )

# subset those that don't have new income variable
d.1 <-( d.old <- d.raw %>%
  filter( consent_version == "Prior to V6" & is.na( pat_income_new ) ) %>%
  mutate( pat_income_new = ifelse( pat_income == old.inc[1], new.inc[3],
                                   ifelse( pat_income == old.inc[2], new.inc[2],
                                           ifelse( pat_income == old.inc[3], new.inc[4],
                                                   ifelse( pat_income == old.inc[4], new.inc[3], ## this one is problematic
                                                           ifelse( pat_income == old.inc[5], new.inc[5],
                                                                   ifelse( pat_income == old.inc[6], new.inc[5],
                                                                           ifelse( pat_income == old.inc[7], new.inc[5],
                                                                                   ifelse( pat_income == old.inc[8], new.inc[7],
                                                                                           ifelse( pat_income == old.inc[9], new.inc[1],
                                                                                                   ifelse( pat_income == old.inc[10], new.inc[8],
                                                                                                           ifelse( pat_income == old.inc[11], new.inc[8],
                                                                                                                   ifelse( pat_income == old.inc[12], new.inc[9], NA )))))))))))))) %>%
  rbind( ., d.raw %>% filter( d.raw$record_id %notin% d.old$record_id ) ) %>%
  select(-pat_income) # remove old income column

table( d.1$pat_income_new ) # check

# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### (2.0) Unit Conversions ( Raw Input to servings/day ) ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


## (2.1) First change milk variable to four separate milk variables (skim, 1%, and 2%) ##

d.2 <- d.1 %>% # for those that have a milk entry that is not missing, they will get "0" for all other milk types not the ones that they consume
  ## note: there are some individuals with missing `milk_used` that have an entry in `milk`
  mutate( milk_used = ifelse( is.na( milk_used ) & !is.na( milk ), "1% Fat", milk_used ),
          
          # now create the milk-specific columns (4 columns total)
          milk_skim = ifelse( milk_used == "Non-Fat or Skim", milk, "Never" ),
          milk_1p = ifelse( milk_used == "1% Fat", milk, "Never"  ),
          milk_2p = ifelse( milk_used == "2% Fat", milk, "Never"  ),
          milk_whole = ifelse( milk_used == "Whole Milk", milk, "Never"  ) ) %>%
  select( -milk )

## --------- End Subsection --------- ##


## (2.2) Variables to convert units for

vars.1 <- names( d.2[ c( 56, 58:71, 223:226 )])
  
d.2[ , vars.1 ]                          #this shows the above diet variables are present in the combined dataset

## --------- End Subsection --------- ##


## (2.3) Create a flag variable for showing those missing at least one diet/food variable ##

# this will allow us to keep track of new variables that are subsequently created #
# and ensure quality control #

d.2$flag_diet <- 0 # initialize flag variable

for( i in 1:length( vars.1 ) ){
  
  d.2$flag_diet <- d.2$flag_diet + as.numeric( is.na( d.2[, vars.1[i] ] ) )
  
  d.2$flag_diet <- ifelse( d.2$flag_diet >= 1, 1, d.2$flag_diet ) # ensure it is "1" or "0"

}

# 12 with at least one diet variable missing

## --------- End Subsection --------- ##


## (2.4) Now do unit conversions ##

# levels 
fr <- levels( as.factor( d.2$chip ) )

#get column indices
these.1 <- which( colnames( d.2 )%in% vars.1 )

# Use for-loop for conversion

for ( i in these.1 ){
  d.2[i] <- ifelse( d.2[i] == fr[1], 1,
                  ifelse( d.2[i] == fr[2], 0.214,
                          ifelse( d.2[i] == fr[3], 0.067, 
                                  ifelse( d.2[i] == fr[4], 2, 
                                          ifelse( d.2[i] == fr[5], 3, 
                                                  ifelse( d.2[i] == fr[6], 0.5, 
                                                          ifelse( d.2[i] == fr[7], 4.5, 
                                                                  ifelse( d.2[i] == fr[8], 0.786, 
                                                                          ifelse( d.2[i] == fr[9], 0, NA ) ) ) ) ) ) ) ) )
}

# check and ensure fidelity
d.2[ , these.1 ]

# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### (3.0) Web Scrape for HTML Tables Containing Adjustment factors ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

url <-  "https://epi.grants.cancer.gov/past-initiatives/open/multifactor/scoring.html#scoring"

## (3.1) Web scrape the link above for the HTML tables that have all the conversion factors ##
df.list <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table( fill = T ) 

## --------- End Subsection --------- ##


## (3.2) Convert food group names to those present in working dataset ##
for( i in 2:length(df.list)){
  if( i %in% 2:5){
df.list[[i]]$`Food Group` <- ifelse( str_detect(df.list[[i]]$`Food Group`, "juice"), "juice", 
                                     ifelse(str_detect(df.list[[i]]$`Food Group`, "cereals"), "cold_cereals",
                                            ifelse(str_detect(df.list[[i]]$`Food Group`, "Bacon"), "bacon_sausage", 
                                                   ifelse( str_detect(df.list[[i]]$`Food Group`, "dogs"), "hot_dogs", 
                                                           ifelse( str_detect(df.list[[i]]$`Food Group`, "Fruit"), "fruit", 
                                                                   ifelse( str_detect(df.list[[i]]$`Food Group`, "dressing"), "regular_fat",
                                                                           ifelse( str_detect(df.list[[i]]$`Food Group`, "Salad \\("), "salad",
                                                                                   ifelse( str_detect(df.list[[i]]$`Food Group`, "bread"), "bread", 
                                                                                           ifelse( str_detect(df.list[[i]]$`Food Group`, "Fried"), "potatoes",
                                                                                                   ifelse( str_detect(df.list[[i]]$`Food Group`, "white pot"), "white_potatoes",
                                                                                                           ifelse( str_detect(df.list[[i]]$`Food Group`, "beans"), "beans",
                                                                                                                  ifelse( str_detect(df.list[[i]]$`Food Group`, "vegetable"), "vegetables",
                                                                                                                          ifelse( str_detect(df.list[[i]]$`Food Group`, "Pasta"), "pasta", 
                                                                                                                                  ifelse( str_detect(df.list[[i]]$`Food Group`, "Nuts"), "nuts",
                                                                                                                                          ifelse( str_detect(df.list[[i]]$`Food Group`, "2%"), "milk_2p",
                                                                                                                                                  ifelse( str_detect(df.list[[i]]$`Food Group`, "Chips"), "chips",
                                                                                                                                                  ifelse( str_detect(df.list[[i]]$`Food Group`, "1%"), "milk_1p",
                                                                                                                                                          ifelse( str_detect(df.list[[i]]$`Food Group`, "Skim"), "milk_skim",
                                                                                                                                                                  ifelse( str_detect(df.list[[i]]$`Food Group`, "Whole milk"), "milk_whole", df.list[[i]]$`Food Group` ) ))))))))))))))))))
  }
  
  if( i %in% 6:8){
    df.list[[i]]$Parameter <- ifelse( str_detect(df.list[[i]]$Parameter, "juice"), "juice", 
                                         ifelse(str_detect(df.list[[i]]$Parameter, "cereals"), "cold_cereals",
                                                ifelse(str_detect(df.list[[i]]$Parameter, "Bacon"), "bacon_sausage", 
                                                       ifelse( str_detect(df.list[[i]]$Parameter, "dogs"), "hot_dogs", 
                                                               ifelse( str_detect(df.list[[i]]$Parameter, "Fruit"), "fruit", 
                                                                       ifelse( str_detect(df.list[[i]]$Parameter, "dressing"), "regular_fat",
                                                                               ifelse( str_detect(df.list[[i]]$Parameter, "Salad (P3)"), "salad",
                                                                                       ifelse( str_detect(df.list[[i]]$Parameter, "bread"), "bread", 
                                                                                               ifelse( str_detect(df.list[[i]]$Parameter, "Fried"), "potatoes",
                                                                                                       ifelse( str_detect(df.list[[i]]$Parameter, "white pot"), "white_potatoes",
                                                                                                               ifelse( str_detect(df.list[[i]]$Parameter, "beans"), "beans",
                                                                                                                       ifelse( str_detect(df.list[[i]]$Parameter, "vegetable"), "vegetables",
                                                                                                                               ifelse( str_detect(df.list[[i]]$Parameter, "Pasta"), "pasta", 
                                                                                                                                       ifelse( str_detect(df.list[[i]]$Parameter, "Nuts"), "nuts",
                                                                                                                                               ifelse( str_detect(df.list[[i]]$Parameter, "2%"), "milk_2p",
                                                                                                                                                       ifelse( str_detect(df.list[[i]]$Parameter, "Chips"), "chips",
                                                                                                                                                               ifelse( str_detect(df.list[[i]]$Parameter, "1%"), "milk_1p",
                                                                                                                                                                       ifelse( str_detect(df.list[[i]]$Parameter, "Skim"), "milk_skim",
                                                                                                                                                                               ifelse( str_detect(df.list[[i]]$Parameter, "Whole milk"), "milk_whole", df.list[[i]]$Parameter ) ))))))))))))))))))
    
  }
}

# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### (4.0) Adjustment of Food Items by Age/Gender Factors ###
## Creates 1/2 cup pyramid serving units (predfv7 predfv6) ##
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


## (4.1) Get column indices ##
these.2 <- which( colnames( d.2 ) %in% vars.1 )

# age list
age.lst <- list( c( 0:17 ),
      c( 18:27 ),
      c( 28:37 ),
      c( 38:47 ),
      c( 48:57 ),
      c( 58:67 ),
      c( 68:77 ),
      c( 78:99 ) )


# copy before adjustments 
d.3 <- d.2

## --------- End Subsection --------- ##


## (4.2) Adjust fruit/veg frequency of food intake by gender/age specific factors ##

# this generates results for 1/2 cup pyramid serving units (predfv7 predfv6) #

# we will use the 4th HTML table in `df.list` (i.e., df.list[[4]])

for( i in 1:nrow( d.3 ) ){  # loop on subject
  
  for( j in 2:length( age.lst) ){ # loop on age which determines which columns of reference table to use
    
    ## inner loops will be determined based on which rows to use from the table
    
    # males inner loop
    if( d.3[ i, "age" ] %in% age.lst[[j]] & d.3[ i, "pat_sex" ] == "Male" ){
      
      for( g in 3:9){
        d.3[ i, paste0( df.list[[4]]$`Food Group`[g],"_m" ) ] <-
          d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ]*as.numeric(df.list[[4]][g,j])
      }
    }
  
  # females inner loop
  if( d.3[ i, "age" ] %in% age.lst[[j]] & d.3[ i, "pat_sex" ] == "Female" ){
    
    for( g in 11:17){
      d.3[ i, paste0( df.list[[4]]$`Food Group`[g],"_m" ) ] <-
        d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ]*as.numeric(df.list[[4]][g,j])
    }
  }
  }
}
  # end loop

## --------- End Subsection --------- ##


## (4.3) Compute 1/2 cup pyramid serving units variables ##

 d.4 <- d.3 %>%
   mutate( fv7 = fruit_m + vegetables_m + juice_m + potatoes_m + white_potatoes_m + salad_m + beans_m,
           fv6 = fruit_m + vegetables_m + juice_m + white_potatoes_m + salad_m + beans_m, # remove fried potatoes
           sqfv7 = sqrt( fv7 ),
           sqfv6 = sqrt( fv6 ),
           ## create predicted outcomes ##
           predfv7ps = ifelse( pat_sex == "Male", 0.90679 + 0.75856*sqfv7,
                               ifelse( pat_sex == "Female", 0.81956 + 0.73086*sqfv7, NA ) ),
           predfv6ps = ifelse( pat_sex == "Male", 0.94077 + 0.73906*sqfv6,
                               ifelse( pat_sex == "Female", 0.81626 + 0.73022*sqfv6, NA ) ) )
 
 
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
 

 
 
### (5.0) Compute `pred.fiber` and `pred.pcf` (Age and Sex-Specific) ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
 
## (5.1) Age-sex adjustment
 
# it will use tables 2 (for males) and 3 (for females) to make the conversions from `df.list`
 
 d.5 <- d.4 # copy before looping and alterating
 for( i in 1:nrow( d.5 ) ){  # loop on subject
   
   for( j in 2:length( age.lst) ){ # loop on age which determines which columns of reference table to use
     
     ## inner loops will be determined based on which rows to use from the table
     
     # males inner loop
     if( d.5[ i, "age" ] %in% age.lst[[j]] & d.5[ i, "pat_sex" ] == "Male" ){
       
       for( g in 3:20){ # loop on all food items this time
         d.5[ i, paste0( df.list[[2]]$`Food Group`[g],"_a" ) ] <-
           d.5[ i, which(colnames( d.5 ) == df.list[[2]]$`Food Group`[g]) ]*as.numeric(df.list[[2]][g,j])
       }
     }
     
     # females inner loop
     if( d.5[ i, "age" ] %in% age.lst[[j]] & d.5[ i, "pat_sex" ] == "Female" ){
       
       for( g in 3:20){
         d.5[ i, paste0( df.list[[3]]$`Food Group`[g],"_a" ) ] <-
           d.5[ i, which(colnames( d.5 ) == df.list[[3]]$`Food Group`[g]) ]*as.numeric(df.list[[3]][g,j])
       }
     }
   }
 }
 
 ## --------- End Subsection --------- ##
 
 
## (5.2) Use table  6 from `df.list` to create predicted fiber intake and % from fat ##
 
 d.6 <- d.5 %>% # copy data before looping and alternating
   
   # initialize variables with intercept values
   mutate( pred.fiber = ifelse( pat_sex == "Male", as.numeric( df.list[[6]][2,3] ),
                                ifelse( pat_sex == "Female", as.numeric( df.list[[6]][2,5] ), NA ) ),
           pred.pcf = ifelse( pat_sex == "Male", as.numeric( df.list[[6]][2,2] ),
                              ifelse( pat_sex == "Female", as.numeric( df.list[[6]][2,4] ), NA ) ) )

 for( i in 1:nrow( d.6 ) ){  # loop on subject

     

       for( g in 3:20){ # loop on all food items this time
         
         ## males
         if( d.6[ i, "pat_sex" ] == "Male" ){
           
           # predicted fiber
         d.6[ i, "pred.fiber" ] <-
           d.6[ i, which(colnames( d.6 ) == paste0( df.list[[6]]$`Parameter`[g], "_a" ) ) ]*
           as.numeric(df.list[[6]][g,3]) + d.6[ i, "pred.fiber" ]
         
         # predicted % from fat
         d.6[ i, "pred.pcf" ] <-
           d.6[ i, which(colnames( d.6 ) == paste0( df.list[[6]]$`Parameter`[g], "_a" ) ) ]*
           as.numeric(df.list[[6]][g,2]) + d.6[ i, "pred.pcf" ]
         }
         
         ## females
         if( d.6[ i, "pat_sex" ] == "Female" ){
           
           # predicted fiber
           d.6[ i, "pred.fiber" ] <-
             d.6[ i, which(colnames( d.6 ) == paste0( df.list[[6]]$`Parameter`[g], "_a" ) ) ]*
             as.numeric(df.list[[6]][g,5]) + d.6[ i, "pred.fiber" ]
           
           # predicted % from fat
           d.6[ i, "pred.pcf" ] <-
             d.6[ i, which(colnames( d.6 ) == paste0( df.list[[6]]$`Parameter`[g], "_a" ) ) ]*
             as.numeric(df.list[[6]][g,4]) + d.6[ i, "pred.pcf" ]
         }
       }
 }
 
 # ---------------------------------------------------------------------------------------------------------------------------------------------------------
 
 
 
 
### (6.0) Adjustment of Food Items by Age/Gender Factors ###
 ## Creates cup-equivalent pyramid serving units (`raw.pred.fv7.ce`, `raw.pred.fv6.ce`, `pred.fv7.ce`, `pred.fv6.ce`) ##
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
 
## (6.1) Create age category variable
for ( i in 1:nrow( d.6 ) ){
  
  for ( j in 2:length( age.lst ) ){
    
    if( d.6[i, "age"] %in% age.lst[[j]] ){
      
      d.6[i, "age_cat"] <- j-1
      
    }
    
  }
}
 
 ## --------- End Subsection --------- ##
 

## (6.2) Input F/V cup equivalent adjustments, from fran.predict.nhis.fortim.cupeq.txt provided by Lisa Kahle 06/15/2007 ##

fvcupadj <- tibble::tribble(
     ~"gender", ~"AgeGrp", ~"FVCAFruit", ~"FVCAFrtJ", ~"FVCAFrFry", ~"FVCAOthPot", ~"FVCASalad", ~"FVCAOthVeg", ~"FVCADrBean",
    1, 1, 0.999580, 1.499160, 0.721125, 1.000400, 0.272700, 0.387675, 0.717550, 
    1, 2, 0.933450, 1.250580, 0.727700, 1.140030, 0.353970, 0.473920, 0.551540, 
    1, 3, 0.867300, 1.000980, 0.641000, 0.999600, 0.377235, 0.499840, 0.566720, 
    1, 4, 0.867300, 1.000980, 0.641000, 0.999600, 0.374963, 0.500240, 0.612360, 
    1, 5, 0.867300, 1.000176, 0.548055, 0.999490, 0.416640, 0.499905, 0.500250, 
    1, 6, 0.774916, 0.750735, 0.480750, 0.833175, 0.375000, 0.460585, 0.502285, 
    1, 7, 0.657060, 0.750735, 0.499980, 0.754400, 0.411323, 0.416899, 0.575360, 
    2, 1, 0.749235, 1.124370, 0.509595, 0.782020, 0.306788, 0.364468, 0.492150, 
    2, 2, 0.867300, 1.000960, 0.455110, 0.876945, 0.286335, 0.395882, 0.341550, 
    2, 3, 0.844838, 1.000176, 0.448700, 0.771260, 0.416625, 0.404303, 0.430530, 
    2, 4, 0.789970, 0.938130, 0.448700, 0.771260, 0.499950, 0.408330, 0.345763, 
    2, 5, 0.742350, 0.764776, 0.394856, 0.749700, 0.397688, 0.416913, 0.430685, 
    2, 6, 0.712640, 0.750728, 0.444260, 0.771260, 0.312469, 0.436560, 0.430530, 
    2, 7, 0.620475, 0.750434, 0.444260, 0.644235, 0.374963, 0.452214, 0.500400
) %>%
  mutate( gender = ifelse( gender == 1, "Male",
                           ifelse( gender == 2, "Female", NA ) ) )


## (6.3) Merge and create variables ## 

# merge portion size adjustment variables to working dataset and multiply to 
# estimate cup equivalents of F & V (`raw.pred.fv7.ce`, `raw.pred.fv6.ce`) and 
# adjust the estimates using regression coefficients from CSFII 94-96 
# (`pred.fv7.ce`, `pred.fv6.ce`)

# this adjustment uses table 8 from `df.list` (i.e., `df.list[[8]]`)

d.7 <- d.6 %>%
  left_join( ., fvcupadj, by = c( "pat_sex" = "gender", "age_cat" = "AgeGrp" ) ) %>%
  mutate( raw.pred.fv7.ce = juice*FVCAFrtJ + fruit*FVCAFruit + potatoes*FVCAFrFry
          + white_potatoes*FVCAOthPot + beans*FVCADrBean + salad*FVCASalad
          + vegetables*FVCAOthVeg,
          
          raw.pred.fv6.ce = juice*FVCAFrtJ + fruit*FVCAFruit
          + white_potatoes*FVCAOthPot + beans*FVCADrBean + salad*FVCASalad
          + vegetables*FVCAOthVeg, # excludes french fries
          
          # adjust using regression coefficients from CSFII 94-96
          
          pred.fv7.ce = ifelse( pat_sex == "Male", 
                                ( 0.666228 + 0.770652*( sqrt(raw.pred.fv7.ce) ) )^2,
                                ifelse( pat_sex == "Female", 
                                        ( 0.611844 + 0.733890*( sqrt(raw.pred.fv7.ce) ) )^2,NA ) ),
          pred.fv6.ce = ifelse( pat_sex == "Male", 
                                ( 0.706696 + 0.742255*( sqrt(raw.pred.fv6.ce) ) )^2,
                                ifelse( pat_sex == "Female", 
                                        ( 0.616033 + 0.727761*( sqrt(raw.pred.fv6.ce) ) )^2,NA ) ) )



# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### (7.0) Return Final Dataset and Save ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

## (7.1) Columns to return in final dataset ##
cols.keep <- c( which( colnames( d.7 ) %in% colnames( d.2 ) ), # original data columns
                which( colnames( d.7 ) %in% c( "pred.fiber", "pred.pcf",
                                               "pred.fv7.ce", "pred.fv6.ce",
                                               "raw.pred.fv7.ce", "raw.pred.fv6.ce",
                                               "predfv7ps", "predfv6ps") )
)

## --------- End Subsection --------- ##


## (7.2) Save ##
( d.8 <- d.7[, cols.keep ] ) 

## --------- End Subsection --------- ##


## (7.3 New column descriptions ##

# pred.fiber = predicted predicted percentage of energy from fiber (%)
# pred.pcf = predicted percentage of calories from fat (%)
# pred.fv7.ce = predicted F & V cup equivalent MyPyramid units, including french fries, adjusted for age and gender
# pred.fv6.ce = predicted F & V cup equivalent MyPyramid units, excluding french fries, adjusted for age and gender
# raw.pred.fv7.ce = predicted F & V cup equivalent MyPyramid units, including french fries, not adjusted for age and gender
# raw.pred.fv6.ce = predicted F & V cup equivalent MyPyramid units, excluding french fries, not adjusted for age and gender
# predfv7ps = predicted F & V 1/2 cup pyramid serving units, including french fries,
# predfv6ps = predicted F & V 1/2 cup pyramid serving units, excluding french fries,

# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### (8.0) FACT-G Scoring ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


## (8.1) Subset column/survey item names based on FACT-G Domain ##

fg.swb <- c( "friends", "family", "friends_support", "fami_accept",
             "satisfied_comm", "clost_to_partner", "sex_life" )

fg.ewb <- c( "sad", "satisfied_coping", "losing_hope", "nervous", "worry_dying",
             "worry_get_worse" )

fg.fwb <- c( "work", "work_fulfilling", "enjoy_life", "accepted_illness", "sleeping_well",
             "fun", "content_qol" )

fg.pwb <- c( "energy", "nausea", "family_needs", "pain", "side_effects",
             "ill", "bed" )

## --------- End Subsection --------- ##


## (8.2) Set to 0 those that chose not to respond to sexual activity item ##

d.9 <- d.8 %>%
  mutate( sex_life = ifelse( ( sex_life == "Prefer not to answer" |
                                 !is.na( sex_activity_answer) ), 0, sex_life ))

sum(is.na( d.9$sex_life)) # check how many missing; 97 subjects

## --------- End Subsection --------- ##


## (8.3) Convert character scores to numeric ##

# subset the FACT-G questionnaire columns only
fg.sub <- d.9[, c( fg.swb,
         fg.ewb,
         fg.fwb,
         fg.pwb ) ]

# assign numerical values based on level
fg.sub[ fg.sub=="Not at all" ] <- 0
fg.sub[ fg.sub=="A little bit" ] <- 1
fg.sub[ fg.sub=="Somewhat" ] <- 2
fg.sub[ fg.sub=="Quite a bit" ] <- 3
fg.sub[ fg.sub=="Very much" ] <- 4

# reassign those columns back to the dataset

d.9[, c( fg.swb,
         fg.ewb,
         fg.fwb,
         fg.pwb ) ] <- fg.sub

## --------- End Subsection --------- ##


## (8.4) Reverse coding items that need to be reverse coded ##

# columns that need reveral
rvs.items <- c( fg.pwb, 
                fg.ewb[ !fg.ewb == 'satisfied_coping' ] )

# subset array to perform reversal on
fg.rvs.it <- d.9[ , rvs.items ]

# apply reversal function
fg.rvs.it.2 <- sapply( fg.rvs.it, function(x) abs( as.numeric( x ) - 4) )

d.10 <- d.9 # copy dataset

# replace unreserved values with reversed
d.10[ , rvs.items ] <- fg.rvs.it.2

## --------- End Subsection --------- ##


## (8.6) No. of answered items in each survey for each participant ##

for ( i in 1:nrow( d.10 ) ){
  
  d.10[ i, "answers.pwb" ] <- length( fg.pwb) - sum( is.na( unlist( d.10[ i, fg.pwb ] ) ) )
  d.10[ i, "answers.fwb" ] <- length( fg.fwb) - sum( is.na( unlist( d.10[ i, fg.fwb ] ) ) )
  d.10[ i, "answers.swb" ] <- length( fg.swb) - sum( is.na( unlist( d.10[ i, fg.swb ] ) ) )
  d.10[ i, "answers.ewb" ] <- length( fg.ewb) - sum( is.na( unlist( d.10[ i, fg.ewb ] ) ) )
  
  
}

## --------- End Subsection --------- ##


## (8.5) Compute subscales and final score ##

# convert all FACT-G columns to numeric prior to final computations
these <- which( colnames( d.10 ) %in% c( fg.swb,
                              fg.ewb,
                              fg.fwb,
                              fg.pwb ) )

for( i in these ){
  
  d.10[, i ] <- as.numeric( d.10[, i ] )
  
}

# do final computations
d.11 <- d.10 %>%
  mutate( factg_pwb = ( ( energy + nausea + family_needs + pain + side_effects + ill +
            bed )*7 ) / answers.pwb,
          
          factg_ewb = ( ( sad + satisfied_coping + losing_hope + nervous + 
            worry_dying + worry_get_worse )*6 ) / answers.ewb,
          
          factg_swb = ( ( friends + family + friends_support + fami_accept +
            satisfied_comm + clost_to_partner + sex_life )*7 ) / answers.swb,
          
          factg_fwb = ( ( work + work_fulfilling + enjoy_life + accepted_illness +
            sleeping_well + fun + content_qol )*7 ) / answers.fwb,
          
          factg_total = factg_ewb + factg_swb + factg_pwb + factg_fwb )

## (8.6) New column descriptions ##

# factg_pwb = FACT-G physical well-being subscale score
# factg_ewb = FACT-G emotional well-being subscale score
# factg_swb = FACT-G social well-being subscale score
# factg_fwb = FACT-G functional well-being subscale score
# factg_total = FACT-G total scale score


# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### (9.0) Food Insecurity Categorization ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

## (9.1) Find column indicators for variables from FI screener ##
these.fs <- which( str_detect( colnames( d.11 ), "hfss" ) )

## --------- End Subsection --------- ##


## (9.2) Convert those columns to binary and numeric affirmative/non-affirmative ##

# subset the array of interest
fs.sub <- d.11[ these.fs ]

# code the responses to binary
fs.sub[ fs.sub=="Sometimes true" ] <- 1 # "1" for affirmative
fs.sub[ fs.sub=="Often true" ] <- 1 # "1" for affirmative
fs.sub[ fs.sub=="Never true" ] <- 0 # "0" for not in the affirmative
fs.sub[ fs.sub=="Yes" ] <- 1 # "1" for affirmative
fs.sub[ fs.sub=="No" ] <- 0 # "0" for not in the affirmative
fs.sub[ "hfss_4" ] <- ifelse( !is.na( fs.sub[ "hfss_4" ] ), 1, 0) # if they have an entry here they get a 1 otherwise a 0

# copy the working dataset
d.12 <- d.11

# replace array in dataset with recoded array
d.12[ these.fs ] <- fs.sub

## --------- End Subsection --------- ##


## (9.3) Compute food insecurity scale ##

# convert food insecurity columns to numeric before computations of scales
for( i in these.fs ){
  
  d.12[, i ] <- as.numeric( d.12[, i ] )
  
}

# compute food insecurity variables
d.13 <- d.12 %>%
  mutate( fi_scale = hfss_1 + hfss_2 + hfss_3 + hfss_4 + hfss_5 +
            hfss_6,
          fi_status = as.factor( ifelse( fi_scale == 0, "High FI",
                              ifelse( fi_scale == 1, "Marginal FI",
                                      ifelse( fi_scale %in% 2:4, "Low FI",
                                              ifelse( fi_scale %in% 5:6, "Very low FI", 
                                                      NA) ) ) ) ),
          fi_binary = as.factor( ifelse( fi_scale %in% 0:1, "High FI",
                                         ifelse( fi_scale %in% 2:6, "Low FI", NA ) ) ) ) 

## --------- End Subsection --------- ##


## (9.4) New column descriptions ##

# fi_scale = raw score from FS screener tool
# fi_status = 4-level food insecurity factor variable coded into high food insecurity, marginal FI, low FI, and very low FI
# fi_binary = 2-level food insecurity factor variable coded into high FI and low FI

# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### (10.0) CHAOS Score ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

## NOTE: The Confusion, Hubbub, and ORder Scale (CHAOS) is derived by a simple sum of the 15 T/F items 
## see: Matheny et al. (1995)--linked in the resources of the preamble of this R script


## (10.1) Column indicators of variables needed for scale ##

these.chaos <- which( colnames( d.13 ) %in% c( "commotion", "find_things", 
                                             "rushed", "stay_on_top",
                                             "running_late", "zoo", "talk_with", 
                                             "fuss", "does_not_work", "think", 
                                             "arguments", "relax", "telephone", 
                                             "calm", "routine" ) )

## --------- End Subsection --------- ##


## (10.2) Convert those columns to numerical values based on documentation ##

# subset the array of interest
ch.sub <- d.13[ these.chaos ]

# recode the responses
ch.sub[ ch.sub=="Very much like your own home"] <- 1
ch.sub[ ch.sub=="Somewhat like your own home"] <- 2
ch.sub[ ch.sub=="A little like your own home"] <- 3
ch.sub[ ch.sub=="Not at all like your own home"] <- 4

# copy working dataset
d.14 <- d.13

# replace array in dataset with recoded array
d.14[ these.chaos ] <- ch.sub

## --------- End Subsection --------- ##


## (10.3) Compute CHAOS score as simple sum  ##

# convert item columns to numeric before computing final score
for( i in these.chaos ){
  
  d.14[, i ] <- as.numeric( d.14[, i ] )
  
}

# compute final score
d.15 <- d.14 %>%
  mutate( chaos_score = commotion + find_things + rushed +
            stay_on_top + running_late + zoo + talk_with + fuss +
            does_not_work + think + arguments + relax + telephone +
            calm + routine )

## --------- End Subsection --------- ##


## (10.4) New column descriptions ##

# chaos_score = CHAOS scale score

# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### (11.0) Financial MGMT Skills Index ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

## SEE: https://doi.org/10.3945/jn.112.162214 for how index is computed


## (11.1) Column indicators of variables needed for scale ##

these.financial <- which( colnames( d.13 ) %in% c( "review_bills", "bills_on_time",
                                               "budget", "review_income" ) )
# NOTE: missing one variable (credit bill pay on time?)


## (10.2) Convert those columns to numerical values based on documentation ##

# subset the array of interest
fin.sub <- d.15[ these.financial ]

# recode the responses
fin.sub[ fin.sub=="Usually" ] <- 1
fin.sub[ fin.sub=="Always" ] <- 1
fin.sub[ fin.sub=="Never" ] <- 0
fin.sub[ fin.sub=="Sometimes" ] <- 0
fin.sub[ fin.sub=="Rarely" ] <- 0

# copy working dataset
d.16 <- d.15

# replace array in dataset with recoded array
d.16[ these.financial ] <- fin.sub


## (11.3) Compute financial skills score as simple sum  ##

# convert item columns to numeric before computing final score
for( i in these.financial ){
  
  d.16[, i ] <- as.numeric( d.16[, i ] )
  
}

# compute final score
d.17 <- d.16 %>%
  mutate( financial_skills_index = review_bills + bills_on_time +
            budget + review_income )

## --------- End Subsection --------- ##


## (11.4) New column descriptions ##

# financial_skills_index = financial skills index (see Gundersen et al. cited in preamble)

# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### (12.0) Other Data Wrangline ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

## (12.1) Ethnicity and Race Variables ##
d.18 <- d.17 %>%
  mutate( pat_ethnicity = ifelse( pat_ethnicity___1 == "Checked","Mexican/Mexican-American/Chicano",
                                  ifelse( pat_ethnicity___2 == "Checked", "Puerto Rican",
                                          ifelse( pat_ethnicity___3=="Checked", "Cuban",
                                                  ifelse( pat_ethnicity___4 == "Checked", "Other Hispanic origin",
                                                          ifelse( pat_ethnicity___5 == "Checked", "None of these", NA) ) ) ) ),
          pat_race_ethnicity = ifelse( pat_ethnicity %in% c( "Mexican/Mexican-American/Chicano",
                                                     "Puerto Rican",
                                                     "Cuban",
                                                     "Other Hispanic origin" ), "Hispanic",
                               ifelse( pat_race == "Caucasian or White" & pat_ethnicity %notin% c( "Mexican/Mexican-American/Chicano",
                                                                                               "Puerto Rican",
                                                                                               "Cuban",
                                                                                               "Other Hispanic origin" ), "white",
                                       ifelse( pat_race == "Black or African-American", "Black or African-American", 
                                               ifelse( pat_race == "Other", "Other",
                                                       ifelse( pat_race == "Asian (i.e. original people of the Far East, Southeast Asia, and Indian subcontinent)",
                                                               "Asian", NA ) ) ) ) ),
          pat_race_ethnicity_b = ifelse( pat_race_ethnicity =="Black or African-American", "Black or African-American",
                                         ifelse( pat_race_ethnicity %in% c( "Hispanic", "Asian" ), "Other",
                                                 ifelse( pat_race_ethnicity == "white", "white", NA ) ) ) )

## --------- End Subsection --------- ##


## (12.2) Fix age at Diagnosis (some subjects put year of Dx instead of date) ## 

d.19 <- d.18 %>%
  mutate( year_dos = as.numeric( str_extract( d.16$date, "\\d\\d\\d\\d(?=\\-)" ) ), # year of survey date
    age_diagnosis = ifelse( age_diagnosis > 1000, age - ( year_dos - age_diagnosis ), age_diagnosis ),
    yrs_since_dx = age - age_diagnosis ) %>%
  select( -year_dos )

## --------- End Subsection --------- ##


## (12.3) Treatment type, recode smoking/drinking statuses ##

d.20 <- d.19 %>%
  
  # treatment type
  mutate( tx_type = ifelse( type_treatment___1=="Checked", "Surgery",
                            ifelse( type_treatment___2=="Checked", "Chemotherapy",
                                    ifelse( type_treatment___3=="Checked", "Radiation",
                                            ifelse( type_treatment___4=="Checked", "Hormone Therapy",
                                                    ifelse( type_treatment___5=="Checked", "Don't Know",
                                                            ifelse( type_treatment___6=="Checked", "Other", NA )))))),
          # recode drinking status into three categories
          pat_alcohol_recode = ifelse( pat_alcohol_drinking == "I drank alcohol in the past, but quit drinking over a year ago",
                                       "Former",
                                       ifelse( pat_alcohol_drinking == "I drank alcohol in the past, but quit drinking with the last 1 month",
                                               "Former",
                                               ifelse( pat_alcohol_drinking == "I drank alcohol in the past, but quit drinking with the last 6 months",
                                                       "Former",
                                                       ifelse( pat_alcohol_drinking == "I drank alcohol in the past, but quit drinking within the last year",
                                                               "Former",
                                                               ifelse( pat_alcohol_drinking == "I have never drunk alcohol",
                                                                       "Never",
                                                                       ifelse( pat_alcohol_drinking == "I currently drink alcohol",
                                                                               "Current", NA ) ) ) ) ) ),
         
           # recode smoking status into three categories
          pat_smoking_recode = ifelse( pat_smoking_status == "I have smoked in the past but quit over a year ago",
                                       "Former",
                                       ifelse( pat_smoking_status == "I have smoked in the past but quit within the last 1 month",
                                               "Former",
                                               ifelse( pat_smoking_status == "I have smoked in the past but quit within the last 6 months",
                                                       "Former",
                                                       ifelse( pat_smoking_status == "I have smoked in the past but quit within the last year",
                                                               "Former",
                                                               ifelse( pat_smoking_status == "I have never smoked",
                                                                       "Never",
                                                                       ifelse( pat_smoking_status == "I currently smoke cigarettes",
                                                                               "Current", NA ) ) ) ) ) ) )
                                                                               
                                                                       
                                                               

sum( table( d.20$pat_smoking_recode) )

## (12.4) New column descriptions ##

# yrs_since_dx = years since cancer diagnosis
# pat_smoking_recode = recode smoking status into 3 categories (current, former, never)
# pat_drinking_recode = recode drinking status into 3 categories (current, former, never)
# pat_race_ethnicity_b = 3 category race/ethnicity
# pat_race_ethnicity = 4 category race/ethnicity
# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### (14.0) Cancer Stage Data Wrangling ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

## (14.1) Read in data ##
stage.raw <- read.csv( "../01-data-raw/cancerstage-data.csv" )
# it looks like this column is very unstructured and will requires lots of wrangling

## --------- End Subsection --------- ##


## (14.2) Clean up the column ##
stage.col <- stage.raw$disease_stage
str_replace_all( stage.col, fixed(" "), "") # remove all white spaces

stage.col[ str_detect( stage.col, "III") ] <- "III"
stage.col[ str_detect( stage.col, "IV") ] <- "IV"
stage.col[ str_detect( stage.col, "^II$") ] <- "II"
stage.col[ str_detect( stage.col, "^II[ A-HJ-UW-Z]$") ] <- "II" # all stage two categorizations not followed by "V", "I"
stage.col[ str_detect( stage.col, "^I$") ] <- "I"
stage.col[ str_detect( stage.col, "^I[ A-HJ-UW-Z]$") ] <- "I" # all stage one categorizations not followed by "V", "I"
stage.col[ str_detect( stage.col, "T") ] <- NA # those with TNM classification set to missing
stage.col[ str_detect( stage.col, "^1") ] <- "I"
stage.col[ str_detect( stage.col, "^2") ] <- "II"
stage.col[ str_detect( stage.col, "^3") ] <- "III"
stage.col[ str_detect( stage.col, "^4") ] <- "IV"

#else 
stage.col[ stage.col %notin% c( "0", "I","II", "III", "IV")] <- NA

## --------- End Subsection --------- ##

## (14.3) Save ##
stage.raw$disease_stage <-  stage.col

d.21 <- d.20 %>%
  left_join(., stage.raw )

## --------- End Subsection --------- ##


## (14.4) New column descriptions ##

# disease_stage = cancer stage (0-IV)

# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### (15.0) Malnutrition ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

## (15.1) Change malnutrition variables to numeric based on table##
d.22 <- d.21 %>%
  mutate( lost_weight = ifelse( lost_weight == "No", 0,
                                ifelse( lost_weight == "Yes", 1,
                                        ifelse( lost_weight == "Unsure", 2, NA))),
          eating_poorly = ifelse( eating_poorly == "Yes", 1,
                                  ifelse( eating_poorly == "No", 0, NA ) ),
          weight_amount = ifelse( is.na(weight_amount), 0,
                                  ifelse( weight_amount == "2-13 lb", 1,
                                    ifelse( weight_amount == "14-23 lb", 2,
                                            ifelse( weight_amount == "24-33 lb", 3,
                                                    ifelse( weight_amount == "34 lb or more", 4,
                                                            ifelse( weight_amount == "Unsure", 5, NA ))))) ),
          malnutrition_score = lost_weight + eating_poorly + weight_amount,
          malnutrition_index = ifelse( malnutrition_score >= 2, "At Risk",
                                       ifelse( malnutrition_score < 2, "Not at Risk",
                                       NA ) ) )

## --------- End Subsection --------- ##


## (15.2) Poverty calculation

# based on https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines 
levs.inc <- levels( factor( d.22$pat_income_new))

d.23 <- d.22 %>%
  mutate( poverty_line = ifelse( number_household == 1 & pat_income_new %in% levs.inc[c(1,2)], 1,
                                 ifelse( number_household == 2 & pat_income_new %in% levs.inc[c(1,2,4)], 1,
                                         ifelse( number_household == 3 & pat_income_new %in% levs.inc[c(1,2,4)], 1,
                                                 ifelse( number_household == 4 & pat_income_new %in% levs.inc[c(1,2,4,5)], 1,
                                                         ifelse( number_household == 5 & pat_income_new %in% levs.inc[c(1,2,4,5)], 1,
                                                                 ifelse( number_household == 6 & pat_income_new %in% levs.inc[c(1,2,4,5)], 1,
                                                                         ifelse( number_household == 7 & pat_income_new %in% levs.inc[c(1,2,4,5)], 1,
                                                                                 ifelse( number_household == 8 & pat_income_new %in% levs.inc[c(1,2,4,5,7)], 1,
                                                                                         ifelse( is.na(pat_income_new) | is.na( number_household ), NA, 0 ))))))))))
## --------- End Subsection --------- ##


## (15.3) New column descriptions ##

# malnutrition_index = binary malnutrition index
# malnutrition_score = malnutrition index score used to create binary indicator
# poverty_line = poverty status indicator (1 == below poverty line)
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

saveRDS( d.23, "../02-data-wrangled/01-data-scores.rds" )
write.csv( d.23, "../02-data-wrangled/01-data-scores.csv")
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

