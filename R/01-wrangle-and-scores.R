library( tidyverse )
library( rvest )      # for webscraping html tables

source( "R/utils.R")

### Read-in Raw Data ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

d.raw <- read.csv( "../01-data-raw/raw_data.csv")

View(d.raw)

# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### Some Data Wrangling ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

## Change all " '' " to NAs

d.raw[ d.raw == "" ] <- NA

## Fix Income Variable (i.e., recode old survey version levels to levels on new survey) ##

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




### Unit Conversions ( Raw Input to servings/day ) ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


# first change milk variable to four separate milk variables (skim, 1%, and 2%)

d.2 <- d.1 %>% # for those that have a milk entry that is not missing, they will get "0" for all other milk types not the ones that they consume
  ## note: there are some individuals with missing `milk_used` that have an entry in `milk`
  mutate( milk_used = ifelse( is.na( milk_used ) & !is.na( milk ), "1% Fat", milk_used ),
          
          # now create the milk-specific columns (4 columns total)
          milk_skim = ifelse( milk_used == "Non-Fat or Skim", milk, "Never" ),
          milk_1p = ifelse( milk_used == "1% Fat", milk, "Never"  ),
          milk_2p = ifelse( milk_used == "2% Fat", milk, "Never"  ),
          milk_whole = ifelse( milk_used == "Whole Milk", milk, "Never"  ) ) %>%
  select( -milk )

## First Conversion
vars.1 <- names( d.2[ c( 56, 58:71, 223:226 )])
  
d.2[ , vars.1 ]                          #this shows the above diet variables are present in the combined dataset


## Create a flag variable for showing those missing at least one diet/food variable ##

# this will allow us to keep track of new variables that are subsequently created #
# and ensure quality control #

d.2$flag_diet <- 0 # initialize flag variable

for( i in 1:length( vars.1 ) ){
  
  d.2$flag_diet <- d.2$flag_diet + as.numeric( is.na( d.2[, vars.1[i] ] ) )
  
  d.2$flag_diet <- ifelse( d.2$flag_diet >= 1, 1, d.2$flag_diet ) # ensure it is "1" or "0"

}

# > table( d.2$flag_diet)
# 
# 0   1
# 222  12
# 12 with at least one diet variable missing



## Now do unit conversions ##

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



## Portion Size Adjustments ##
### (0.0) Web Scrape for HTML Tables ###
url <-  "https://epi.grants.cancer.gov/past-initiatives/open/multifactor/scoring.html#scoring"

# web scrape the link above for the HTML tables that have all the conversion factors
df.list <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table( fill = T ) 

## Fat and Fiber Analysis ##

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




### Adjustments ###


#get column indices
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


## adjust fruit/veg frequency of food intake by gender/age specific factors ##

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
  

## Compute pyramid serving units variables ##

 d.4 <- d.3 %>%
   mutate( fv7 = fruit_m + vegetables_m + juice_m + potatoes_m + white_potatoes_m + salad_m + beans_m,
           fv6 = fruit_m + vegetables_m + juice_m + white_potatoes_m + salad_m + beans_m, # remove fried potatoes
           sqfv7 = sqrt( fv7 ),
           sqfv6 = sqrt( fv6 ) ) 
 


## Adjust food frequency by gender/age specific factors ##
 
# this will generate
 
# it will use tables 2 (for males) and 3 (for females) to make the conversions
 
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
 
 
## create predicted outcomes ##
 
 d.5 <- d.5 %>%
   mutate( predfv7ps = ifelse( pat_sex == "Male", 0.90679 + 0.75856*sqfv7,
                               ifelse( pat_sex == "Female", 0.81956 + 0.73086*sqfv7, NA ) ),
           predfv6ps = ifelse( pat_sex == "Male", 0.94077 + 0.73906*sqfv6,
                               ifelse( pat_sex == "Female", 0.81626 + 0.73022*sqfv6, NA ) ) )
 
 
## use table  6 from `df.list` to create predicted fiber intake and % from fat
 
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
 
 
    