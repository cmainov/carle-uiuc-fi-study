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
  mutate( milk_skim = ifelse( milk_used == "Non-Fat or Skim", milk, "Never" ),
          milk_1p = ifelse( milk_used == "1% Fat", milk, "Never"  ),
          milk_2p = ifelse( milk_used == "2% Fat", milk, "Never"  ),
          milk_whole = ifelse( milk_used == "Whole Milk", milk, "Never"  ) ) %>%
  select( -milk )

## First Conversion
vars.1 <- names( d.2[ c( 56, 58:71, 223:226 )])
  
d.2[ , vars.1 ]                          #this shows the above diet variables are present in the combined dataset


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
                                                                           ifelse( str_detect(df.list[[i]]$`Food Group`, "Salad (P3)"), "salad",
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


# 
# *adjust fruit/veg frequency of food intake by gender/age specific factors;      
# * this generates results for 1/2 cup pyramid serving units (predfv7 predfv6);  


#get column indices
these.2 <- which( colnames( d.2 )%in% vars.1 )

# copy before adjustments 
d.3 <- d.2

for( i in 1:nrow( d.3 ) ){
  
  for( j in these.2 ){
    
    ## 18-27
    if( d.3[ i, "age" ] %in% 18:27 & d.3[ i, "pat_sex" ] == "Male" ){
      
      for( g in 3:9){
        d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ] <-
          d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ]*as.numeric(df.list[[4]][g,2])
      }
      }
    
    
    if( d.3[ i, "age" ] %in% 18:27 & d.3[ i, "pat_sex" ] == "Female" ){
      
      for( g in 11:17){
        d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ] <-
          d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ]*as.numeric(df.list[[4]][g,2])
      }
      }

  
    
    ## 28-37
    if( d.3[ i, "age" ] %in% 28:37 & d.3[ i, "pat_sex" ] == "Male" ){
      
      for( g in 3:9){
        d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ] <-
          d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ]*as.numeric(df.list[[4]][g,3])
      }
      }

    
    if( d.3[ i, "age" ] %in% 28:37 & d.3[ i, "pat_sex" ] == "Female" ){
      
      for( g in 11:17){
        d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ] <-
          d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ]*as.numeric(df.list[[4]][g,3])
      }
      }
  
    
    
    ## 38-47
    if( d.3[ i, "age" ] %in% 38:47 & d.3[ i, "pat_sex" ] == "Male" ){
      
      for( g in 3:9){
        d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ] <-
          d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ]*as.numeric(df.list[[4]][g,4])
      }
      }
  
    
    if( d.3[ i, "age" ] %in% 38:47 & d.3[ i, "pat_sex" ] == "Female" ){
      
      for( g in 11:17){
        d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ] <-
          d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ]*as.numeric(df.list[[4]][g,4])
      }
      }
    
    
    
    ## 48-57
    if( d.3[ i, "age" ] %in% 48:57 & d.3[ i, "pat_sex" ] == "Male" ){
      
      for( g in 3:9){
        d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ] <-
          d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ]*as.numeric(df.list[[4]][g,5])
      }
      }
    
    
    if( d.3[ i, "age" ] %in% 48:57 & d.3[ i, "pat_sex" ] == "Female" ){
      
      for( g in 11:17){
        d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ] <-
          d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ]*as.numeric(df.list[[4]][g,5])

      }
    }
    
    
    ## 58-67
    if( d.3[ i, "age" ] %in% 58:67 & d.3[ i, "pat_sex" ] == "Male" ){
      
      for( g in 3:9){
        d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ] <-
          d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ]*as.numeric(df.list[[4]][g,6])
      }
    }
    
    if( d.3[ i, "age" ] %in% 58:67  & d.3[ i, "pat_sex" ] == "Female" ){
      
      for( g in 11:17){
        d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ] <-
          d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ]*as.numeric(df.list[[4]][g,6])
      }
      }
    
    
    
    ## 68-77
    if( d.3[ i, "age" ] %in% 68:77 & d.3[ i, "pat_sex" ] == "Male" ){
      
      for( g in 3:9){
        d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ] <-
          d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ]*as.numeric(df.list[[4]][g,7])
      }
      }
    
    
    if( d.3[ i, "age" ] %in% 58:67 & d.3[ i, "pat_sex" ] == "Female" ){
      
      for( g in 11:17){
        d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ] <-
          d.3[ i, which(colnames( d.3 ) == df.list[[4]]$`Food Group`[g]) ]*as.numeric(df.list[[4]][g,7])
      }
      }
    }
  }
View(d.2[,vars.1])
