##-----------------------------------------------------
###   03-REGRESSION ANALYSIS-AMIRAH'S DISSERTATION
###----------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# In this script, we fit regression (logistic and linear regression) models
# that assess  one of the specific aims of Amirah's analysis for her dissertation.
# We evaluate the relationship between dietary scores from the NCI multifactor screener
# and food insecurity and also examine the relationship between QOL (from the FACTG survey)
# and food insecurity. All models adjust for relevant covariates.
#
# INPUT DATA FILES: 
# i. "../02-data-wrangled/01-data-scores.rds"
#
#
# OUTPUT FILES: 
# i. "../04-tables-figures/03-preliminary-results-diet.txt"
# ii "../04-tables-figures/04-preliminary-results-factg.txt"
#
# **A Special Note**: The data and tables are not being hosted on this GitHub repository given privacy concerns
# Relative paths are used for obtaining the data from a local folder on my machine.
#
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


library( tidyverse )
library( car ) # to compute variance inflation factor (VIF)

# load in helper functions
source( "R/utils.R" )

### (0.0) Read-in Wrangled Data ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

d <- readRDS( "../02-data-wrangled/01-data-scores.rds" )

# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### (1.0) Malnutrition Risk Models ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


term <- "fi_binary"


f.1 <- mal_nut ~ fi_binary + inc_pov_binary + age + pat_sex + food_assist_yn  + factg_fwb 

m1 <- d %>%
  mutate( mal_nut = ifelse( malnutrition_index == "At Risk", 1,
                            ifelse( malnutrition_index == "Not at Risk", 0, NA ) ) ) %>%


  glm( f.1, data = ., family = binomial )

r1 <- res_or( model.obj = m1, term = "fi_binary", y.var = "Malnutrition Index",
              stratum = "All" )  

# n = 211 with out disease stage and gets reduced to n = 165 when we add `disease_stage`

# on <=2 yrs since dx
m2 <- d %>%
  mutate( mal_nut = ifelse( malnutrition_index == "At Risk", 1,
                            ifelse( malnutrition_index == "Not at Risk", 0, NA ) ) ) %>%
  filter( yrs_since_dx <= 2 ) %>%
  
  
  glm( f.1, data = ., family = binomial )
  
  summary(m2)
  
  r2 <- res_or( model.obj = m2, term = "fi_binary", y.var = "Malnutrition Index",
                stratum = "<= 2 yrs Since Dx" )  
  
  # on 2-6yrs since dx
  m3 <- d %>%
    mutate( mal_nut = ifelse( malnutrition_index == "At Risk", 1,
                              ifelse( malnutrition_index == "Not at Risk", 0, NA ) ) ) %>%
    filter( yrs_since_dx > 2 &  yrs_since_dx < 6  ) %>%
    
    
    glm( f.1, data = ., family = binomial )
  
  summary(m3)
  
  r3 <- res_or( model.obj = m3, term = "fi_binary", y.var = "Malnutrition Index",
                stratum = "> 2 and < 6 yrs Since Dx " )  
  
  # > 6 yrs since diagnosis
  m4 <- d %>%
    mutate( mal_nut = ifelse( malnutrition_index == "At Risk", 1,
                              ifelse( malnutrition_index == "Not at Risk", 0, NA ) ) ) %>%
    filter( yrs_since_dx >= 6  ) %>%
    
    
    glm( f.1, data = ., family = binomial )
  
  summary(m4)
  
  r4 <- res_or( model.obj = m4, term = "fi_binary", y.var = "Malnutrition Index",
                stratum = "> 6 yrs Since Dx " )  
  
  
  # < 2 in the household
  m5 <- d %>%
    mutate( mal_nut = ifelse( malnutrition_index == "At Risk", 1,
                              ifelse( malnutrition_index == "Not at Risk", 0, NA ) ) ) %>%
    mutate( number_household = ifelse( number_household == 0 , 1, number_household ) ) %>% # 2 subjects with 0 replaced to 1
    filter( number_household < 2  ) %>%
    
    
    glm( f.1, data = ., family = binomial )
  
  summary(m5)
  
  r5 <- res_or( model.obj = m5, term = "fi_binary", y.var = "Malnutrition Index",
                stratum = "Household Size < 2" )  
  
  # >= 2 in the household
  m6 <- d %>%
    mutate( mal_nut = ifelse( malnutrition_index == "At Risk", 1,
                              ifelse( malnutrition_index == "Not at Risk", 0, NA ) ) ) %>%
    mutate( number_household = ifelse( number_household == 0 , 1, number_household ) ) %>% # 2 subjects with 0 replaced to 1
    filter( number_household >= 2  ) %>%
    
    
    glm( f.1, data = ., family = binomial )
  ## did not fit  as numerical probabilities equal to 1 or 0 occurred
  
  summary(m6)
  
  r6 <- res_or( model.obj = m6, term = "fi_binary", y.var = "Malnutrition Index",
              stratum = "Household Size >=2" )  
  
  
  # stratify on SNAP status as SNAP does not appear to be an important confounder in any of the model above
  f.2 <- mal_nut ~ fi_binary + inc_pov_binary + age + pat_sex + food_assist_yn  + factg_fwb
  
  m7 <- d %>%
    mutate( mal_nut = ifelse( malnutrition_index == "At Risk", 1,
                              ifelse( malnutrition_index == "Not at Risk", 0, NA ) ) ) %>%
    filter( snap_yn == "Yes" ) %>%
    
    
    glm( f.2, data = ., family = binomial )
  
  r7<- res_or(model.obj = m7, term = "fi_binary", y.var = "Malnutrition Index",
               stratum = "Receiving SNAP Benefits")    
  
  m8 <- d %>%
    mutate( mal_nut = ifelse( malnutrition_index == "At Risk", 1,
                              ifelse( malnutrition_index == "Not at Risk", 0, NA ) ) ) %>%
    filter( snap_yn == "No" ) %>%
    
    
    glm( f.2, data = ., family = binomial )
  
  r8 <- res_or(model.obj = m8, term = "fi_binary", y.var = "Malnutrition Index",
              stratum = "Not receiving SNAP Benefits" )  
  
  
  # stratify on multimodal/single or no treatment
  
  m9 <- d %>%
    mutate( mal_nut = ifelse( malnutrition_index == "At Risk", 1,
                              ifelse( malnutrition_index == "Not at Risk", 0, NA ) ) ) %>%
    filter( multi_modal == "multimodal" ) %>%
    
    
    glm( f.2, data = ., family = binomial )
  
  r9 <- res_or(model.obj = m9, term = "fi_binary", y.var = "Malnutrition Index",
               stratum = "Multimodal Treatment" )  
  
  m10 <- d %>%
    mutate( mal_nut = ifelse( malnutrition_index == "At Risk", 1,
                              ifelse( malnutrition_index == "Not at Risk", 0, NA ) ) )  %>%
    filter( multi_modal == "single or no tx" ) %>%
    
    
    glm( f.2, data = ., family = binomial )
  
  r10 <- res_or(model.obj = m10, term = "fi_binary", y.var = "Malnutrition Index",
               stratum = "Single or no Treatment" )  
  
  # combine results for malnutrition index
  
  malnut.mod <- rbind( r1, r2, r3, r4, r5, r6, r7, r8, r9, r10 )
    
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  
  
  ### (2.0) FACT-G Risk Models ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
 # model specifications
  f.3 <-  fi_binary ~ I(factg_total/sd(d$factg_total,na.rm=T) ) + inc_pov_binary + age + pat_sex + food_assist_yn  + malnutrition_index + disease_stage 
  f.4 <-  fi_binary ~ I(factg_ewb/sd(d$factg_ewb,na.rm=T) )  + inc_pov_binary + age + pat_sex + food_assist_yn  + malnutrition_index + disease_stage 
  f.5 <-  fi_binary ~ I(factg_pwb/sd(d$factg_pwb,na.rm=T) )  + inc_pov_binary + age + pat_sex + food_assist_yn  + malnutrition_index + disease_stage 
  f.6 <-  fi_binary ~ I(factg_fwb/sd(d$factg_fwb,na.rm=T) )  + inc_pov_binary + age + pat_sex + food_assist_yn  + malnutrition_index + disease_stage 
  f.7 <-  fi_binary ~ I(factg_swb/sd(d$factg_swb,na.rm=T) )  + inc_pov_binary + age + pat_sex + food_assist_yn  + malnutrition_index + disease_stage 
  
  # data to use
  list.sub <- list( d,
                    d %>% filter( yrs_since_dx <= 2 ),
                    d %>%   filter( yrs_since_dx > 2 &  yrs_since_dx < 6  ),
                    d %>%  mutate( number_household = ifelse( number_household == 0 , 1, number_household ) ) %>% # 2 subjects with 0 replaced to 1
                      filter( number_household < 2  ),
                    d %>%  mutate( number_household = ifelse( number_household == 0 , 1, number_household ) ) %>% # 2 subjects with 0 replaced to 1
                      filter( number_household >= 2  ),
                    d %>% filter( multi_modal == "multimodal" ),
                    d %>% filter( multi_modal == "single or no tx" ) )
  
  table.stratum <- c( "All", "<= 2 yrs Since Dx", "> 2 and < 6 yrs Since Dx ",
                      "Household Size < 2", "Household Size >= 2",
                      "Multimodal Treatment", "Single or No Treatment")
  
  # initialize dataframes to store results
  out.total <- data.frame()
  out.pwb <- data.frame()
  out.ewb <- data.frame()
  out.fwb <- data.frame()
  out.swb <- data.frame()
  
for( i in seq_along( list.sub ) ){
    

  # fit models
    m.total <- list.sub[[i]] %>%
    mutate( fi_binary = ifelse( fi_binary == "High FI", 1, 
                                ifelse( fi_binary == "Low FI", 0, NA ))) %>%
    glm( f.3, data = . )
  
  m.ewb <- list.sub[[i]] %>%
    mutate( fi_binary = ifelse( fi_binary == "High FI", 1, 
                                ifelse( fi_binary == "Low FI", 0, NA ))) %>%
    glm( f.4, data = . )
  
  m.pwb <- list.sub[[i]] %>%
    mutate( fi_binary = ifelse( fi_binary == "High FI", 1, 
                                ifelse( fi_binary == "Low FI", 0, NA ))) %>%
    glm( f.5, data = . )
  
  m.fwb <- list.sub[[i]] %>%
    mutate( fi_binary = ifelse( fi_binary == "High FI", 1, 
                                ifelse( fi_binary == "Low FI", 0, NA ))) %>%
    glm( f.6, data = . )
  
  m.swb <- list.sub[[i]] %>%
    mutate( fi_binary = ifelse( fi_binary == "High FI", 1, 
                                ifelse( fi_binary == "Low FI", 0, NA ))) %>%
    glm( f.7, data = . )
  
  # tabulate model reults
  r.total <- res_lr(model.obj = m.total, term = "factg_total", y.var = "Food Insecurity Status",
               stratum = table.stratum[i], logistic = T )  
  
  r.ewb <- res_lr(model.obj = m.ewb, term = "factg_ewb", y.var = "Food Insecurity Status",
                    stratum = table.stratum[i], logistic = T )  
  
  r.pwb <- res_lr(model.obj = m.pwb, term = "factg_pwb", y.var = "Food Insecurity Status",
                  stratum = table.stratum[i], logistic = T )  
  
  r.fwb <- res_lr(model.obj = m.fwb, term = "factg_fwb", y.var = "Food Insecurity Status",
                  stratum = table.stratum[i], logistic = T )  
  
  r.swb <- res_lr(model.obj = m.swb, term = "factg_swb", y.var = "Food Insecurity Status",
                  stratum = table.stratum[i], logistic = T )  

  
  out.total <- rbind( out.total, r.total )
  out.ewb <- rbind( out.ewb, r.ewb )
  out.pwb <- rbind( out.pwb, r.pwb )
  out.fwb <- rbind( out.fwb, r.fwb )
  out.swb <- rbind( out.swb, r.swb )
  
  }
  
  # bind all results into final table
  out.factg <- rbind( out.total, out.ewb, out.pwb, out.fwb, out.swb )
  rownames( out.factg ) <- NULL
  
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  
  ### (3.0) Fiber Risk Models ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------

  
  f.8 <- pred.fiber ~ fi_binary + inc_pov_binary + age + pat_sex + food_assist_yn  + malnutrition_index + disease_stage 
  
  
  g1 <- lm( f.8, data = d )  
  j1 <- res_lr(model.obj = g1, term = "fi_binary", y.var = "Predicted Fiber Intake",
               stratum = "All" )  
  
  g2 <- lm( f.8, data = d %>% filter( yrs_since_dx <= 2 )  )  
  j2 <- res_lr(model.obj = g2, term = "fi_binary", y.var = "Predicted Fiber Intake",
               stratum = "<= 2 yrs Since Dx" ) 
  
  g3 <- lm( f.8, data = d %>%   filter( yrs_since_dx > 2 &  yrs_since_dx < 6  )  )  
  j3 <- res_lr(model.obj = g3, term = "fi_binary", y.var = "Predicted Fiber Intake",
               stratum = "> 2 and < 6 yrs Since Dx " ) 
  
  
  g4 <- lm( f.8, data = d %>%   filter( yrs_since_dx >= 6   )  )  
  j4 <- res_lr(model.obj = g4, term = "fi_binary", y.var = "Predicted Fiber Intake",
               stratum = ">=6 yrs Since Dx " ) 
  
  g5 <- lm( f.8, data = d %>%  mutate( number_household = ifelse( number_household == 0 , 1, number_household ) ) %>% # 2 subjects with 0 replaced to 1
              filter( number_household < 2  )  )  
  j5 <- res_lr(model.obj = g5, term = "fi_binary", y.var = "Predicted Fiber Intake",
               stratum = "Household Size < 2" ) 
  
  g6 <- lm( f.8, data = d %>%  mutate( number_household = ifelse( number_household == 0 , 1, number_household ) ) %>% # 2 subjects with 0 replaced to 1
              filter( number_household >= 2  )  )  
  j6 <- res_lr(model.obj = g6, term = "fi_binary", y.var = "Predicted Fiber Intake",
               stratum = "Household Size >= 2" ) 
  
  g7 <- lm( f.8, data = d %>% 
              filter( multi_modal == "multimodal"  )  )  
  j7 <- res_lr(model.obj = g7, term = "fi_binary", y.var = "Predicted Fiber Intake",
               stratum = "Multimodal Treatment" ) 
  
  g8 <- lm( f.8, data = d %>%  
              filter(  multi_modal == "single or no tx"  )  )  
  j8 <- res_lr(model.obj = g8, term = "fi_binary", y.var = "Predicted Fiber Intake",
               stratum = "Single or No Treatment" ) 
  
  fib.mod <- rbind( j1, j2, j3, j4, j5, j6, j7, j8 )
  
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  ### (4.0) Fruit and Veg Models ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  f.9 <- pred.fv6.ce ~ fi_binary + inc_pov_binary + age + pat_sex + food_assist_yn  + malnutrition_index + disease_stage 
  
  
  g1 <- lm( f.9, data = d )  
  j1 <- res_lr(model.obj = g1, term = "fi_binary", y.var = "Age/Sex-Adjusted F & V Intake (Cups)",
               stratum = "All" )  
  
  g2 <- lm( f.9, data = d %>% filter( yrs_since_dx <= 2 )  )  
  j2 <- res_lr(model.obj = g2, term = "fi_binary", y.var = "Age/Sex-Adjusted F & V Intake (Cups)",
               stratum = "<= 2 yrs Since Dx" ) 
  
  g3 <- lm( f.9, data = d %>%   filter( yrs_since_dx > 2 &  yrs_since_dx < 6  )  )  
  j3 <- res_lr(model.obj = g3, term = "fi_binary", y.var = "Age/Sex-Adjusted F & V Intake (Cups)",
               stratum = "> 2 and < 6 yrs Since Dx " ) 
  
  
  g4 <- lm( f.9, data = d %>%   filter( yrs_since_dx >= 6   )  )  
  j4 <- res_lr(model.obj = g4, term = "fi_binary", y.var = "Age/Sex-Adjusted F & V Intake (Cups)",
               stratum = ">=6 yrs Since Dx " ) 
  
  g5 <- lm( f.9, data = d %>%  mutate( number_household = ifelse( number_household == 0 , 1, number_household ) ) %>% # 2 subjects with 0 replaced to 1
              filter( number_household < 2  )  )  
  j5 <- res_lr(model.obj = g5, term = "fi_binary", y.var = "Age/Sex-Adjusted F & V Intake (Cups)",
               stratum = "Household Size < 2" ) 
  
  g6 <- lm( f.9, data = d %>%  mutate( number_household = ifelse( number_household == 0 , 1, number_household ) ) %>% # 2 subjects with 0 replaced to 1
              filter( number_household >= 2  )  )  
  j6 <- res_lr(model.obj = g6, term = "fi_binary", y.var = "Age/Sex-Adjusted F & V Intake (Cups)",
               stratum = "Household Size >= 2" ) 
  
  g7 <- lm( f.9, data = d %>%  mutate( number_household = ifelse( number_household == 0 , 1, number_household ) ) %>% # 2 subjects with 0 replaced to 1
              filter( multi_modal == "multimodal"  )  )  
  j7 <- res_lr(model.obj = g7, term = "fi_binary", y.var = "Age/Sex-Adjusted F & V Intake (Cups)",
               stratum = "Multimodal Treatment" ) 
  
  g8 <- lm( f.9, data = d %>%
              filter(  multi_modal == "single or no tx"  )  )  
  j8 <- res_lr(model.obj = g8, term = "fi_binary", y.var = "Age/Sex-Adjusted F & V Intake (Cups)",
               stratum = "Single or No Treatment" ) 
  
  fv.mod <- rbind( j1, j2, j3, j4, j5, j6, j7, j8 )
  
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  ### (5.0) % from Fat Models ###
  # ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
  f.10 <- pred.pcf ~ fi_binary + inc_pov_binary + age + pat_sex + food_assist_yn  + malnutrition_index + disease_stage 
  
  
  g1 <- lm( f.10, data = d )  
  j1 <- res_lr(model.obj = g1, term = "fi_binary", y.var = "% KCAL From Fat",
               stratum = "All" )  
  
  g2 <- lm( f.10, data = d %>% filter( yrs_since_dx <= 2 )  )  
  j2 <- res_lr(model.obj = g2, term = "fi_binary", y.var = "% KCAL From Fat",
               stratum = "<= 2 yrs Since Dx" ) 
  
  g3 <- lm( f.10, data = d %>%   filter( yrs_since_dx > 2 &  yrs_since_dx < 6  )  )  
  j3 <- res_lr(model.obj = g3, term = "fi_binary", y.var = "% KCAL From Fat",
               stratum = "> 2 and < 6 yrs Since Dx " ) 
  
  
  g4 <- lm( f.10, data = d %>%   filter( yrs_since_dx >= 6   )  )  
  j4 <- res_lr(model.obj = g4, term = "fi_binary", y.var = "% KCAL From Fat",
               stratum = ">=6 yrs Since Dx " ) 
  
  g5 <- lm( f.10, data = d %>%  mutate( number_household = ifelse( number_household == 0 , 1, number_household ) ) %>% # 2 subjects with 0 replaced to 1
              filter( number_household < 2  )  )  
  j5 <- res_lr(model.obj = g5, term = "fi_binary", y.var = "% KCAL From Fat",
               stratum = "Household Size < 2" ) 
  
  g6 <- lm( f.10, data = d %>%  mutate( number_household = ifelse( number_household == 0 , 1, number_household ) ) %>% # 2 subjects with 0 replaced to 1
              filter( number_household >= 2  )  )  
  j6 <- res_lr(model.obj = g6, term = "fi_binary", y.var = "% KCAL From Fat",
               stratum = "Household Size >= 2" ) 
  
  g7 <- lm( f.10, data = d %>%  
              filter( multi_modal == "multimodal"  )  )  
  j7 <- res_lr(model.obj = g7, term = "fi_binary", y.var = "% KCAL From Fat",
               stratum = "Multimodal Treatment" ) 
  
  g8 <- lm( f.10, data = d %>%
              filter(  multi_modal == "single or no tx"  )  )  
  j8 <- res_lr(model.obj = g8, term = "fi_binary", y.var = "% KCAL From Fat",
               stratum = "Single or No Treatment" ) 
  
    fat.mod <- rbind( j1, j2, j3, j4, j5, j6, j7, j8 )
  
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  
    
    
### (6.0) Save Results ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

  diet.and.malnut.mod <- rbind( malnut.mod,
                    fib.mod, fv.mod, fat.mod)
  
  write.table( diet.and.malnut.mod, "../04-tables-figures/03-preliminary-results-diet.txt", sep = "," )
  write.table( out.factg, "../04-tables-figures/04-preliminary-results-factg.txt", sep = "," )
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  