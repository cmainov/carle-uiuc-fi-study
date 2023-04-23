######################################################
# Bar plots Similar to Jagsi Paper and Correlations  #
# Between Dim 2 and Other Continuous Variables       #
######################################################

library( tidyverse )
library( ggpubr )
library( Hmisc )

d <- readRDS( "../03-data-rodeo/01-data-mca-recat.rds" ) %>%
  # recode levels for mark all that apply questions
  mutate( how_pay_out_of_pocket___1 = ifelse( how_pay_out_of_pocket___1 == "Checked", 
                                              "I used my income and/or savings", 
                                              how_pay_out_of_pocket___1 ),
          how_pay_out_of_pocket___2 = ifelse( how_pay_out_of_pocket___2 == "Checked", 
                                              "I borrowed money from family or friends", 
                                              how_pay_out_of_pocket___2 ),
          how_pay_out_of_pocket___3 = ifelse( how_pay_out_of_pocket___3 == "Checked", 
                                              "I borrowed money against my house", 
                                              how_pay_out_of_pocket___3 ),
          how_pay_out_of_pocket___4 = ifelse( how_pay_out_of_pocket___4 == "Checked", 
                                              "I left some of my medical bills unpaid", 
                                              how_pay_out_of_pocket___4 ),
          how_pay_out_of_pocket___5 = ifelse( how_pay_out_of_pocket___5 == "Checked", 
                                              "I increased my credit card debt", 
                                              how_pay_out_of_pocket___5 ),
          how_pay_out_of_pocket___6 = ifelse( how_pay_out_of_pocket___6 == "Checked", 
                                              "Other", 
                                              how_pay_out_of_pocket___6 ),
          
          # dichotomize "How much is this due to your cancer?" questions
          health_insurance_effect = ifelse( health_insurance_effect %in% c( "Quite a bit", "Very much"), "Quite a bit/Very much",
                                            ifelse( health_insurance_effect %in% c( "A little", "Somewhat","Not at all"), "Little/Somewhat/Not at all",
                                                    health_insurance_effect ) ),
          employment_status_effect = ifelse( employment_status_effect %in% c( "Quite a bit", "Very much"), "Quite a bit/Very much",
                                             ifelse( employment_status_effect %in% c( "A little", "Somewhat","Not at all"), "Little/Somewhat/Not at all",
                                                     employment_status_effect ) ),
          financial_status_effect = ifelse( financial_status_effect %in% c( "Quite a bit", "Very much"), "Quite a bit/Very much",
                                            ifelse( financial_status_effect %in% c( "A little", "Somewhat","Not at all"), "Little/Somewhat/Not at all",
                                                    financial_status_effect ) ),
          
          # re-code pat_treatment into binary, yes/no, variable for treatment status
          pat_treatment = ifelse( pat_treatment %in% c( "Currently receiving treatment for a cancer",
                                                        "Receiving treatment for cancer that has returned" ), "Receiving treatment",
                                  ifelse( pat_treatment %in% c( "I am not receiving treatment" ), "Not receiving treatment",
                                          pat_treatment ) ) )




### Bar Plot for Worse Financial Effect ###

# get p-value for plot
t.fin <- table( d$financial_status_effect, d$fi_binary )
p.fin <- round( chisq.test( t.fin, simulate.p.value = T )$p.value, 2 )
p.fin <- ifelse( p.fin == 1, "0.99", p.fin ) # to avoid 1's
p.fin <- str_replace( p.fin, "(\\.\\d)$", "\\10" ) # for significant digits

# generate plot
fin.plot <- d %>%
  filter( !is.na( financial_status_effect ) & !is.na( fi_binary ) ) %>%
  ggplot() +
  geom_bar( aes( fi_binary, fill = factor( financial_status_effect ) ),
            width = 0.35 ) +
  scale_fill_manual( values = c( "#F0E442", "#0072B2" ), # set manual colors
                     breaks = c( "Little/Somewhat/Not at all",
                                 "Quite a bit/Very much" ) ) +
  theme_classic() +
  theme( text = element_text( family = "Avenir" ),
         legend.position = c( 0.8, 0.8 ),
         legend.title = element_blank(),
         axis.title.x = element_blank(),
         axis.text.x = element_text( size = 12, face = "bold" ),
         axis.title.y = element_text( size = 12, face = "bold" ),
         plot.title = element_text( face = "italic" ) ) +
  ylab( "Frequency" ) + 
  annotate("text", x = 1.9, y = 35, 
           label = paste0( "italic(p) ==", p.fin ), # add annotated p-value with only p italicized
           parse = T,
           family = "Avenir",
           size = 5 ) +
  ggtitle( "Worse Financial Status Due to Cancer Diagnosis" )



### Bar Plot for Worse Health Insurance Effect ###

# get p-value for plot
t.hi <- table( d$health_insurance_effect, d$fi_binary )
p.hi <- round( fisher.test( t.hi, simulate.p.value = T )$p.value, 2 )
p.hi <- ifelse( p.hi == 1, "0.99", p.hi ) # to avoid 1's
p.hi <- str_replace( p.hi, "(\\.\\d)$", "\\10" ) # for significant digits

# generate plot
hi.plot <- d %>%
  filter( !is.na( health_insurance_effect ) & !is.na( fi_binary ) ) %>%
  ggplot() +
  geom_bar( aes( fi_binary, fill = factor( health_insurance_effect ) ),
            width = 0.35 ) +
  scale_fill_manual( values = c( "#F0E442", "#0072B2" ), # set manual colors
                     breaks = c( "Little/Somewhat/Not at all",
                                 "Quite a bit/Very much" ) ) +
  theme_classic() +
  theme( text = element_text( family = "Avenir" ),
         legend.position = c( 0.8, 0.85 ),
         legend.title = element_blank(),
         axis.title.x = element_blank(),
         legend.text = element_text( size = 12 ),
         axis.text.x = element_text( size = 12, face = "bold" ),
         axis.title.y = element_text( size = 12, face = "bold" ),
         plot.title = element_text( face = "italic" )  ) +
  ylab( "Frequency" ) + 
  annotate("text", x = 1.9, y = 9.5, 
           label = paste0( "italic(p) ==", p.hi ), # add annotated p-value with only p italicized
           parse = T,
           family = "Avenir",
           size = 5 ) +
  ggtitle( "Worse Insurance Status Due to Cancer Diagnosis" )


### Bar Plot for Worse Employment Effect ###

# get p-value for plot
t.emp <- table( d$employment_status_effect, d$fi_binary )
p.emp <- round( fisher.test( t.emp, simulate.p.value = T )$p.value, 2 )
p.emp <- ifelse( p.emp == 1, "0.99", p.emp ) # to avoid 1's
p.emp <- str_replace( p.emp, "(\\.\\d)$", "\\10" ) # for significant digits



# generate plot
emp.plot <- d %>%
  filter( !is.na( employment_status_effect ) & !is.na( fi_binary ) ) %>%
  ggplot() +
  geom_bar( aes( fi_binary, fill = factor( employment_status_effect ) ),
            width = 0.35 ) +
  scale_fill_manual( values = c( "#F0E442", "#0072B2" ), # set manual colors
                     breaks = c( "Little/Somewhat/Not at all",
                                 "Quite a bit/Very much" ) ) +
  theme_classic() +
  theme( text = element_text( family = "Avenir" ),
         legend.position = c( 0.8, 0.8 ),
         legend.title = element_blank(),
         axis.title.x = element_blank(),
         axis.text.x = element_text( size = 12, face = "bold"  ),
         axis.title.y = element_text( size = 12, face = "bold" ),
         plot.title = element_text( face = "italic" ) ) +
  ylab( "Frequency" ) + 
  annotate("text", x = 1.9, y = 20, 
           label = paste0( "italic(p) ==", p.emp ), # add annotated p-value with only p italicized
           parse = T,
           family = "Avenir",
           size = 5 ) + 
  ggtitle( "Worse Empoyment Status Due to Cancer Diagnosis" )


### Arrange into Single Plot ###

ggarrange( hi.plot, 
           emp.plot + theme( legend.position = "none",
                             legend.text = element_text( size = 19 )), 
           fin.plot + theme( legend.position = "none"), 
           labels = list( "A", "B", "C"),
           nrow = 3, ncol = 1 )

ggsave( "../04-tables-figures/effect-bar-plots.png",
        width = 7.36,
        height = 10.42 )




### Correlation Matrix of Dim 2 with other Variables ###

# create a matrix of variables to include in the correlation analysis
d.cor <- d %>%
  select( mca.dim.2, chaos_score, age,
          yrs_since_dx, financial_skills_index ) %>%
  as.matrix()

# generate the table (p-values also printed)
rcor.obj <- rcorr(as.matrix(d.cor))

# retain the correlations and p-values matrices
cor.mat <- round( rcor.obj$r, 2 )
p.mat <- rcor.obj$P

# assign asteriks depending on the p-values matrix

for( i in 1:nrow( cor.mat ) ){
  
  for( j in 1:ncol( cor.mat ) ){
    
    cor.mat[i,j] <- ifelse( p.mat[i,j] < 0.05, paste0( cor.mat[i,j], "*" ),
                            cor.mat[i,j] )
    
  }
}
  
write.table( cor.mat, "../04-tables-figures/financial-tox-correlation-matrix.txt", sep = "," )

