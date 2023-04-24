######################################################
# Bar plots Similar to Jagsi Paper and Correlations  #
# Between Dim 2 and Other Continuous Variables       #
######################################################

library( tidyverse )
library( ggpubr ) # for arranging subplots
library( Hmisc ) # for correlation matrix with p-values

d <- readRDS( "../03-data-rodeo/01-data-mca-recat.rds" )


### Worse Status Questions ###

### Bar Plot for Worse Financial Effect ###

# get p-value for plot
t.fin.s <- table( d$worse_financial_status, d$fi_binary )
p.fin.s <- round( chisq.test( t.fin.s, simulate.p.value = T )$p.value, 2 )
p.fin.s <- ifelse( p.fin.s == 1, "0.99", p.fin.s ) # to avoid 1's
p.fin.s <- ifelse( p.fin.s == 0, "'< 0.01'", p.fin.s ) # to avoid 1's
p.fin.s <- str_replace( p.fin.s, "(\\.\\d)$", "\\10" ) # for significant digits

# generate plot
fin.s.plot <- d %>%
  filter( !is.na( worse_financial_status ) & !is.na( fi_binary ) ) %>%
  ggplot() +
  geom_bar( aes( fi_binary, fill = factor( worse_financial_status ) ),
            width = 0.35 ) +
  scale_fill_manual( values = c( "#F0E442", "#0072B2" ), # set manual colors
                     breaks = c( "No",
                                 "Yes" ) ) +
  theme_classic() +
  theme( text = element_text( family = "Avenir" ),
         legend.position = c( 0.8, 0.8 ),
         legend.title = element_blank(),
         axis.title.x = element_blank(),
         axis.text.x = element_text( size = 12, face = "bold" ),
         axis.title.y = element_text( size = 12, face = "bold" ),
         plot.title = element_text( face = "italic" ) ) +
  ylab( "Frequency" ) + 
  annotate("text", x = 2.2, y = 100, 
           label = paste0( "italic(p) ==", p.fin.s ), # add annotated p-value with only p italicized
           parse = T,
           family = "Avenir",
           size = 5 ) +
  ggtitle( "Worse Financial Status Since Cancer Diagnosis" )



### Bar Plot for Worse Health Insurance Effect ###

# get p-value for plot
t.hi.s <- table( d$worse_insurance, d$fi_binary )
p.hi.s <- round( chisq.test( t.hi.s, simulate.p.value = T )$p.value, 2 )
p.hi.s <- ifelse( p.hi.s == 1, "0.99", p.hi.s ) # to avoid 1's
p.hi.s <- ifelse( p.hi.s == 0, "'< 0.01'", p.hi.s ) # to avoid 1's
p.hi.s <- str_replace( p.hi.s, "(\\.\\d)$", "\\10" ) # for significant digits

# generate plot
hi.s.plot <- d %>%
  filter( !is.na( worse_insurance ) & !is.na( fi_binary ) ) %>%
  ggplot() +
  geom_bar( aes( fi_binary, fill = factor( worse_insurance ) ),
            width = 0.35 ) +
  scale_fill_manual( values = c( "#F0E442", "#0072B2" ), # set manual colors
                     breaks = c( "No",
                                 "Yes" ) ) +
  theme_classic() +
  theme( text = element_text( family = "Avenir" ),
         legend.position = c( 0.8, 0.8 ),
         legend.title = element_blank(),
         axis.title.x = element_blank(),
         axis.text.x = element_text( size = 12, face = "bold" ),
         axis.title.y = element_text( size = 12, face = "bold" ),
         plot.title = element_text( face = "italic" ) ) +
  ylab( "Frequency" ) + 
  annotate("text", x = 2.2, y = 100, 
           label = paste0( "italic(p) ==", p.hi.s ), # add annotated p-value with only p italicized
           parse = T,
           family = "Avenir",
           size = 5 ) +
  ggtitle( "Worse Insurance Status Since Cancer Diagnosis" )


### Bar Plot for Worse Employment Effect ###

# get p-value for plot
t.emp.s <- table( d$worse_employment_status, d$fi_binary )
p.emp.s <- round( chisq.test( t.emp.s, simulate.p.value = T )$p.value, 2 )
p.emp.s <- ifelse( p.emp.s == 1, "0.99", p.emp.s ) # to avoid 1's
p.emp.s <- ifelse( p.emp.s == 0, "'< 0.01'", p.emp.s ) # to avoid 1's
p.emp.s <- str_replace( p.emp.s, "(\\.\\d)$", "\\10" ) # for significant digits

# generate plot
emp.s.plot <- d %>%
  filter( !is.na( worse_employment_status ) & !is.na( fi_binary ) ) %>%
  ggplot() +
  geom_bar( aes( fi_binary, fill = factor( worse_employment_status ) ),
            width = 0.35 ) +
  scale_fill_manual( values = c( "#F0E442", "#0072B2" ), # set manual colors
                     breaks = c( "No",
                                 "Yes" ) ) +
  theme_classic() +
  theme( text = element_text( family = "Avenir" ),
         legend.position = c( 0.8, 0.8 ),
         legend.title = element_blank(),
         axis.title.x = element_blank(),
         axis.text.x = element_text( size = 12, face = "bold" ),
         axis.title.y = element_text( size = 12, face = "bold" ),
         plot.title = element_text( face = "italic" ) ) +
  ylab( "Frequency" ) + 
  annotate("text", x = 2.2, y = 100, 
           label = paste0( "italic(p) ==", p.emp.s ), # add annotated p-value with only p italicized
           parse = T,
           family = "Avenir",
           size = 5 ) +
  ggtitle( "Worse Employment Status Since Cancer Diagnosis" )

### Arrange into Single Plot ###

worse.plot <- ggarrange( hi.s.plot, 
           emp.s.plot + theme( legend.position = "none",
                             legend.text = element_text( size = 19 )), 
           fin.s.plot + theme( legend.position = "none"), 
           labels = list( "A", "", ""),
           nrow = 3, ncol = 1 )

ggsave( "../04-tables-figures/worse-bar-plots.png",
        width = 7.36,
        height = 10.42 )







### Effect Questions ###
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

effect.plot <- ggarrange( hi.plot, 
           emp.plot + theme( legend.position = "none",
                             legend.text = element_text( size = 19 )), 
           fin.plot + theme( legend.position = "none"), 
           labels = list( "B", "", ""),
           nrow = 3, ncol = 1 )

ggsave( "../04-tables-figures/effect-bar-plots.png",
        width = 7.36,
        height = 10.42 )


### Now Arrange Effect and Worse Plots into Single Plot ###
all.plot <- ggarrange( worse.plot, effect.plot, nrow = 1, ncol = 2)


ggsave( "../04-tables-figures/worse-effect-bar-plots.png",
        width = 9.72,
        height = 14.84 )




### Correlation Matrix of Dim 2 with other Variables ###

# create a matrix of variables to include in the correlation analysis
d.cor <- d %>%
  filter( number_household >= 2 ) %>% #filter by no. in household given potential issues with CHAOS score
  select( mca.dim.2, chaos_score, age,
          yrs_since_dx, financial_skills_index,factg_total,
          factg_pwb, factg_ewb, factg_fwb, factg_swb ) %>%
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

