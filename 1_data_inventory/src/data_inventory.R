library(readr)
library(tidyverse)
library(plyr)

#read in Georgia data
ga <- list()
ga[['2019']] <- readxl::read_xlsx('Infrastructure_data/Infrastructure_data/Georgia/GA_WaterAudits2019.xlsx', 
                                  na = c('Note 1','Note 2','Note 3','Note 4','Note A','Note B','N/A','Note 1 / A','Note 2 / B'), n_max = 242, 
                                  col_types = c(rep('text',3),rep('numeric',23),'text', rep('numeric',8), rep('text',3)))
ga[['2018']] <- readxl::read_xlsx('Infrastructure_data/Infrastructure_data/Georgia/GA_WaterAudits2018.xlsx', 
                                  na = c('Note 1','Note 2','Note 3','Note 4','Note A','Note B','N/A', 'Note 1 & A','Note 2 & B'), n_max = 244,
                                  col_types = c(rep('text',3),rep('numeric',23),'text', rep('numeric',8), rep('text',3)))
ga[['2017']] <- readxl::read_xlsx('Infrastructure_data/Infrastructure_data/Georgia/GA_WaterAudits2017.xlsx', 
                                  na = c('Note 1','Note 2','Note 3','Note 4','Note A','Note B', 'N/A', 'Note 1 & A','Note 2 & B'), n_max = 234,
                                  col_types = c(rep('text',3),rep('numeric',23),'text', rep('numeric',8), rep('text',3)))
ga[['2016']] <- readxl::read_xlsx('Infrastructure_data/Infrastructure_data/Georgia/GA_WaterAudits2016.xlsx', 
                                  na = c('Note 1','Note 2','Note 3','Note 4','Note A','Note B', 'N/A', 'Note 1 & A','Note 2 & B'), n_max = 231,
                                  col_types = c(rep('text',3),rep('numeric',23),'text', rep('numeric',8), rep('text',3)), range = 'A1:AL231')
ga[['2015']] <- readxl::read_xlsx('Infrastructure_data/Infrastructure_data/Georgia/GA_WaterAudits2015.xlsx', 
                                  na = c('','N/A','See limits in definition'), n_max = 228, 
                                  col_types = c(rep('text',3),rep('numeric',23),'text', rep('numeric',8), rep('text',3)), range = 'A1:AL228')
ga[['2014']] <- readxl::read_xlsx('Infrastructure_data/Infrastructure_data/Georgia/GA_WaterAudits2014.xlsx', 
                                  na = c('','N/A','See limits in definition'), n_max = 227, 
                                  col_types = c(rep('text',3),rep('numeric',23),'text', rep('numeric',8), rep('text',3)))
ga[['2013']] <- readxl::read_xlsx('Infrastructure_data/Infrastructure_data/Georgia/GA_WaterAudits2013.xlsx', 
                                  na = c('','N/A','See limits in definition'), n_max = 227, 
                                  col_types = c(rep('text',3),rep('numeric',22),'text', rep('numeric',8), rep('text',3))) %>%
  add_column(`Authorized Consumption         (Million gallons / Year)` = NA, .after = "Unbilled Unmetered (Million Gallons/Yr)")
ga[['2012']] <- readxl::read_xlsx('Infrastructure_data/Infrastructure_data/Georgia/GA_WaterAudits2012.xlsx', 
                                  na = c('','N/A','See limits in definition'), n_max = 226, 
                                  col_types = c(rep('text',3),rep('numeric',22),'text', rep('numeric',8), rep('text',3)))%>%
  add_column(`Authorized Consumption         (Million gallons / Year)` = NA, .after = "Unbilled Unmetered (Million Gallons/Yr)")


#population served
shortened_names <- c('Util','WSID','County','Population',
                     'Own Source','Imported','Exported','Supplied',
                     'Billed Metered','Billed Unmetered','Unbilled Metered','Unbilled Unmetered',
                     'Authorized Consumption','Water Losses','Apparent Losses','Real Losses','Non-Revenue Water','Length of Mains','Service COnnections','Pressure', 
                     'Operating Cost','Unit Cost','Production Cost',
                     'UARL','Apparent Losses Cost','Real Losses Cost','VPC/CRUC','Non-Rev Water as % Volume', 'Non-Rev Water as % Op Cost','Apparent Losses per connection per day','Real Losses per connection per day','Real Losses per length per day','CARL','ILI','Data Validity',
                     'PA-1','PA-2','PA-3')

do.call(rbind.data.frame, lapply(ga, function(x) quantile(x[,4], na.rm = TRUE, probs = seq(.2,1,by = .2))))

do.call(rbind.data.frame, lapply(ga, function(x) colnames(x)))

for(i in 1:8){
  colnames(ga[[i]]) <- shortened_names
}

ga_all_yrs <- ldply(ga, data.frame)

colnames(ga_all_yrs)[1] <- 'Year'
write_csv(ga_all_yrs, '1_data_inventory/out/georgia_all_years.csv')



quants <- quantile(ga_all_yrs$Population, na.rm = TRUE, probs = seq(.2,1,by = .2))

ga_all_yrs %>%
  mutate(ntiles = ntile(Population,5)) %>%
  group_by(ntiles) %>%
  summarise(min_pop = min(Population), max_pop = max(Population), number_of_unique_municipalities = n_distinct(WSID), mean_supplied = mean(Supplied), mean_lost = mean(Water.Losses)) %>%
  mutate(mean_lost_frac = mean_lost/mean_supplied) %>%
  select(!mean_lost)
