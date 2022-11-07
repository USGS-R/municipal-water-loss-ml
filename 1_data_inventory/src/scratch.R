x <- ga_all_yrs %>%
  filter(!is.na(Population)) %>%
  mutate(ntiles = ntile(Population,5)) %>%
  group_by(ntiles, Year) %>%
  summarise(Supplied = mean(Supplied, na.rm = T), Apparent.Losses = mean(Apparent.Losses, na.rm = T), Real.Losses = mean(Real.Losses, na.rm = T))%>%
  pivot_longer(cols = Supplied:Real.Losses)
ggplot(x, aes(x = Year, y = value, fill = name)) +
  geom_bar(position = "stack", stat = 'identity')+
  facet_wrap(~ntiles, scales = 'free_y')


library(readr)
library(ggridges)
library(hrbrthemes)
data <- ga_all_yrs %>%
  filter(!is.na(Population)) %>%
  mutate(ntiles = ntile(Population,5)) %>%
  group_by(ntiles) %>%
  mutate(Supplied_per_Capita = Supplied/Population)

data$ntiles_f <- ordered(data$ntiles, levels = c(1,2,3,4,5), labels = c('< 4,853', '4,855 - 7,062', '7,073 - 13,050','13,260 - 28,025','> 28,1832'))
  
ggplot(data, aes(x = Supplied_per_Capita, y = ntiles_f, fill = ntiles_f))+
  geom_density_ridges(rel_min_height = 0.005, scale = 0.75) +
  theme_ipsum()+
  theme(legend.position = 'none')+
  xlim(0,0.25)+
  xlab('Gallons supplied per capita (Million Gallons/Yr/Person)')+
  ylab('Population Served')




t <- data %>%
  filter(ntiles == 5) %>%
  group_by(Year)

y_axis <- list(title = 'Water Supplied (Million Gallons)')
plot_ly(t, y = ~Supplied, x = ~Year, type = 'box', text = ~Util, color = I('#FDE725FF')) %>%
  layout(yaxis = y_axis, title = 'Water Supplied Municipalities > 28,183')


t <- data %>%
  filter(ntiles == 5) %>%
  group_by(Year) %>%
  pivot_longer(cols = c('Supplied','Water.Losses')) %>%
  select(Year, Util, ntiles, name, value)

plot_ly(t, x = ~Year, y = ~value, color = ~name, type = 'box', text = ~Util,colors = c('#0C4B8E','#BF382A'))%>% 
  layout(boxmode = "group", 
         xaxis = list(title='Year'), 
         yaxis = list(title='Water (Million Gallons)'),
         title = 'Water Supplied Municipalities > 28,183')


data %>%
  filter(ntiles == 2) %>%
  plot_ly(x = ~Service.COnnections, y = ~Real.Losses, text = ~paste(Util,Year,sep = ' | '), type = 'scatter')

g <- data %>%
  ggplot(aes(x = Service.COnnections, y = Real.Losses, fill= ntiles_f, text = paste(Util,Year,sep = ' | ')))+
  geom_point(alpha = 0.6, stroke= 0.25)+
  facet_wrap(~ntiles_f, scales = c('free'), nrow = 1)+
  theme_ipsum()+
  theme(legend.position = 'none')+
  #xlim(0,0.25)+
  xlab('Service Connections')+
  ylab('Real Losses (Million Gallons)')
  
ggplotly(g, tooltip = 'text')






ga_all_yrs <- read_csv('C:/Users/ggorski/OneDrive - DOI/USGS_ML/WaterUse/github/municipal-water-loss-ml/1_data_inventory/out/georgia_all_years.csv',show_col_types = FALSE)

ca_all_yrs <- read_csv('C:/Users/ggorski/OneDrive - DOI/USGS_ML/WaterUse/github/municipal-water-loss-ml/Infrastructure_data/Infrastructure_data/California/CA_water_audit_data.csv')

ca_pwsid <- data.frame(state = 'CA', pwsid = ca_all_yrs$pwsid %>% unique())
ga_pwsid <- data.frame(state = 'GA', pwsid = ga_all_yrs$WSID %>% unique())


ca_ga_pwsid <- rbind(ca_pwsid, ga_pwsid)

#write_csv(ca_ga_pwsid, '1_data_inventory/out/ca_ga_pwsid.csv')

library(plotly)
library(hrbrthemes)
library(viridis)
library(gghalves)
library(ggdist)
library(readr)
library(ggridges)

p <- ga_all_yrs %>%
  filter(!is.na(Population)) %>%
  mutate(ntiles = ntile(Population,5)) %>%
  mutate(ntiles_f = ordered(ntiles, levels = c(1,2,3,4,5), 
                          labels = c('Population < 4,853', 'Population 4,855 - 7,062', 'Population 7,073 - 13,050',
                                     'Population 13,260 - 28,025','Population > 28,1832'))) %>%
  select(Year, Util, Population, Supplied, Non.Revenue.Water, Real.Losses, Apparent.Losses, ntiles_f) %>%
  mutate(Unbilled.Authorized.Consumption = Non.Revenue.Water-(Real.Losses+Apparent.Losses)) %>%
  mutate(Real.Losses.pct = (Real.Losses/Supplied), Apparent.Losses.pct = (Apparent.Losses/Supplied), 
         Unbilled.Authorized.Consumption.pct = (Unbilled.Authorized.Consumption/Supplied))%>%
  group_by(ntiles_f, Year) %>%
  summarise(Real.Losses = median(Real.Losses.pct), Apparent.Losses = median(Apparent.Losses.pct), 
            Unbilled.Authorized.Consumption = median(Unbilled.Authorized.Consumption.pct))%>%
  #filter(Util == 'Adel') %>%
  pivot_longer(cols = Real.Losses:Unbilled.Authorized.Consumption) %>%
  ggplot(aes(x = Year, y = value, fill = name, text = name))+
  geom_area()+
  scale_fill_viridis(discrete = TRUE, name = 'Non-Revenue Category')+
  #geom_line(data = supp_data, aes(x = Year, y = Supplied))+
  theme_bw()+
  ggtitle('Non-Revenue Water by Municipality Population')+
  ylab('Water Losses as a fraction of water supplied')+
  facet_wrap(.~ntiles_f, ncol = 1, scales = "fixed")
p

#ggplotly(p)
my_pal <- viridis(5)

#Real water lost
p <- ga_all_yrs %>%
  filter(!is.na(Population)&!is.na(Real.Losses)) %>%
  mutate(ntiles = ntile(Population,5)) %>%
  mutate(ntiles_f = ordered(ntiles, levels = c(1,2,3,4,5), 
                            labels = c('Population < 4,853', 'Population 4,855 - 7,062', 'Population 7,073 - 13,050',
                                       'Population 13,260 - 28,025','Population > 28,1832'))) %>%
  ggplot(aes(y = ntiles_f, x = Real.Losses, fill = ntiles_f, color = ntiles_f))+
  stat_density_ridges(alpha = 0.8, quantiles = 2, quantile_lines = TRUE)+
  theme_ipsum()+
  xlim(0,1250)+
  xlab('Real water losses \n (Millions of Gallons)')+
  ylab('Population served')+
  labs(title = 'Water lost 2012-2019')+
  theme(legend.position = 'none', plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14))+
  scale_color_manual(values = rep('black',5), guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none")

p

#Apparent water lost
p <- ga_all_yrs %>%
  filter(!is.na(Population)&!is.na(Apparent.Losses)) %>%
  mutate(ntiles = ntile(Population,5)) %>%
  mutate(ntiles_f = ordered(ntiles, levels = c(1,2,3,4,5), 
                            labels = c('Population < 4,853', 'Population 4,855 - 7,062', 'Population 7,073 - 13,050',
                                       'Population 13,260 - 28,025','Population > 28,1832'))) %>%
  ggplot(aes(y = ntiles_f, x = Apparent.Losses, fill = ntiles_f, color = ntiles_f))+
  stat_density_ridges(alpha = 0.8, quantiles = 2, quantile_lines = TRUE)+
  theme_ipsum()+
  xlim(0,250)+
  xlab('Apparent water losses \n (Millions of Gallons)')+
  ylab('Population served')+
  labs(title = 'Water lost 2012-2019')+
  theme(legend.position = 'none', plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14))+
  scale_color_manual(values = rep('black',5), guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none")

p


#cost of water losses
p <- ga_all_yrs %>%
  filter(Real.Losses.Cost >=0) %>%
  filter(!is.na(Population)&!is.na(Real.Losses.Cost)) %>%
  mutate(ntiles = ntile(Population,5)) %>%
  mutate(ntiles_f = ordered(ntiles, levels = c(1,2,3,4,5), 
                            labels = c('Population < 4,853', 'Population 4,855 - 7,062', 'Population 7,073 - 13,050',
                                       'Population 13,260 - 28,025','Population > 28,1832'))) %>%
  mutate(Real.Losses.Cost.Thou = Real.Losses.Cost/1000) %>%
  ggplot(aes(y = ntiles_f, x = Real.Losses.Cost.Thou, fill = ntiles_f, color = ntiles_f))+
  stat_density_ridges(alpha = 0.8, quantiles = 2, quantile_lines = TRUE)+
  theme_ipsum()+
  xlim(0,500)+
  xlab('Cost of real water losses \n (Thousand $)')+
  ylab('Population served')+
  labs(title = 'Annual cost of real water losses 2012-2019')+
  theme(legend.position = 'none', plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14))+
  scale_color_manual(values = rep('black',5), guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none")
p

p <- ga_all_yrs %>%
  filter(Real.Losses.Cost >=0) %>%
  filter(!is.na(Population)&!is.na(Real.Losses.Cost)&!is.na(Operating.Cost)) %>%
  mutate(ntiles = ntile(Population,5)) %>%
  mutate(ntiles_f = ordered(ntiles, levels = c(1,2,3,4,5), 
                            labels = c('Population < 4,853', 'Population 4,855 - 7,062', 'Population 7,073 - 13,050',
                                       'Population 13,260 - 28,025','Population > 28,1832'))) %>%
  mutate(Real.Losses.Cost.Frac = (Real.Losses.Cost/Operating.Cost)*100) %>%
  ggplot(aes(y = ntiles_f, x = Real.Losses.Cost.Frac, fill = ntiles_f, color = ntiles_f))+
  stat_density_ridges(alpha = 0.8, quantiles = 2, quantile_lines = TRUE)+
  theme_ipsum()+
  xlim(0,30)+
  xlab('Cost of real water losses \n (% of operating cost)')+
  ylab('Population served')+
  labs(title = 'Annual cost of real water losses 2012-2019')+
  theme(legend.position = 'none', plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14))+
  scale_color_manual(values = rep('black',5), guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none")
p


ga_all_yrs %>%
  filter(!is.na(Population)&!is.na(Apparent.Losses.Cost)&!is.na(Real.Losses.Cost)) %>%
  mutate(ntiles = ntile(Population,5)) %>%
  group_by(ntiles) %>%
  summarise(med_real_losses_cost = median(Real.Losses.Cost), med_apparent_losses_cost = median(Apparent.Losses.Cost))

ga_all_yrs %>%
  filter(!is.na(Population)&!is.na(Apparent.Losses)&!is.na(Real.Losses)) %>%
  mutate(ntiles = ntile(Population,5)) %>%
  group_by(ntiles) %>%
  summarise(med_real_losses = median(Real.Losses), med_apparent_losses = median(Apparent.Losses))
