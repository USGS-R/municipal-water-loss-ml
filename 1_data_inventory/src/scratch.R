x <- ga_all_yrs %>%
  filter(!is.na(Population)) %>%
  mutate(ntiles = ntile(Population,5)) %>%
  group_by(ntiles, Year) %>%
  summarise(Supplied = mean(Supplied, na.rm = T), Apparent.Losses = mean(Apparent.Losses, na.rm = T), Real.Losses = mean(Real.Losses, na.rm = T))%>%
  pivot_longer(cols = Supplied:Real.Losses)
ggplot(x, aes(x = Year, y = value, fill = name)) +
  geom_bar(position = "stack", stat = 'identity')+
  facet_wrap(~ntiles, scales = 'free_y')



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