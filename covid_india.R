load("rda/final.plot.rda")


combined.final <- combined.final %>% mutate(density_norm = (max(Density) - Density)/max(Density))
combined.rural <- combined.rural %>% mutate(rural_norm = (max(rural_prop , na.rm = TRUE) - rural_prop)/max(rural_prop , na.rm = TRUE))

fig_cases_per_mil <- ggplot() +
  geom_polygon(data = final.plot,
               aes(x = long, y = lat, group = group, fill = confirmed_per_million), 
               color = "black", size = 0.25) + 
  coord_map()+
  scale_fill_gradient(name="COV-19", limits = c(1200,18000) ,low = 'white', high = 'red')+
  labs(title="State-Wise Covid-19 Confirmed Cases per Million")+
  xlab('Longitude')+
  ylab('Latitude') +
  theme_cowplot(line_size = 0.4,rel_large = 15/14 , rel_small = 11/14 , rel_tiny = 10/14)
load("rda/final.plot2.rda")

fig_tests_per_mil <- ggplot() +
  geom_polygon(data = final.plot2,
               aes(x = long, y = lat, group = group, fill = samples_per_million), 
               color = "black", size = 0.25) + 
  coord_map()+
  scale_fill_gradient(name="COV-19_Test" ,low = 'red', high = 'white')+
  labs(title="State-Wise Covid-19 Tests per Million")+
  xlab('Longitude')+
  ylab('Latitude') +
  theme_cowplot(line_size = 0.4,rel_large = 15/14 , rel_small = 11/14 , rel_tiny = 10/14)

grid_compare <-plot_grid(fig_cases_per_mil , fig_tests_per_mil)

ggsave("figs/cases_per_mil.png" ,plot = fig_cases_per_mil)
ggsave("figs/tests_per_mil.png" ,plot = fig_tests_per_mil)
ggsave("figs/compare.png" ,plot = grid_compare)

load("rda/combined.final.rda")

combined.plot <- ggplot(data = combined.final,
         aes(samples_per_million, confirmed_per_million ) ) +
  geom_point(aes( size = Density) ,fill="darkred", color="darkred" , alpha = 0.6) +
  scale_size_area(max_size = 25) +
  labs(title="State-Wise Covid-19 Confirmed Cases per Million") + 
  geom_text(aes(label = id) ,check_overlap = TRUE , size =2 , vjust = 2.5) + 
  xlab('Samples per Million')+
  ylab('Confirmed Cases per Million')   +
  labs(size="Population Density") +
  theme_cowplot(line_size = 0.4,rel_large = 15/14 , rel_small = 11/14 , rel_tiny = 10/14) 


ggsave("figs/combined.png" , plot = combined.plot)

combined_rural.plot <- ggplot(data = combined.rural,
                        aes(samples_per_million , confirmed_per_million) ) +
  geom_point(aes( size = rural_prop) ,fill="darkred", color="darkred" , alpha = 0.6) +
  scale_size_area(max_size = 25) +
  labs(title="State-Wise Covid-19 Confirmed Cases per Million") + 
  geom_text(aes(label = id) ,check_overlap = TRUE , size =2 , vjust = 2.5) + 
  xlab('Samples per Million')+
  ylab('Confirmed Cases per Million') + 
  labs(size="Rural Proportion") +
  theme_cowplot(line_size = 0.4,rel_large = 15/14 , rel_small = 11/14 , rel_tiny = 10/14)

ggsave("figs/combined_rural.plot.png" , plot = combined_rural.plot)

plot_grid(combined.plot , combined_rural.plot)

#plot4
load("rda/testing.rda")
load("rda/testing.final.rda")
testing.plot <- ggplot(data = testing.final , aes(x=state ,y = tests_per_million , fill = Month)) +
  geom_bar(stat="identity", position=position_dodge()) + theme_cowplot(rel_large = 15/14 , rel_small = 10/14 , rel_tiny = 9/14) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) + 
  xlab("States") +
  ylab("Tests per Million") +
  labs(title = "Tests Conducted")
ggsave("figs/testing.plot.png")  


