library(patchwork)
library(cowplot)
library(ggridges)

lemur_pal <- c("#ccb79d", "#6e5337", "#687277", "#949992", "#c69d60")

life_exp$common_name %>% class()

mean_age <- mean(life_exp$age_at_death_y) %>% round(2)

 

a <- life_exp %>% 
  ggplot(aes(x = age_at_death_y))+
  geom_histogram(binwidth = 1, 
                 colour = "black", 
                 fill = "#ccb79d")+
  geom_segment(aes(x = mean_age,
                   xend = mean_age,
                   y = 0, 
                   yend = 75),
                   linetype = "dashed")+
  theme_minimal_hgrid()+
  labs(title = "Life expectancy has a multimodal distribution",
       subtitle = "More lemurs die in first year of life than at any other age")+
  scale_x_continuous(name = "Age at death (yrs)")+
  labs(y = NULL)+
  annotate("text", x = mean_age + 0.5, y = 50, hjust = 0,
           label =  paste0("Mean life \nexpectancy\n (", mean_age, " yrs)"))+
  annotate("label", x = 2, y = 70, hjust = 0,
           label = "Many Lemurs die in\n1st year of life")
a

## add an arrow or curve to connect label to 1st bin

b <- life_exp %>%
  group_by(common_name) %>% 
  summarise(mean_life_expectancy = mean(age_at_death_y, na.rm = TRUE)) %>% 
  mutate(common_name = fct_reorder(common_name, mean_life_expectancy)) %>% 
  ggplot()+
  geom_col(aes(x = common_name, y = mean_life_expectancy, fill = common_name), 
           colour = "black")+
  coord_flip()+
  scale_fill_manual(values = lemur_pal)+
  theme_minimal_vgrid()+
  theme(legend.position = "none")+
  labs(x = NULL)

c <- life_exp %>%
  mutate(common_name = fct_reorder(common_name, age_at_death_y)) %>% 
  ggplot()+
  geom_boxplot(aes(x = common_name, y = age_at_death_y,
                   fill = common_name))+
  scale_fill_manual(values = lemur_pal)+
  coord_flip()+
  theme_minimal_grid()                    

d <-life_exp %>% 
  ggplot(aes(x = age_at_death_y), fill = lemur_pal)+
  geom_density(aes(fill = common_name)) +
  facet_wrap(~ common_name, ncol = 1)+
  scale_fill_manual(values = lemur_pal)+
  theme_minimal_hgrid()+
  theme(legend.position = "none")+
  labs(y = NULL)+
  scale_y_continuous(breaks = NULL,
                     labels = NULL)

d
  theme(strip.text = element_blank())


  e <- life_exp %>% 
    mutate(common_name = fct_reorder(common_name, age_at_death_y)) %>% 
    ggplot()+
    geom_density_ridges(aes(x = age_at_death_y, 
                            y = common_name,
                            height = ..density..,
                            fill = common_name,
                            scale = 0.95),
                        stat = "density") +
  scale_fill_manual(values = lemur_pal)+
    theme_minimal_vgrid()+
    theme(legend.position = "none")+
    labs(y = NULL)+
    scale_y_discrete(breaks = NULL,
                       labels = NULL) 

e  
  
a
b
c
d
e

a / (b + e)

a / b | e

a | b + e
