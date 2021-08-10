# set up ------------------------------------------------------------------
library(tidyverse)
tuesdata <- tidytuesdayR::tt_load('2021-07-27')
olympics <- tuesdata$olympics
rm(tuesdata)


# ideas for exploration ---------------------------------------------------

# how many gold medals have GB won in the summer olympics?
# what sports have GB won medals and which type of medal
# what trends are evident in sports that GB medal in?

# create gb_summer dataset and subsets for medals and gold medals ---------

# filter dataset for GB and summer olympics
gb_summer <- olympics %>% 
  filter(season == "Summer" & noc =="GBR") %>% 
  select(sex, year, city, sport, event, medal)

rm(olympics)

# create sets for medals & golds

gb_gold <- gb_summer %>%  
  filter(medal == "Gold")

gb_medal <- gb_summer %>% 
  filter(medal %in% c("Gold", "Silver", "Bronze"))

## check for any event where >1 gold medal in same games
gb_gold[gb_gold %>% duplicated,] %>% 
  arrange(event, year, medal)

gb_medal[gb_medal %>% duplicated,] %>% 
  arrange(event, year, medal)

## change from medal per individual to medal per sport.
gb_gold <- gb_gold %>% 
  distinct()

gb_medal <- gb_medal %>% 
  distinct() 

# explore GB medal haul visually ------------------------------------------

## check single year (last olympics - 2016)
gb_gold %>% 
  filter(year == 2016) %>% 
  ggplot(aes(x = sport))+
  geom_bar()

### expand to all games
gb_gold %>% 
  group_by(sport) %>% 
  count() %>% 
  ggplot(aes(x = fct_reorder(sport, n), y = n))+
  geom_col(fill = "gold", colour = "black")+
  coord_flip()

# break down into medal types and visualise

gb_medal_split <- gb_medal %>% 
  group_by(sport, medal) %>% 
  tally %>% 
  mutate(total_medals = sum(n))

gb_medal_plot <- gb_medal_split %>% 
  ggplot(aes(x = fct_reorder(sport, total_medals),y = n))+
  geom_col(aes(fill = fct_relevel(medal, c("Bronze", "Silver", "Gold"))), 
           colour = "black")+
  coord_flip()+
  scale_fill_manual(values=c("#cd7f32", "#C0C0C0", "#FFDF00")) +
  labs(x = "Sport", y = "Number of Medals Won",
       title = "Medals won by GB Summer Olympics team (1896-2016)",
       subtitle = "Cycling, Swimmming, Rowing and Sailing are most successful sports behind Athletics",
       caption = "Data from https://github.com/rfordatascience/tidytuesday.") +
  theme(legend.title = element_blank(), 
        legend.background = element_rect(color = "grey", linetype = "solid"),
        legend.position = c(.95, .5),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.key.size = unit(0.5, 'cm'))+
  guides(fill =  guide_legend(nrow = 1,reverse = TRUE))

gb_medal_plot

ggsave("2021_07_27_Olympic_Medals/gb_medal_plot.png", width = 10, height = 6)



# lump categories together to make plot simpler --------------------

gb_medal_plot2 <- gb_medal %>%
  mutate(sport = fct_lump(sport, 9)) %>% 
  group_by(sport, medal) %>% 
  tally() %>% 
  mutate(total_medals = sum(n)) %>%
  ungroup() %>% 
  mutate(sport = fct_reorder(sport, total_medals, min),
         sport = fct_relevel(sport, "Other", after = 0)) %>% 
  ggplot()+
  geom_col(aes(sport, n, 
           fill = fct_relevel(medal, c("Bronze", "Silver", "Gold"))), 
           colour = "black")+
  coord_flip()+
  scale_fill_manual(values=c("#cd7f32", "#C0C0C0", "#FFDF00")) +
  labs(x = "Sport", y = "Number of Medals Won",
       title = "Medals won by GB Summer Olympics team (1896-2016)",
       subtitle = "Cycling, Swimmming, Rowing and Sailing are most successful sports behind Athletics",
       caption = "Data from https://github.com/rfordatascience/tidytuesday.") +
  theme(legend.title = element_blank(), 
        legend.background = element_rect(color = "grey", linetype = "solid"),
        legend.position = c(.95, .5),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.key.size = unit(0.5, 'cm'))+
  guides(fill =  guide_legend(nrow = 1,reverse = TRUE))

gb_medal_plot2

ggsave("2021_07_27_Olympic_Medals/gb_medal_plot2.png", width = 10, height = 6)

 
