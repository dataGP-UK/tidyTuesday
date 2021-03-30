# visualisation script for tidytuesday 2021_3_23 'UN Voting Data'

# load tidied data

library(tidyverse)
issue_voting <- read_csv("2021_03_23/Data/issue_voting.csv")

# visualisation

issue_voting %>%
  filter(country %in% c('United States', 'Israel', 
                        'United Kingdom','Saudi Arabia')) %>%
  group_by(issue,country) %>% 
  mutate(YES =
           (yes / sum(yes, no, abstain, na.rm = TRUE) * 100)) %>% 
  ggplot(aes(country, YES))+
  geom_col(aes(fill = country))+
  coord_flip()+
  facet_wrap(~issue)+
  theme_bw()+
  theme(legend.position = 'NULL')+
  labs(y = "Yes (%)", x = NULL,
       title = "Voting Patterns by Issue (UN General Congress 21/1/2017 - 31/12/2019)")


  
 
