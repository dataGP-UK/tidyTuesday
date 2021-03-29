# visualisation script for tidytuesday 2021_3_23 'UN Voting Data'

# load tidied data

issue_voting <- read_csv('issue_voting.csv')

# visualisation

issue_voting %>%
  filter(country %in% c('United States', 'Israel', 
                        'United Kingdom')) %>%
  
  group_by(issue,country) %>% 
  mutate(YES =
           (yes / (yes + no + abstain) * 100), 
         na.rm = TRUE) %>% 
  ggplot(aes(country, YES))+
  geom_col(aes(fill = country))+
  coord_flip()+
  facet_wrap(~issue)+
  theme_bw()+
  theme(legend.position = 'NULL')
  
 
