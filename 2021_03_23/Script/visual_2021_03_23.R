# visualisation script for tidytuesday 2021_3_23 'UN Voting Data'

# load tidied data

issue_voting <- read_csv('issue_voting.csv')

# visualisation

issue_voting1 %>%
  filter(country %in% c('United States', 'Israel', 
                        'United Kingdom', 'Saudi Arabia')) %>% 
  ggplot(aes(country, yes))+
  geom_col(aes(fill = issue))+
  coord_flip()
facet_wrap(~issue)+
  theme(legend.position = 'NULL')
