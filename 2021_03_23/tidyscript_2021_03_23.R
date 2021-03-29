# 25/3/2021 Simon Hulme dataGP-UK

# tidy tuesday 23rd March 2021

rm(list=ls())

# Install packages and data
install.packages("tidytuesdayR")
library(tidytuesdayR)
library(tidyverse)

# Load datasets for the week of interest

tuesdata <- tidytuesdayR::tt_load('2021-03-23')

unvotes <- tuesdata$unvotes
roll_calls <- tuesdata$roll_calls
issues <- tuesdata$issues

# initial observations

## relational databases - explore databases to identify keys

roll_calls %>% count(rcid) %>% 
  filter(n > 1) %>%   # rcid = primary key for roll_calls
  nrow()

## reviewed 3 datasets - roll_call rcid is primary key
## no primary key in other data but roll_call$rcid as foreign key
## that links each data base with roll_call

# look at each dataframe to understand variables
## descriptions provided with datasets

# define a question: 
# how did the UK vote on UN resolutions in 2019 compared to the US
# does this indicate any policy differences
# explore differences in voting between Trump & Obama presidencies

# filter to trump presidential term

roll1 <- roll_calls %>%
  filter(date >= "2017-01-20" & date <= "2019-12-31") # switched to trump term

#simplify dates to year only - separate column then select out

roll2 <- roll1 %>% 
  separate(date, 
           into = c('year', 'month_day'),
           sep='-', extra = 'merge') %>% 
  select(c(rcid, year, short))

## table simplified to show rcid, year & details of resolutions only

# next join issues and unvotes with roll2
## also filtered for countries of interest


un_voting <- roll2 %>% 
  left_join(issues, by = 'rcid') %>% 
  left_join(unvotes, by = 'rcid') %>% 
  select(-(country_code)) %>% 
  filter(issue != (is.na = TRUE))

# ready for some exploratory analysis and visualisation

issue_voting <- un_voting %>% 
  group_by(issue, country) %>% 
  count(vote)

# simpler single table - keep if can use for visualisation and delete other
# need to produce wider table

issue_voting1 <- issue_voting %>%
  pivot_wider(names_from = vote, values_from = n)

issue_voting1 %>%
  filter(country %in% c('United States', 'Israel', 
                        'United Kingdom', 'Saudi Arabia')) %>% 
  ggplot(aes(country, yes))+
  geom_col(aes(fill = issue))+
  coord_flip()
  facet_wrap(~issue)+
  theme(legend.position = 'NULL')

  
write_csv(issue_voting1, 'issue_voting.csv')
  
