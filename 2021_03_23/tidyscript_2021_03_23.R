# 25/3/2021 Simon Hulme dataGP-UK

# tidy tuesday 23rd March 2021



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

# filter roll_call data to 2019 only

roll1 <- roll_calls %>% 
  filter(date > '2019-01-10')

# initial attempt didn't work as expected

roll1 <- roll_calls %>%
  filter(date >= "2019-01-01" & date <= "2019-12-31") # or

#reviewed - there are only 90 observations - the UN only convenes in dec!

#simplify dates to year only - separate column then select out

roll2 <- roll1 %>% 
  separate(date, 
           into = c('year', 'month_day'),
           sep='-', extra = 'merge') %>% 
  select(c(rcid, year, short))

## table simplified to show rcid, year & details of resolutions only

# next join issues and unvotes with roll2

un_voting <- roll2 %>% 
  left_join(issues, by = 'rcid') %>% 
  left_join(unvotes, by = 'rcid')

# lots of NAs - issues table doesnt cover all resolutions
# also Namibia has no Country code as r has interpreted na
## as missing value- need to work out how to correct this
## may be need dataframe looking at key issues 
## new questions: israel vs usa - palestine and all
## usa vs. superpowers (China, Russia, India)


