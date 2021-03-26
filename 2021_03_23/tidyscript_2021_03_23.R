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