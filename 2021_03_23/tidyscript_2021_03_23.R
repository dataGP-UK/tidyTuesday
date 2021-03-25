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

## relational databases: which I haven't done yet!
## need to do this module in R4DS before I can move on