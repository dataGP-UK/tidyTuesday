# setup -------------------------------------------------------------------
library(tidyverse)
library(skimr)
library(here)
tuesdata <- tidytuesdayR::tt_load(2021, week = 34)
computer <- tuesdata$computer


# tidying -----------------------------------------------------------------

# on inspection of the data there are a number of variables that include
# differing spelling/syntax of the same value. 
# these should also be converted into factors
# will need to use stringr for string manipulation

# task 1: detect any value in {name} containing "com" or "Com"
## create object containing  variations of "computer" and convert into factor
com_variation <- 
  computer[str_detect(computer$char, "(com|Com)+"),] %>%
  distinct(char) %>% 
  .$char

computer$char <-as_factor(computer$char)

# task 2: collapse all factor levels containing "com|Com" into single factor level of "Computer"
computer$char <-
  fct_collapse(computer$char, Computer = com_variation)

# task 3: Deal with duplication of other characters names
## remove extra information in parentheses (e.g V.O. or O.S)
## identify pattern of " ("  an remove it plus characters that follow it
## recode a few remaining duplicateswhere no pattern evident
computer$char <- 
  computer$char %>% 
  # regex to remove " (" and replace with nothing
  str_replace_all(pattern = "\\s+\\(+.+", replacement = "") %>% 
  fct_recode(Picard = "Jean-Luc",
             Troi = "Mrs. Troi",
             Riker = "Riker'S Voice")

## check levels of char factor
computer$char %>% levels

# task 4: "type" variable has duplication due to inconsitsent use of case
## duplicates due to different cases used
## switch all to title case and change to factor
computer$type <- 
  computer$type %>% 
  str_to_title() %>% 
  as_factor()

# task 5: "IoT" and "Iot" both exist - recode to "IoT"
## represents "Internet of Things" - I objects controlled via VUI
computer$domain <- 
  computer$domain %>% 
  as_factor() %>% 
  fct_recode(IoT = "Iot") 

# task 6: "Holodeck" appears twice in sub_domain category.
computer$sub_domain <- 
  computer$sub_domain %>% 
  as_factor() %>% 
  fct_recode(Holodeck = "Holodeck?")  


# Data Transformation -----------------------------------------------------


