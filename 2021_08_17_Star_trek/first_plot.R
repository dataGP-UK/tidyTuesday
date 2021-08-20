# setup -------------------------------------------------------------------
library(tidyverse)
library(skimr)
library(here)

load("2021_08_17_Star_trek/person.RDS")

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

# task 2: collapse levels containing "com|Com" into single level of "Computer"
computer$char <-
  fct_collapse(computer$char, Computer = com_variation)

# task 3: Deal with duplication of other characters names
computer$char <- 
  computer$char %>% 
  # regex to remove " ("  (and what follows this). Replace with nothing
  str_replace_all(pattern = "\\s+\\(+.+", replacement = "") %>% 
  # no pattern evident  so recode levels manually
  fct_recode(Picard = "Jean-Luc",
             Troi = "Mrs. Troi",
             Riker = "Riker'S Voice")

## check levels of char factor
computer$char %>% levels

# task 4: "type" variable has duplication due to inconsistent use of case
computer$type <- 
  computer$type %>% 
  # switch all to title case and change to factor
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
  # replace entries that have a "?"
  fct_recode(Holodeck = "Holodeck?")  

# task 7: # convert final variables to  factors {char_type}
computer$char_type <- 
  as_factor(computer$char_type)

computer$pri_type <- 
  as_factor(computer$pri_type)

rm(com_variation)

# Data Transformation -----------------------------------------------------

## data frame is now tidy 
## I would like to explore characters interaction with computer
## create a subset containing interactions by "person" {char_type}
## also need to look at missing values and the structure of the dataset
## in order to generate some ideas for analysis

# Task 1: subset data to create new "person" data frame
person <- computer %>%
  filter(char_type == "Person") %>% 
  select(-char_type)

# Task 2: Transform data so only primary type is included
## I am interested in main theme of each characters interaction with VUI
## I don't require all the detail for my analysis
person <- 
  person %>%  
  # include only the rows where ["type" = "primary type"]
  filter(type == pri_type)

# Task 3: Explore Missing Values and transform data if needed
## missing values are all in domain and sub_domain variables
person %>% 
  summarise("total" = nrow(person),
            "no domain" = sum(is.na(domain)),
            "no sub domain" = sum(is.na(sub_domain)),
            "no domain or sub domain" = sum(is.na(domain) & is.na(sub_domain)))

### more sub domains missing than domains. 8 have both missing
### I don't feel this is "missing data' so change NAs to "Unspecified"
person$domain <- 
  # create a new level within "domain" variable
  fct_expand(person$domain, domain = "Unspecified") %>%
  # replace NA values with this new "Unspecified" level
  replace_na(list(domain = "Unspecified"))

person$sub_domain <- 
  # repeat for sub_domain variable
  fct_expand(person$sub_domain, sub_domain = "Unspecified") %>%
  replace_na(list(sub_domain = "Unspecified"))

save(person, file = "2021_08_17_Star_trek/person.RDS")
# Exploratory Data Analysis -----------------------------------------------

skim(person)

## visualise categorical data to identify any themes
person %>% 
  ggplot()+
  geom_bar(aes(x = fct_rev(fct_infreq(domain))))+
  coord_flip()

person %>% 
  ggplot()+
  geom_bar(aes(x = fct_rev(fct_infreq(sub_domain))))+
  coord_flip()

person %>% 
  ggplot()+
  geom_bar(aes(x = fct_rev(fct_infreq(pri_type))))+
  coord_flip()

person %>% 
  group_by(char) %>% 
  count %>% 
  arrange(desc(n))

person %>%
  # main characters only (top 10)
  mutate(char = fct_lump(person$char, 10)) %>% 
  ggplot(aes(y = fct_rev(fct_infreq(char))))+
  geom_bar()

# plot sub domains of each domain
## create function
domain_plot <- 
  function(x){
    person %>% 
      filter(domain == x) %>% 
      ggplot()+
      geom_bar(aes(x = sub_domain))+
      coord_flip()+
      labs(title = x)
  }
# apply function iteratively
map(unique(person$domain), domain_plot)


# work on plot to publish -------------------------------------------------

# basic plot showing main domains where VUI used by all characters
person %>% 
  ggplot()+
  geom_bar(aes(x = fct_rev(fct_infreq(domain))))+
  coord_flip()

# explore use by individual characters

## create vector containing names of top 5 characters 
main <- person %>% 
  group_by(char) %>% 
  count %>% 
  arrange(desc(n)) %>% 
  head(5) %>% 
  pull(char)

### filter person data-frame by main characters
main <- person %>% 
  filter(char %in% main)

## main plot showing characters and number VUI interactions
main %>% 
  mutate(char = fct_rev(fct_infreq(char))) %>% 
  ggplot()+
  geom_bar(aes(x = char))+ 
  coord_flip()

## sub plot showing characters and domains: faceted plot
main %>%
  mutate(domain = fct_lump(domain, 4),
         char = fct_infreq(char)) %>% 
  filter(domain != "Other") %>% 
  mutate(domain = fct_relevel(domain,
                              c("Analysis", "InfoSeek",
                                "Entertainment", "IoT"))) %>% 
  ggplot()+
  geom_bar(aes(x = domain))+ 
  coord_flip()+
  facet_wrap(~ char, ncol = 1)
  

# can I visualise facets as proportions rather than counts

plot_data <- 
  main %>% 
  mutate(domain = fct_lump(domain, n = 4),
         char = fct_infreq(char)) %>% 
  filter(domain != "Other") %>% 
  group_by(char, domain) %>% 
  tally %>% 
  mutate(prop = n / sum(n)) %>% ungroup %>% 
  group_by(domain) %>% 
  mutate(domain_total = sum(n),
         domain_prop = n / sum(n)) 

entertainment <- plot_data %>% 
  filter(domain == "Entertainment") %>% 
  group_by(char) %>% 
  arrange(desc(prop)) %>% 
  pull(char) %>% 
  as.character()


plot_data %>% 
  ggplot()+
  geom_col(aes(x = fct_reorder(domain, domain_total), 
               y = prop, 
               fill = domain))+
  coord_flip()+
  scale_y_continuous(labels = NULL, breaks = NULL)+
  theme_minimal()+
  scale_fill_ordinal()+
  labs(x = NULL, y = NULL)+
  scale_x_discrete()+
  facet_wrap(~ char, ncol = 1)

faceted_plot
