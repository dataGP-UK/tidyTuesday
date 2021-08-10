#  set up -----------------------------------------------------------------
library(tidyverse)
library(skimr)

tuesdata <- tidytuesdayR::tt_load('2021-07-27')
olympics <- tuesdata$olympics
regions <- tuesdata$regions


# initial observations and quality checks ---------------------------------

glimpse(olympics)
# 15 columns and 271116 rows
# each row appears to relate to an individual athlete

str(olympics)
# id/age/height/weight/year : all numeric (should year be numeric or factor?)
# rest are character variables - no factors

olympics$id <-  as.character(olympics$id) # switch id to character type

skim(olympics)
# 271116 rows - I thought each represented an individual athlete
## in fact there are 134731 unique names: cross check with id number
### are there the same number of unique id or do some athletes have same name
#### this may represent some athletes competing in more than one sport or maybe
#### olympics???

# winter and summer included
## filter for summer only and within last 50 years
summer_olympics_2012 <- olympics %>% 
  filter(season == "Summer" & year == 2012)

summer_olympics <- olympics %>% 
  filter(season == "Summer")

# medal data incomplete
## i presume this is athletes who haven't medalled
### explore a small subset to check this
#### could cheeck that each event has a gold, silver, and bronze medal

summer_olympics_2012 %>% 
  filter(event == "Athletics Men's 10,000 metres") %>% 
  view

# EDA  --------------------------------------------------------------------

skim(summer_olympics)

# focus on 2012 London
skim(summer_olympics_2012)
# age

summer_olympics_2012 %>% 
  ggplot(aes(x = age)) +
  geom_histogram()

## youngest athletes
summer_olympics_2012 %>% 
  filter(age < 14)%>% 
  pull(name)

## oldest athletes
summer_olympics_2012 %>% 
  filter(age > 65) %>% pull(name)

### could explore oldest and youngest medal winners??

# height
summer_olympics_2012 %>% 
  ggplot(aes(x=height))+
  geom_histogram(binwidth = 5)

## shortest athletes
summer_olympics_2012 %>% 
  arrange(height) %>% 
  head()

## tallest athletes
summer_olympics_2012 %>% 
  arrange(desc(height)) %>% 
  head

# weight
summer_olympics_2012 %>% 
  ggplot(aes(x=weight))+
  geom_histogram(binwidth = 5)

## lightest athletes
summer_olympics_2012 %>% 
  filter(weight < 29)%>% 
  View

## heaviest athletes
summer_olympics_2012 %>% 
  filter(weight > 170)%>% 
  View

# stratify these distributions by sex
summer_olympics_2012 %>% 
  ggplot(aes(x = age)) +
  geom_density(aes(fill = sex), alpha = 0.5, adjust  = 1.75 )

summer_olympics_2012 %>% 
  ggplot(aes(x = height)) +
  geom_density(aes(fill = sex), alpha = 0.5, adjust  = 1.75 )

summer_olympics_2012 %>% 
  ggplot(aes(x = weight)) +
  geom_density(aes(fill = sex), alpha = 0.5, adjust  = 1.75 )

# explore number of events in each olympic games
summer_olympics %>% 
  group_by(year) %>% 
  summarise("events" = n_distinct(event)) %>% 
  ggplot(aes(year, events))+
  geom_line()+
  theme_bw()

## increasing every year with esp rapid rise between 1970 and 2010
## does this mean that more events have been included?
### can this rate be measured?
### look at split between males and females
summer_olympics %>% 
  group_by(year, sex) %>% 
  summarise("events" = n_distinct(event)) %>% 
  ggplot(aes(year, events))+
  geom_area(aes(fill = sex))+
  theme_bw()


## indicates that rise is mainly due to increased events for female athletes
### need to annotate or amend plot to recognise war time
### make plot prettier and upload to twitter

# explore number of nations represented at each summer olympics

summer_olympics %>% 
  group_by(year) %>% 
  summarise(nations = n_distinct(noc)) %>% 
  ggplot(aes(year, nations)) +
  geom_line()

## steady increase with dip in 1980 ? LA or Moscow and boycotts by USA/USSR
### increeasingly global competitions: idea for a series of maps/continents
### showing percentage of countries from continent represented.

## focus on large countries: lump rest together

summer_olympics$noc <- summer_olympics$noc %>% fct_infreq()

summer_olympics_gold <- summer_olympics %>% 
  filter(medal == "Gold")
summer_olympics_modern <- summer_olympics %>% 
  filter(year >1970)

summer_olympics %>% 
  mutate(noc = fct_lump(summer_olympics$noc, 5))%>% 
  group_by(noc, year) %>% 
  summarise(n=n()) %>% 
  #filter(noc != "Other") %>% 
  ggplot(aes(year, n))+
  geom_col(aes(fill = noc), position = "fill")
# story of russia and germany(east) plus USA - Moscow / LA boycotts


