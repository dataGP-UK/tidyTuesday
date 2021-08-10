library(tidyverse)
library(ggplot2)
library(ggforce)
library(concaveman)
library(Rcpp)
library(here)

tuesdata <- tidytuesdayR::tt_load('2021-07-27')

tbl_olympics <- tibble(tuesdata$olympics)
tbl_regions <- tibble(tuesdata$regions)

# create new column describing discipline within sport
tbl_olympics <- 
  tbl_olympics %>% 
  mutate(substring = paste(sport, 
                           if_else(sex=="M", "Men's ", "Women's ")),
         discipline = str_replace(string = event,
                                  pattern = substring,
                                  replacement = "")) %>% 
  select(-c(substring, event, games))

# plot ht wt of different disciplines -------------------------------------

tbl_plot <- tbl_olympics %>% 
  filter(season == "Summer" &
           year >= 1992 &
           !is.na(medal) &
           sex == "M" &
           sport =="Athletics" &
           discipline != "Other" &
           !is.na(weight) & 
           !is.na(height)) 

tbl_plot$discipline <- 
  fct_collapse(tbl_plot$discipline, 
               "Long Distance Runners" = c("Marathon", "10,000 metres", 
                                           "5,000 metres"),
               "Middle Distance Runners" = c("1,500 metres", "800 metres"),
               "Sprinters" = c("400 metres", "200 metres", "100 metres"),
               "Throwers" = c("Hammer Throw", "Discus Throw", "Shot Put"),
               other_level = "Other")

tbl_plot_2 <- tbl_plot %>% 
  filter(!discipline %in% c("Other", "Middle Distance Runners"))

tbl_plot_2 %>%
  ggplot(aes(height, weight, col=discipline)) +
  geom_point(size=0.2, show.legend = FALSE) +
  geom_mark_hull(aes(col= discipline, 
                     fill= discipline,
                     label = discipline), 
                 show.legend = FALSE, 
                 expand = unit(3, "mm"), 
                 concavity = 10,
                 con.cap = 0,
                 con.border = "all") +
  theme_bw(base_size = 12) + 
  labs(x="Height", 
       y="Weight", 
       title= "Runners vs. Throwers",
       subtitle = "Size of Olympic Medal Winners Since 1992 (Men)",
       caption="github.com/dataGP-UK") + 
  scale_x_continuous(breaks = seq(150,210,5) ) +
  scale_y_continuous(breaks = seq(50,130,10) ) + 
  NULL

## can I identify which athletes are in subset that involves intersection of
## all 3 groups?

middle_distance <- 
  tbl_plot %>% 
  filter(discipline == "Middle Distance")

long_distance <- 
  tbl_plot %>% 
  filter(discipline == "Long Distance")

sprinting <- 
  tbl_plot %>% 
  filter(discipline == "Sprinting")

groups <- list(middle_distance, long_distance, sprinting)

