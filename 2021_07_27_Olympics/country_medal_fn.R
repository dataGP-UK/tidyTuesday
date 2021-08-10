# set up ------------------------------------------------------------------
library(tidyverse)
tuesdata <- tidytuesdayR::tt_load('2021-07-27')
olympics <- tuesdata$olympics
rm(tuesdata)


# filter data -------------------------------------------------------------
summer_medals <- olympics %>% 
  filter(medal %in% c("Gold", "Silver", "Bronze") & 
           season == "Summer") %>% 
  select(sex, year, city, noc, sport, event, medal) %>% 
  distinct

# test
summer_medals[summer_medals %>% duplicated,] %>% 
  arrange(event, year, medal) %>% nrow

# code --------------------------------------------------------------------
nation <- summer_medals$noc %>% 
  unique 

medal_plot <- function(nation){
  summer_medals %>%
    filter(noc == nation) %>% 
    mutate(sport = fct_lump(sport, 10)) %>% 
    group_by(sport, medal) %>% 
    tally() %>% 
    mutate(total_medals = sum(n)) %>%
    ungroup() %>% 
    mutate(sport = fct_reorder(sport, total_medals, min),
           sport = fct_relevel(sport, "Other", after = 0)) %>% 
    ggplot()+
    geom_col(aes(sport, n, 
                 fill = fct_relevel(medal, c("Bronze", "Silver", "Gold"))), 
             colour = "black")+
    coord_flip()+
    scale_fill_manual(values=c("#cd7f32", "#C0C0C0", "#FFDF00")) +
    labs(x = "Sport", y = "Number of Medals Won",
         title = paste("Medals won by", nation, "Summer Olympics team (1896-2016)"),
         subtitle = "",
         caption = "Data from https://github.com/rfordatascience/tidytuesday.") +
    theme(legend.title = element_blank(), 
          legend.background = element_rect(color = "black", linetype = "solid"),
          legend.position = c(.95, .5),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6),
          legend.key.size = unit(0.5, 'cm'))+
    guides(fill =  guide_legend(nrow = 1,reverse = TRUE))
}

plots <- map(nation, medal_plot)
plots


