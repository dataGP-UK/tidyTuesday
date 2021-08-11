# set up ------------------------------------------------------------------
library(tidyverse)
tuesdata <- tidytuesdayR::tt_load('2021-08-10')
chain_investment <- tuesdata$chain_investment

# total infrastructure investment -----------------------------------------
library(ggstream)

chain_investment %>% 
  filter(meta_cat == "Total infrastructure" &
           !category %in% c("Federal", "Private", "S&L")) %>% 
  mutate(category = fct_recode(category, 
                               "Digital" = "Total digital infrastructure",
                               "Social" = "Total social infrastructure",
                               "Basic" = "Total basic infrastructure"),
         category = fct_relevel(category,
                                "Digital", "Social", "Basic"))%>% 
  ggplot()+
  geom_stream(aes(year, gross_inv_chain, fill = category),
              type = "mirror", show.legend = T, bw = 0.5)+
  labs(title = "Total infrastructure Investment (USA 1947-2017)",
       subtitle = "Total investment is increasing with Digital showing the largest recent increase.")+
  labs(y = "Gross Investment (chained 2012 dollars)",
       x = "Year",
       fill = NULL)+
  scale_y_continuous(breaks = NULL)+
  scale_x_continuous(breaks = seq(1940, 2020, 10),limits = c(1945,2020))+
  theme_minimal()+
  scale_fill_ordinal()

