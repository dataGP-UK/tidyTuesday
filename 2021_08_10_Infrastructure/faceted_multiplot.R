# set up ------------------------------------------------------------------
library(tidyverse)
tuesdata <- tidytuesdayR::tt_load('2021-08-10')
chain_investment <- tuesdata$chain_investment


# plots -------------------------------------------------------------------

# Filter for main domains only
main <- chain_investment %>%
  filter(group_num == 1) %>%    # Filter thanks to "group_num" attribute        
  mutate(lab = case_when(category == "Total basic infrastructure"~"Basic",
                         category == "Total digital infrastructure"~"Digital",
                         category == "Total social infrastructure"~"Social"))

# Reorder factors
main$lab <- factor(main$lab,  c("Digital",
                                "Social",
                                "Basic"))

# create labels to replace legend
final <- main %>%
  filter(year == "2017") %>%              
  arrange(desc(lab)) %>%                
  mutate(ypos = cumsum(gross_inv_chain))       

# Create color palette
pal <- c("#0F4C5C","#E36414","#9A031E")

# Specify color palette with a new column inside main
main <- main %>%
  mutate(col_lab = case_when(lab=="Basic"~"#0F4C5C",
                             lab=="Social"~"#E36414",
                             lab=="Digital"~"#9A031E"))

#plot
ggplot(data=main,
       aes(x=year,y=gross_inv_chain,fill=lab))+
  geom_area()+
  labs(title = "Evolution of investment on US infrastructures",
       x = "Year",
       y = "Investment (millions of 2012 $)")+
  geom_text(data = final,
            aes(y = ypos-150000, label=lab, color=lab),
            x = 2018,
            hjust = 0)+
  scale_fill_manual(breaks = main$lab,
                    values = main$col_lab)+
  scale_color_manual(breaks = main$lab,
                     values = main$col_lab)+
  scale_x_continuous(limits = c(1947,2022),
                     breaks = c(1950,1980,2010))+
  guides(fill = FALSE, 
         color = FALSE)+
  theme_minimal()



