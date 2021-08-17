tuesdata <- tidytuesdayR::tt_load(2021, week = 33)
chain_investment <- tuesdata$chain_investment
rm(tuesdata)

# Single area plot --------------------------------------------------------

# Load extensions for data handling
library(tidyverse)

# Filter for main domains only
main <- chain_investment %>%
  # Filter thanks to "group_num" attribute  
  filter(group_num == 1) %>% 
  # New column with simpler names          
  mutate(lab = case_when(       
    category == "Total basic infrastructure" ~ "Basic",
    category == "Total digital infrastructure" ~ "Digital",
    category == "Total social infrastructure" ~ "Social"))

ggplot(data = main,
  # Choose newly created lab column for fill
       aes(x = year, y = gross_inv_chain, fill = lab))+
  geom_area()+
  labs(title = "Investment in US infrastructure",
       x = "Year",
       y = "Investment (millions of 2012 dollars)")+
  theme_minimal()

# Reorder factors
  # convert character vector into factor and assign levels
main$lab <- factor(main$lab,
                 c("Digital", "Social", "Basic"))
# Plot again
ggplot(data = main,
       aes(x = year, y = gross_inv_chain, fill =lab))+
  geom_area()+
  labs(title = "Investment in US infrastructure",
       x = "Year",
       y = "Investment (millions of 2012 dollars)")+
  theme_minimal()


# add the names of the factors directly to the right of the graph ---------

# new data frame containing value taken by each factor for the last year (2017) 
# sum these values from the first factor (Basic) to the last factor (Digital)
# use these values as y positions for our legend labels.

final <- main %>%
  # Keep only 2017 value
  filter(year == "2017") %>% 
  # Inverse factor order (first is at the bottom of plot)
  arrange(desc(lab)) %>% 
  # Create new column ypos and fill with cumulative sum of invest for 2017  
  mutate(ypos = cumsum(gross_inv_chain))

# use 'final' data frame to directly position labels on plot with geom_text().
ggplot(data = main,
       aes(x = year, y = gross_inv_chain, fill = lab))+
  geom_area()+
  labs(title = "Investment in US infrastructure",
       x = "Year",
       y="Investment (millions of 2012 dollars)")+
  # Different data than the main plot ('final' vs 'main')
  geom_text(data = final, 
  # y and lab in the aes() (change between labels)         
            aes(y = ypos,label = lab),  
  # x is the same for all labels
    x = 2017)+
  theme_minimal()

# modify the code to improve the labels positioning
ggplot(data = main,
       aes(x = year, y = gross_inv_chain, fill = lab))+
  geom_area()+
  labs(title = "Investment in US infrastructure",
       x = "Year",
       y = "Investment (millions of 2012 dollars)")+
  geom_text(data = final,
            aes(y = ypos - 150000 ,label = lab),
 # Decrease label y position
            x = 2017,
 # Left align text
            hjust = 0)+
  theme_minimal()+
  # Expand x axis to leave space for labels
  scale_x_continuous(limits = c(1947,2022),
                     breaks=c(1950,1980,2010))+ 
  # No need for fill legend anymore !
  guides(fill=FALSE)+
  theme_minimal()

# associating the same colors for our filled areas and matched lab --------

# Create color palette
pal<-c("#0F4C5C","#E36414","#9A031E")

# Specify color palette with a new column inside main
main <- main %>%
  mutate(col_lab = case_when(
    lab == "Basic"~"#0F4C5C",
    lab == "Social"~"#E36414",
    lab == "Digital"~"#9A031E"))

# use inside ggplot() for both fill and colour arguments.
A <- ggplot(data = main,
       aes(x = year, y = gross_inv_chain, fill = lab))+
  geom_area()+
  labs(title = "Investment in US infrastructure",
       x = "Year",
       y = "Investment (millions of 2012 dollars)")+
  # geom_text(data = final,
  #           aes(y = ypos - 150000 ,
  #               label = lab,
  # # Add color inside the aes()    
  #               colour = lab),   
  #           x = 2017, 
  #           hjust = 0)+
  # Specify fill and colour palette
  scale_fill_manual(breaks = main$lab,
                    values = main$col_lab)+
  scale_color_manual(breaks = main$lab,
                     values = main$col_lab
  )+
  theme_minimal()+
  scale_x_continuous(limits = c(1947,2022),
                     breaks=c(1950,1980,2010))+
  guides(fill=FALSE,
  # Hide color legend)    
         color=FALSE)+
  theme_minimal()

A

# Faceted area plots ------------------------------------------------------

# select sub-categories with modifications to shorten he name of the longest
sub <- chain_investment%>%
  filter(group_num == 4| group_num == 17| group_num == 22) %>%
  # Create new column with shortest names
  mutate(lab = case_when(
    category == "Conservation and development" ~ "Conservation",
    category == "Private computers in NAICS 515, 517, 518, and 519" ~ "Computers",
    category == "Private software in NAICS 515, 517, 518, and 519" ~ "Software",
    category == "Private communications equipment in NAICS 515, 517, 518, and 519" 
    ~ "Com. equipment",
    category == "Private communications structures" ~ "Com. structures",
    TRUE ~ category))

# produce faceted area plots.
ggplot(data = sub,
       aes(x = year, y = gross_inv_chain, fill = lab))+
  geom_area()+
  facet_wrap(~ meta_cat, ncol = 1, strip.position = "top")

# multiple labels so put directly in the graph
final_sub <- sub %>%
  filter(year == "2017") %>%
  # Group by major fields before cumulative sums
  group_by(group_num) %>%    
  arrange(desc(category)) %>%
  # still grouped so cumsum will act group wise
  mutate(ypos = cumsum(gross_inv_chain)) %>%
  ungroup()

# use this data frame to position the labels with geom_text()
ggplot(data = sub,
       aes(x = year, y = gross_inv_chain, fill = lab))+
  geom_area()+
  facet_wrap(~ meta_cat, ncol = 1, strip.position = "top")+
  geom_text(data = final_sub,
            aes(y = ypos - 20000, label = lab, color = lab),
            x = 2018, hjust = 0, fontface = "italic")+
  scale_x_continuous(limits = c(1947,2030),
                     breaks = c(1950,1970,1990,2010))+
  guides(fill=FALSE, color=FALSE)+
  labs(title = "Evolution of investment on US infrastructures",
       x = "Year",
       y = "Investment (millions of 2012 $)")+
  theme_minimal()

# decrease text size and use ggrepel to optimize position of labels
library(ggrepel)

facet <- ggplot(data = sub,
                aes(x = year, y = gross_inv_chain, fill = lab))+
  geom_area()+
  facet_wrap(~ meta_cat, ncol = 1, strip.position = "top")+
  # Replace geom_text()
  geom_text_repel(data = final_sub,
                  aes(y = ypos - 20000, 
                      label = lab, color = lab),
    # Repel on y-axis (align on x-axis)
                  direction='y',
    # Decrease size
    x = 2018, hjust = 0, size = 3,               
    fontface = "italic")+
  scale_x_continuous(limits = c(1947,2030),
                     breaks = c(1950,1970,1990,2010))+
  guides(fill=FALSE, color=FALSE)+
  labs(title = "Evolution of investment on US infrastructures",
       x = "Year",
       y = "Investment (millions of 2012 $)")+
  theme_minimal()

facet


# customize the color palette ---------------------------------------------


# Create color palettes based on color gradients of former legend

pal_basic <- c(
  "#030F12",
  "#0E4958",
  "#2CB9DD",
  "#73D1E8",
  "#B9E8F4"
)

pal_social <- c(
  "#83390B",
  "#E36414",
  "#F4AA7C"
)

pal_digital <- c(
  "#3C010C",
  "#9A031E",
  "#FA0F3A",
  "#FC738C"
)

# Add color palette to data
sub <- sub %>%
  mutate(col_lab = case_when(
    lab == "Water"~pal_basic[1],
    lab == "Transportation"~pal_basic[2],
    lab == "Sewer"~pal_basic[3],
    lab == "Power"~pal_basic[4],
    lab == "Conservation"~pal_basic[5],
    lab == "Public safety"~pal_social[1],
    lab == "Health"~pal_social[2],
    lab == "Education"~pal_social[3],
    lab == "Software"~pal_digital[1],
    lab == "Computers"~pal_digital[2],
    lab == "Com. structures"~pal_digital[3],
    lab == "Com. equipment"~pal_digital[4]
  ))

# Add color to plots
B <- facet +  
  scale_fill_manual(breaks = sub$lab,
                    values = sub$col_lab)+
  scale_color_manual(breaks = sub$lab, 
                     values = sub$col_lab)

B

# join it all together ----------------------------------------------------
library(cowplot)

A1 <- A +
  theme(title = element_blank(),
    axis.text = element_text(size = 20),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_blank())+
  annotate(geom = "segment",
           x = 1947, xend = 2017,
           y = 600000, yend = 600000, color="#343a40")+
  annotate(geom = "segment",
          x = 1947, xend = 2017,
          y = 300000, yend = 300000, color="#343a40")+
  annotate(geom = "text",
           x = 1947, y = 630000,
           label="600 billion $",
           color="#343a40", hjust=0, size = 4)+
  annotate(geom = "text", 
           x = 1947, y = 330000,
           label = "300 billion $",
           color = "#343a40", hjust = 0, size = 4)+
  geom_area(colour = "white")
A1

B1<- B + 
  coord_cartesian(clip = "off")+
  labs(
    x="",
    y=""
  )+
  theme_minimal()+
  theme(title = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.grid = element_blank()
  )

B1

ggdraw() +
  draw_plot(A1, x = 0, y = 0, width = 0.5, height = 0.9) +
  draw_plot(B1, x = 0.48, y = 0.035, width = 0.51, height = 1)+ 
  draw_plot_label(
    label = "Investment in US infrastructure",  
    size = 20,hjust=0,color="#343a40",
    x = 0.04, y = 0.97)+
  draw_plot_label(
    label = "from 1947 to 2017",  
    size = 20,hjust=0,color="#343a40",
    x = 0.04, y = 0.90)+
  draw_text(
    text = "Gross investment in 2012 dollars",  
    size = 10,hjust=0,color="#343a40",
    x = 0.04, y = 0.80)+
  draw_text(
    text = "Digital",  
    size = 12,hjust=0,color="#9A031E",
    x = 0.47, y = 0.85)+
  draw_text(
    text = "Social",  
    size = 12,hjust=0,color="#E36414",
    x = 0.47, y = 0.55)+
  draw_text(
    text = "Basic",  
    size = 12, hjust=0,color="#0F4C5C",
    x = 0.47, y = 0.25)+
  draw_text(
    text = "Data: Bureau of Economic Analysis",  
    size = 8,hjust = 0, color = "#343a40", angle = 90,
    x = 0.025, y = 0.08, vjust = 0)

