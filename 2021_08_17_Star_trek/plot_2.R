library(tidyverse)
library(showtext)

computer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv')

tng_color_palette <- c("#c1c730",
                       "#a71313",
                       "#2b53a7",
                       "#000000",
                       "#d6a444",
                       "#B85900",
                       "#288298",
                       "#787878")

font_add_google("Bebas Neue", "bebas")
font_add_google("Fjalla One", "fjalla")
font_add_google("Orbitron", "orbitron")
font_add_google("Barlow Condensed", "barlow")

showtext_auto()
showtext_opts(dpi = 96)

# r plot-command-type-frequencies

command_type_freq_data_all <- computer %>%
  filter(char_type != "Computer") %>%
  mutate(type =
           case_when(type == "command" ~ "Command",
                     type == "question" ~ "Question",
                     type == "Password" |
                       type == "Comment" ~ "Other",
                     TRUE ~ type)) %>%
  replace_na(list(domain = "Other"))

command_type_freq_data_all$type2 <- fct_infreq(command_type_freq_data_all$type)
table(command_type_freq_data_all$type2)

command_type_summary_data <- command_type_freq_data_all %>% 
  group_by(type2) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  mutate(cumsum = cumsum(n)) %>% 
  mutate(prop = 1-cumsum/sum(n)) %>% 
  mutate(y_label = case_when(
    is.na(lag(prop)) ~ prop + (1-prop) / 2,
    TRUE ~ prop + (lag(prop) - prop) / 2
  ))

p_command_types <- ggplot(data = command_type_freq_data_all,
                          mapping = aes(x = 1)) +
  geom_bar(mapping = aes(fill = type2),
           position = "fill",
           color = "white",
           show.legend = FALSE) +
  scale_y_continuous(breaks = c(0,0.5,1),
                     labels = c("0%","50%","100%")) +
  scale_x_continuous(breaks = NULL) +
  labs(
    title = "What interactions are most common?",
    subtitle = "Commands and wake words are the most common interactions",
    caption = "Data: Tidy Tuesday | Plot: dataGP-UK",
    x = element_blank(),
    y = element_blank()
  ) +
  annotate("text", 
           x = rep(1,length(command_type_summary_data$y_label)),
           y = command_type_summary_data$y_label,
           label = command_type_summary_data$type2,
           family = "barlow",
           color = "white",
           size = 5) +
  theme_minimal() +
  scale_fill_manual(values = tng_color_palette) +
  theme(
    plot.title = element_text(family = "bebas"),
    plot.subtitle = element_text(family = "barlow"),
    axis.text = element_text(family = "barlow"),
    axis.title = element_text(family = "barlow"),
  )

p_command_types

ggsave("2021_08_17_Star_trek/startrekcommands.png", units = "cm",width = 10, height = 6)

