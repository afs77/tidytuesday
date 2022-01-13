#Amanda Fanelli for Tidytuesday week 2 2022

#Reading the data ----
colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')


#Loading packages ---- 
library(tidyverse)
library(showtext)
library(ggtext)
library(ggdark)


#Exploring dataset ----
colony %>%
  count(year)

View(colony %>%
  count(year,months))

View(colony %>%
       count(state))

us_colony <- filter(colony, state == "United States") %>%
  filter(year < 2019) %>%
  mutate(months = fct_inorder(months))

#plot ----
font_add_google("Bangers")
font_add_google("Rokkitt")
showtext_auto()

ggplot(us_colony, aes(x = year, y = colony_lost_pct)) +
  geom_point(size = 0.5) +
  geom_line(size = 0.5)+
  facet_wrap(~months, ncol = 4) +
  labs(x = NULL, y = "Percent of colony loss",
       title = "The loss of bee colonies in the US from 2015 to 2018",
       subtitle = "Colony loss was higher in **winter** (jan-mar) and **fall** (oct-dec) months.",
       caption = "Data from USDA | Amanda Fanelli for #Tidytuesday") +
  dark_mode(theme_bw()) + 
  theme(plot.title = element_markdown(color = "#FDCB03", family = "Bangers", size = 22),
        plot.subtitle = element_markdown(family = "Rokkitt", size = 14),
        plot.caption = element_markdown(family = "Rokkitt", size = 10),
        axis.title = element_markdown(family = "Rokkitt", color = "#FDCB03", size = 16),
        axis.text = element_markdown(family = "Rokkitt", size = 12),
        strip.background = element_rect(fill = "#FDCB03"),
        strip.text = element_text(color = "black", size = 12))


ggsave("2022/wk2/bee_colonies.png", width = 4,  height = 2.5)
