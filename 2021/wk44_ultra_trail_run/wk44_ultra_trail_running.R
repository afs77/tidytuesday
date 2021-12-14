#Amanda Fanelli for TidyTuesday week 44, 10/27/2021

#Getting the data ----
ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

#Loading packages ----

library(tidyverse)
library(extrafont)
library(ggtext)
library(patchwork)

#Cleaning the data ---

#Checking to see if each key has only one value in race

race %>%
  count(race_year_id) %>%
  filter(n>1)
#Only one key in race

#joining datasets and separating date

race_rankings <- race %>%
  left_join(ultra_rankings, by = "race_year_id") %>%
  separate(date, into = c("year", "month", "day"), sep = "-")

#Grouping by year and gender

year_gender <- race_rankings %>%
  group_by(year, gender) %>%
  filter(!(is.na(gender))) %>%
  summarize(total_participants = n()) 

#percentage

percent_women <- year_gender %>%
  pivot_wider(names_from = gender,
              values_from = total_participants) %>%
  mutate(percent_w = 100*(W/(W+M))) %>%
  arrange(percent_w)

#Cleaning race_rankings, grouping by gender and country

gender_country <- race_rankings %>%
  mutate(country = str_replace_all(country, c("LA, United States" = "United States",
                                              "PA, United States" = "United States",
                                              "FL, United States" = "United States",
                                              "Myoko, Japan" = "Japan",
                                              "Hong Kong, China" = "China"))) %>%
  group_by(country, gender) %>%
  filter(!(is.na(country))) %>%
  filter(!(is.na(gender))) %>%
  summarize(total_participants = n()) 

#Top 10 countries with the most male participants

top_10_men <- gender_country %>%
  ungroup() %>%
  filter(gender == "M") %>%
  slice_max(total_participants, n = 10)%>%
  arrange(total_participants) %>%
  mutate(country = factor(country, levels = country))

#Top 10 countries with the most female participants

top_10_women <- gender_country %>%
  ungroup() %>%
  filter(gender == "W") %>%
  slice_max(total_participants, n = 10) %>%
  arrange(total_participants) %>%
  mutate(country = factor(country, levels = country))


#Plot ----

#Barplot participants for each gender by year

p1 <- ggplot(year_gender, aes(x = gender, y = total_participants, fill = gender)) +
  geom_col(position = "dodge", color = "black") +
  geom_text(aes(label = total_participants), vjust = -0.5, 
            family = "Garamond", size = 5) +
  facet_wrap(.~year, nrow = 2) +
  ylim(0,22000) +
  scale_fill_manual(values = c("deepskyblue3", "salmon"), name = "Gender",
                    labels = c("Men", "Women")) +
  labs(x = element_blank(), y = "Total of participants",
       title = "Overall, the number of participants increased from 2012 to 2019, 
       but decreased after the pandemics.",
       subtitle = "The majority of participants are men. The share of women 
       participation ranged from 13.02% to 16.82%, being the largest in 2020") +
  theme_bw() + theme(axis.text.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     text = element_text(family = "Garamond"),
                     plot.title = element_markdown(size = 22),
                     plot.subtitle = element_markdown(size = 19),
                     axis.text = element_text(size = 16),
                     axis.title.y = element_text(size = 18, vjust = -8,
                                                 color = "black"),
                     legend.title = element_text(size = 18),
                     legend.text = element_text(size = 18),
                     strip.background = element_rect(fill = "dodgerblue4"),
                     strip.text = element_text(face = "bold", color = "white", size = 16))

#Barplot male participants per country

p2 <- ggplot(top_10_men, aes(x = country, y = total_participants)) +
  geom_col(color = "black", fill = "deepskyblue3") +
  labs(y = "Total participants", x = NULL) +
  coord_flip() +
  labs(title = "The countries with the most **male** participants were <br> USA,
       France and UK.",
       subtitle = "**Spain, Andorra and Croatia** are in the **top 10 countries**<br>
       with the most male participants, but not in the female ranking") +
  theme_bw() + theme(text = element_text(family = "Garamond"),
                     plot.title = element_markdown(size = 22),
                     plot.subtitle = element_markdown(size = 19),
                     axis.text.x = element_text(size = 15, color = "black"),
                     axis.text.y = element_text(size = 16, color = "black"),
                     axis.title = element_text(size = 18, color = "black"),
                     plot.margin = margin(0,10,0,0))

#Barplot female participants per country

p3 <- ggplot(top_10_women, aes(x = country, y = total_participants)) +
  geom_col(color = "black", fill = "salmon") +
  labs(y = "Total participants", x = NULL) +
  coord_flip() +
  labs(title = "The countries with the most **female** participants were <br> also USA, 
       France and UK.",
       subtitle = "**New Zeland, Canada and Australia** are in the **top 10 countries** <br>
       with the most female participants, but not in the male ranking") +
  theme_bw() + theme(text = element_text(family = "Garamond"),
                     plot.title = element_markdown(size = 22),
                     plot.subtitle = element_markdown(size = 19),
                     axis.text.x = element_text(size = 15, color = "black"),
                     axis.text.y = element_text(size = 16, color = "black"),
                     axis.title = element_text(size = 18, color = "black"))


#Combining plots
patchwork <- p1/(p2|p3)
patchwork + plot_annotation(title = "Trends in Ultra Trail Running from 2012 to 2021",
                            caption = "Data from Benjamin Nowak by way of International Trail Running Association (ITRA)| Amanda Fanelli for #Tidytuesday",
                            theme = theme (plot.title = element_text(family = "Garamond", size = 28),
                                           plot.caption = element_text(family = "Garamond", size = 16, hjust = 0)))


ggsave("tidytuesday/wk44_ultra_trail_run/ultra_trail.png", width = 19, height = 16, 
       units = "in")
