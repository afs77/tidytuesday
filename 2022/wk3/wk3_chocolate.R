#Amanda Fanelli for #Tidytuesday Wk3

#Loading data----
tuesdata <- tidytuesdayR::tt_load('2022-01-18')

chocolate <- tuesdata$chocolate

#Loading packages----
library(tidyverse)
library(showtext)
library(ggtext)
library(patchwork)

#Data wrangling----

#Total countries
countries <- chocolate %>%
  count(country_of_bean_origin)

#Top countries with 50 or more entries
top_countries <- countries %>%
  filter(n >= 50 & countries != "Blend") %>%
  arrange(desc(n)) 

#Filtering only top countries. For each country, counting the amount of entries in each grade.
#Convert rating(numeric) as factor
chocolate_grades_bean <- chocolate %>%
  group_by(country_of_bean_origin, rating) %>%
  summarize(count = n()) %>%
  filter(country_of_bean_origin %in% top_countries$country_of_bean_origin) %>%
  mutate(rating = as_factor(rating)) %>%
  mutate(rating = fct_relevel(rating, c("1","1.5", "1.75", "2", "2.25", 
                                        "2.5", "2.75", "3", "3.25", 
                                        "3.5", "3.75", "4")))
#Making implicit NA visible (observations for all possible grades)
#Also adding percent of entries
all <- chocolate_grades_bean %>%
  expand(rating)

chocolate_grades_countries <- all %>% 
  left_join(chocolate_grades_bean) %>%
  group_by(country_of_bean_origin) %>%   
  mutate(total = sum(count, na.rm = TRUE),
         percent = (count/total)*100) 



#plot----
#Add fonts
font_add_google("Alfa Slab One", "Alfa")
font_add_google("Gothic A1", "Gothic")
showtext_auto()


p1 <- ggplot(chocolate_grades_countries, aes(x = rating, 
                                       y = fct_reorder(country_of_bean_origin, 
                                                       total))) +
  geom_tile(aes(fill = percent), color = "gray40") +
  geom_text(data = top_countries, 
            aes(x = 16, y = country_of_bean_origin, label = paste("n = ",n)),
            family = "Gothic",
            size = 6) +
  geom_text(x = 16, y = 16, label = "Total ratings",
            family = "Gothic",
            size = 6) +
  scale_fill_gradient(name = "Percent of ratings per country", 
                      low = "cornsilk", high = "chocolate4",
                      na.value = "gray88") +
  scale_x_discrete(position = "top") +
  coord_fixed(clip = "off") +
  labs(y = NULL, x = "Grades", 
       title = "Percent of ratings in each grade (1 to 4), for each country of bean origin")+
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_markdown(family = "Gothic", color = "black", size = 16),
        axis.title = element_markdown(family = "Gothic", color = "black", face = "bold", size = 18),
        plot.title = element_markdown(family = "Alfa", color = "chocolate4", size = 18),
        legend.title = element_markdown(family = "Gothic", size = 18),
        legend.text = element_text(size = 14),
        aspect.ratio = 5/7,
        legend.position = "bottom") 

final <- p1 +
  plot_annotation(title = "Chocolate bar ratings from flavors of cacao",
                  subtitle = "Flavors of Cacao database has over 2500 plain dark chocolate bars ratings. Each entry also contains informations
about the chocolate ingredients, cocoa <br> percent, most memorable characteristics and about the origin of the bean.
Here, we see the distribution of ratings for the countries of bean origin <br> with more than 50 entries in the database.
Venezuela has the largest number of entries, and only Dominican Replublic didn't have any grade 4. <br>",
                  caption = "Data from Flavors of Cacao by way of Georgios and Kelsey | Amanda Fanelli for #TidyTuesday Wk3",
                  theme = theme(plot.title = element_markdown(family = "Alfa", color = "chocolate4", size = 22, hjust = 0, vjust = 1),
                                plot.subtitle = element_markdown(family = "Gothic", size = 18, hjust = 0, vjust = 1),
                                plot.caption = element_markdown(family = "Gothic", size = 14, hjust = 0, vjust = 1),
                                plot.margin = margin(5,0,5,-80, unit = "pt")))


ggsave("2022/wk3/chocolate.png", width = 6, height = 5)
