#Loading packages ----
library(tidyverse)
library(lubridate)
library(showtext)
library(ggtext)

#Loading tidytyesday tuskgee airmen dataset ----
airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

#Cleaning and preparing data ----

pilot_year <- airmen %>%
  mutate(year = year(graduation_date)) %>%
  group_by(year) %>%
  summarize(single_engine = sum(pilot_type == "Single engine"),
            twin_engine = sum(pilot_type == "Twin engine"),
            liaison_pilot = sum(pilot_type == "Liaison pilot"),
            service_pilot = sum(pilot_type == "Service pilot")) %>%
  filter(!is.na(year)) %>%
  mutate(total = single_engine + twin_engine + liaison_pilot + service_pilot,
         percent_twin = round(100*twin_engine/total,0),
         percent_others = 100 - percent_twin,
         #turn zeros into NA, so that I can position zeros in another layer in ggplot2
         percent_twin = case_when(percent_twin !=0 ~ percent_twin)) 
  


#Plot----

#Add fonts from google fonts
font_add_google("Space mono", family = "spacemono")
font_add_google("JetBrains Mono", family = "jetbrains")
font_add_google("B612 Mono", family = "b612")

showtext_auto()

#Area chart

ggplot(pilot_year, aes(x = year)) +
  #green layer
  geom_area(aes(y = 100), fill = "#008746", alpha = 0.8) + 
  #black layer
  geom_area(aes(y = percent_others), fill = "#000000") +
  #show percent twin engine (except zeros, that are NA and will be removed)
  geom_text(aes(y = percent_others, label = str_c(percent_twin, "%")), 
            vjust = -1.8, family = "b612", size = 10) + 
  #show 0% twin engine in 1942 
  geom_text(x = 1942, y = 100, label = "0%", family = "b612", size = 10, hjust = 1.2) +
  #show 0% twin engine in 1942 
  geom_text(x = 1948, y = 100, label = "0%", family = "b612", size = 10, hjust = -0.5) +
  #print twin engine in green area
  geom_text(aes(x = 1945, y = 95, label = "TWIN ENGINE - BIMOTOR"),
            color = "black", family = "jetbrains", size = 15, fontface = "bold") +
  
  #print others in black area
  geom_text(aes(x = 1945, y = 40, label = "OTHERS"), 
            color = "#FAF7D5", family = "jetbrains", size = 16, fontface = "bold") +
  geom_text(aes(x = 1945, y = 37, label = "OUTROS"),
            color = "#FAF7D5", family = "jetbrains", size = 16, fontface = "bold") +
  scale_x_continuous(limits = c(1942,1948), 
                     breaks = c(1942, 1943, 1944, 1945, 1946, 1948),
                     position = "top",
                     expand = c(0,0.4)) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  coord_cartesian(clip = "off") +
  labs(y = NULL, x = NULL,
       title = "**PROPORTION OF TWIN ENGINE TUSKGEE AIRMEN PILOTS BY GRADUATION YEAR .**<br>
       **PROPORÇÃO DE PILOTOS TUSKGEE DE AVIÃO BIMOTOR POR ANO DE GRADUAÇÃO** .",
       subtitle = "<br>DATA FROM COMMEMORATIVE AIRFORCE (CAF) .<br><br>") +
  theme_minimal() +
  theme(panel.grid.major.x = element_line(color = "black"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#FAF7D5", color = NA),
        panel.background = element_rect(fill = "#FAF7D5", color = NA),
        plot.title = element_markdown(color = "black", family = "jetbrains", 
                                      hjust = 0.3, vjust = 0, size = 34),
        plot.subtitle = element_markdown(color = "black", family = "jetbrains", 
                                         hjust = 0.5, vjust = 0.5, size = 30),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(color = "black", family = "jetbrains", face = "bold", 
                                   size = 34),
        plot.margin = margin(0.2,0.3,0,0.3, unit = "in"))

ggsave("2022/wk7/dubois_tuskgee.png", width = 9, height = 8, units = "in")
