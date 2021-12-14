#Amanda Fanelli for #TidyTuesday 10/12/2021

# Loading packages ---- 
library(tidyverse)
library(ggrepel)
library(grid)
library(gridExtra)


# Getting the Data ----

farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/aquaculture-farmed-fish-production.csv')
captured_vs_farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fisheries-vs-aquaculture.csv')
captured <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fishery-production.csv')
consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-and-seafood-consumption-per-capita.csv')
stock <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-stocks-within-sustainable-levels.csv')
fishery <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/global-fishery-catch-by-sector.csv')
production <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv')


#Data wrangling ----
#Tidying capture_vs_farmed tibble 
tidy_capture_farmed <- captured_vs_farmed %>%
  pivot_longer(cols = contains("production"),
               names_to = "Type",
               values_to = "Production")


#Countries rank of total fish production in 2017
top_producers <- tidy_capture_farmed %>%
  drop_na(Code) %>%
  filter(Year == "2017") %>%
  group_by(Entity) %>%
  summarize(Total = sum(Production)) %>%
  arrange(desc(Total)) %>%
  drop_na(Total) %>%
  filter(Entity != "World")  %>%
  mutate(Entity = fct_reorder(Entity, Total, .desc = TRUE),
         Total = Total/10^6)

#Countries rank of fish consumption per capta in 2017
consumption <- consumption %>%
  rename( "Quantity"=`Fish, Seafood- Food supply quantity (kg/capita/yr) (FAO, 2020)`)
top_consumers <- consumption %>%
  drop_na(Code) %>%
  filter(Year == "2017") %>%
  arrange(desc(Quantity)) %>%
  mutate(Entity = fct_reorder(Entity, Quantity, .desc = TRUE))


#Fish production in Brazil yearly
brazil_production <- tidy_capture_farmed %>%
  filter(Entity == "Brazil") %>%
  group_by(Entity, Year) %>%
  summarize(Total = sum(Production)) %>%
  mutate(Total = Total/10^6) 

#Fish consumption in Brazil yearly
brazil_consumption <- consumption %>%
  filter(Entity == "Brazil")

#Captured vs farmed in Brazil and in the world
brazil_capture_farmed <- tidy_capture_farmed %>%
  filter(Entity == "Brazil" | Entity == "World") %>%
  mutate(Production = Production/10^6)


#Plots ----

#Theme
my_theme <- function(){
  theme_classic() + theme(plot.title = element_text(size = 16),
                        plot.subtitle = element_text(size = 14),
                        axis.title.y = element_text(size = 14),
                        axis.text.y = element_text (size = 14, color = "black"),
                        axis.title.x = element_text(size =14),
                        axis.text.x = element_text (size = 12, color = "black"))
}

#Producers ranking ----

#Set of other countries to highlight, to compare to Brazil
other <- c("China", "Indonesia", "India",  "United States", 
           "Japan", "Portugal", "South Africa")

other_producers <- top_producers %>%
  filter(Entity %in% other) 

#Set only with Brazil to highlight
brazil_rank_prod <- filter(top_producers, Entity == "Brazil")

p1 <- ggplot(top_producers, aes(x = Entity, y = Total, ymin = 0, ymax = Total)) +
  geom_point(color = "grey")+
  geom_linerange(color = "grey") +
  geom_point(data = brazil_rank_prod, color = "Darkblue") +
  geom_linerange(data = brazil_rank_prod, color = "Blue") +
  geom_text(data = brazil_rank_prod, aes(label = Entity), vjust = -0.5,  
            color = "Darkblue", size = 5) +
  geom_point(data = other_producers, color = "black") +
  geom_linerange(data = other_producers, color = "black") +
  geom_text_repel(data = other_producers, aes(label = Entity), size = 5, 
                  box.padding = 0.15) +
  labs( y = "Production in million metric tons",
        title = "Brazil occupied the 20th position in the fish and other sea food 
production worldwide ranking in 2017",
        subtitle = "China was by far the largest producer, followed by Indonesia and India") +
 my_theme() + theme(axis.title.x=element_blank(),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank())

p1

#Consumers ranking ----

other_consumers <- top_consumers %>%
  filter(Entity %in% other|Quantity > 57) 

brazil_rank <- filter(top_consumers, Entity == "Brazil")

p2 <- ggplot(top_consumers, aes(x = Entity, y = Quantity, ymin = 0, ymax = Quantity)) +
  geom_point(color = "grey")+
  geom_linerange(color = "grey") +
  geom_point(data = brazil_rank, color = "Darkblue") +
  geom_linerange(data = brazil_rank, color = "Blue") +
  geom_text(data = brazil_rank, aes(label = Entity), vjust = -0.3, color = "Darkblue", 
            size = 5) +
  geom_point(data = other_consumers, color = "black") +
  geom_linerange(data = other_consumers, color = "black") +
  geom_text_repel(data = other_consumers, aes(label = Entity), size = 5, 
                  box.padding = 0.1) +
  labs(y = "Sea food consumption (kg/capta)",
       title = "Brazil occupied only the 116th position in the sea food consumption 
worldwide ranking in 2017",
       subtitle = "Top consumers were Iceland and Maldives")+
  my_theme() + theme(axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank())

p2

#Sea food production in Brazil over years ----

labels <- brazil_production %>%
  filter(Year == 1999 | Year == 2012)

p3 <- ggplot(brazil_production, aes(x = Year, y = Total)) +
  geom_line(color = "Cornflowerblue", size = 1) +
  geom_point(color = "Darkblue") +
  scale_x_continuous(breaks = seq(1960,2018, by = 4)) +
  scale_y_continuous(breaks = seq(0,1.4, by = 0.2), limits = c(0,1.4)) +
  geom_label(data = labels, aes(label = Year), hjust = 1, vjust = -0.25, size = 5) +
  geom_vline(xintercept = c(1999,2012), linetype= "dashed") +
  labs(y = "Production in million metric tons",
       title = "Fish and other sea food production has increased significantly 
in Brazil, specially from 1999 to 2012") +
  my_theme() 

p3

#Sea food consumption in Brazil over years ----

labels_2 <- brazil_consumption %>%
  filter(Year == 2005 | Year == 2012 | Year == 2015)

p4 <- ggplot(brazil_consumption, aes( x = Year, y = Quantity)) +
  geom_line(color = "Cornflowerblue", size = 1) +
  geom_point(color = "Darkblue") +
  scale_x_continuous(breaks = seq(1960,2017, by = 5)) +
  ylim(0,11.5) +
  geom_vline(xintercept = c(2005, 2012, 2015), linetype = "dashed") +
  geom_label(data = labels_2, aes(label = Year), hjust = 1, vjust = -0.25, size = 5) +
  labs(y = "Sea food consumption (kg/capta)",
       title = "Sea food consumption per capta increased in Brazil,specially 
from 2005 to 2012, and then started to decrease",
       subtitle = "From 2015 to 2017, consumption was similar.") +
  my_theme() 

p4

#Aquaculture vs capture ----

a_labels <- tibble(Entity = c("Brazil", "World"),
                   label = c("Capture", "Capture"),
                   x = c(2017, 2017),
                   y = c(0.85, 89))

c_labels <- tibble(Entity = c("Brazil", "World"),
                   label = c("Aquaculture", "Aquaculture"),
                   x = c(2017, 2017),
                   y = c(0.4, 110))

p5 <- ggplot(brazil_capture_farmed, aes(x = Year, y = Production, color = Type)) +
  geom_point() +
  geom_line(size = 1)+
  scale_color_manual(values = c("mediumseagreen","mediumvioletred"))+
  scale_x_continuous(breaks = seq(1960,2020, by = 5)) +
  facet_wrap(~Entity,ncol=1, scales = "free_y") +
  geom_text(data = a_labels, aes(label = label, x = x, y = y), 
            color = "mediumvioletred", inherit.aes = FALSE, size = 5) +
  geom_text(data = c_labels, aes(label = label, x = x, y = y), 
            color = "mediumseagreen", inherit.aes = FALSE, size = 5) +
  geom_vline(xintercept = c(2000,2013), linetype = "dashed") +
  labs(y = "Production in million metric tons",
       title = "Aquaculture production increased in Brazil, specially since the 2000's, but capture fisheries production is still larger then cultivated",
       subtitle = "For the total world production, aquaculture production has surpassed capture fisheries in 2013") +
  my_theme() + theme(legend.position = "none",
                    strip.text = element_text(size = 16))

p5

#Combining plots ----

title <- textGrob("How is sea food production and consumption in Brazil compared to other countries?", 
                  gp = gpar(fontsize = 20, fontface = 'bold'))
caption <-textGrob("Data from ourworldindata.org | Amanda Fanelli for TidyTuesday Wk 42", 
                    gp = gpar(fontsize = 12, fontface = 'bold'),
                   just = c(-0.5,0))

png(filename="tidytuesday/wk42_fish/fish_brazil.png", width = 40, height = 48, units = "cm", res = 300)
grid.arrange(title,
             arrangeGrob(p1,p2, ncol = 2),
             arrangeGrob(p3,p4, ncol = 2),
             p5, 
             caption,
             nrow = 5,
             heights = c(3,15,15,15, 1))
dev.off()
