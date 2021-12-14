#Amanda Fanelli for #TidyTuesday 09/28/2021

# Loading packages ---- 
library(tidyverse)
library(gganimate)

# Getting the Data ----

papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')


#Joining dataset ----
nber_df <- papers %>%
  left_join(paper_programs) %>%
  left_join(programs) 


#Counts of paper per program per year ----
program_year <- nber_df %>%
  filter(!is.na(program)) %>%
  group_by(year, program, program_desc, program_category) %>%
  summarize(paper_count = n())



#Counts of paper per program per decade ----
program_decade <- program_year %>% 
  mutate(decade = floor(year/10)*10) %>%
  group_by(decade, program, program_desc, program_category) %>%
  summarize(paper_count = sum(paper_count)) %>%
  filter(decade != 2020 & decade != 1970) %>%
  replace_na(list(program_category = "Technical")) 


#Animated bar plot ---- 
static_plot <- ggplot(program_decade, 
                      aes(x = reorder(program_desc, paper_count),
                          y = paper_count,
                          fill = program_category)) +
  geom_bar(stat = "identity") +
  coord_flip()+
  scale_fill_manual(values = c("lightpink", "lightgreen", 
                               "lightblue", "lightgrey")) +
  theme_classic()+ theme(legend.key.size = unit(20, "pt"),
                         text = element_text (size = 30),
                         plot.title = element_text(hjust = 1, vjust = 1),
                         plot.caption = element_text (hjust = 1, vjust = 1, 
                                                      size =20)) +
  labs(x = element_blank(), y = "Total papers published", 
       caption = "Amanda Fanelli for TidyTuesday week 40. Data from National Bureau of Economic Research (NBER)",
       fill = "Category") 
  

animated_plot <- static_plot + 
  transition_states(decade, transition_length = 3, state_length = 2) +
  labs(title = "NBER papers published by research program per decade:{closest_state}") 

animate(animated_plot, 200, fps = 20,  width = 1500, height = 1000,
        renderer = gifski_renderer("nbcr_gganim.gif"))
