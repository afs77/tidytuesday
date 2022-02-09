
# Loading dataset ----
airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

# Loading packages----
library(tidyverse)
library(gt)


#Exploring dataset----

glimpse(airmen)

View(airmen %>%
  filter(!is.na(reported_lost)))

airmen%>%
  count(state)

airmen%>%
  count(pilot_type)

#Data wrangling----

#Adjust names in the tibble, remove NA in the state column
#Group by state and count for each state the total of pilots, 
#total per pilot type, and the sum of credits for each state
state_airmen <- airmen %>%
  filter(!is.na(state)) %>%
  filter(state!="Unk"& state!="Haiti") %>%
  mutate(state = str_replace(state, "In", "IN")) %>%
  group_by(state) %>%
  summarize(total_pilots = n(),
            single_engine = sum(pilot_type == "Single engine"),
            twin_engine = sum(pilot_type == "Twin engine"),
            liaison_pilot = sum(pilot_type == "Liaison pilot"),
            service_pilot = sum(pilot_type == "Service pilot"),
            aerial_credits = sum(number_of_aerial_victory_credits)) %>%
  filter(total_pilots > 20)

#Add the name of the states corresponding to each abreviation
states <- tibble(name = c(state.name, "District of Columbia"), 
                 state = c(state.abb, "DC")) #state.name and state.abb are R built-in constants

state_names_airmen <- left_join(state_airmen, states, by = "state") %>%
  relocate(name, .after = state) 
  
  
  
  
  

#Table ----

gt_table <- state_names_airmen %>%
  gt(rowname_col = "state") %>%
  tab_header(title = md("**Tuskegee airmen**: The first african-american 
                        military aviators in the US. armed forces"),
             subtitle = md("For each state with more than 20 aviators, here's the **number of pilots** in 
                           each category and the total of **aerial victory credits**.")) %>%
  opt_align_table_header(align = "left") %>%
  cols_align(align = "center",
             columns = everything()) %>%
  cols_label(name = "Name",
             total_pilots = "Total of pilots",
             single_engine = "Single engine",
             twin_engine = "Twin engine",
             liaison_pilot = "Liaison pilot",
             service_pilot = "Service pilot",
             aerial_credits = "Aerial victory credits") %>%
  fmt_number(total_pilots:aerial_credits, decimals = 0) %>%
  cols_width(name ~ px(160),
             total_pilots ~ px(160),
             single_engine ~ px(160),
             twin_engine ~ px(160),
             liaison_pilot ~ px(160),
             service_pilot ~ px(160),
             aerial_credits ~ px(160),
             everything() ~ px(80)) %>%
  tab_spanner(single_engine:service_pilot, label = "Count per type of pilot") %>%
  tab_stubhead(label = "State") %>%
  tab_source_note(source_note = md("Data from Commemorative AirForce (CAF) by way of the VA-TUG<br>
                                   Visualization by Amanda Fanelli for #Tidytuesday")) %>%
  opt_table_font(font = google_font("Questrial")) %>%
  data_color(
    columns = c(total_pilots),
    colors = scales::col_numeric(
      palette = c("#AED7C7", "#17AB72", "#067047"),
      domain = c(0, 110))) %>%
  data_color(
    columns = c(aerial_credits),
    colors = scales::col_numeric(
      palette = c("#B0D7FC", "#1F8EF7", "#013D76"),
      domain = c(0, 15))) %>%
  tab_options(heading.title.font.size = 28,
              heading.subtitle.font.size = 24,
              column_labels.font.size = 24,
              stub.font.size = 24,
              table.font.size = 24)
  
gt_table

gtsave(gt_table, "2022/wk6/airmen_table.png", vwidth = 2000, vheight = 700)





