library(tidyverse)
#load data
breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

#long data
breed_rank_long <- breed_rank %>% 
  select(Breed:`2020 Rank`) %>% 
  pivot_longer(cols = -Breed, 
               names_to = "Year",
               names_pattern = "(....) Rank",
               values_to = "Rank") %>% 
  filter(Rank <= 15) %>% 
  mutate(Year = as.numeric(Year))

#data of first entry
first_enty <- breed_rank_long %>% 
  group_by(Breed) %>% 
  summarise(Year = min(Year)) %>% 
  left_join(breed_rank_long, by = c("Breed", "Year")) %>% 
  select(Breed, Year, Rank) %>% 
  mutate(color = case_when(
    Breed == "Great Danes" ~ "blue",
    TRUE ~ "gray"
  ))


#bumpchart
breed_rank_long %>% 
  mutate(color = case_when(
    Breed == "Great Danes" ~ "blue",
    TRUE ~ "gray"
  )) %>% 
  ggplot(aes(x = Year, y = Rank, group = Breed)) +
  geom_line(aes(color = color), size = 2) +
  geom_text(data = first_enty, aes(x = Year + 0.5, y = Rank - 0.5, label = Breed, color = color)) +
  scale_y_reverse() +
  scale_color_manual(values = c("blue", "gray")) +
  theme(legend.position = "none") +
  labs(title = "Dog breed ranks 2013-2020",
       y = "",
       x = "")
