# Install if not done yet
install.packages("devtools")
devtools::install_github("statsbomb/StatsBombR")

library(StatsBombR)
library(tidyverse)
library(ggsoccer)
library(purrr)

#####  Match selection
# Download competitions
Comp <- FreeCompetitions()  # list of competitions available

# Pick one (e.g., “FIFA WC 2018”)
Matches <- FreeMatches(Comp%>%filter(competition_id==43))  

# Pick one match
Events <- get.matchFree(Matches%>%filter(match_id==7580))

#####  Data exploration
# How many passes per team?
Events %>%
  filter(type.name == "Pass") %>%
  group_by(team.name) %>%
  summarise(passes = n())

#####  Shotmap plotting
# Count shots
Events %>%
  filter(type.name == "Shot") %>%
  group_by(player.name) %>%
  summarise(shots = n()) %>%
  arrange(desc(shots))

Shots <- Events %>% filter(type.name == "Shot")%>%
  mutate(
    x = map_dbl(location, 1),  # first element = x
    y = map_dbl(location, 2)   # second element = y
  )%>%
  mutate(x=case_when(
    period==2 ~ 120-x,
    TRUE ~ x
  ))%>%
  select(team.name,x,y)
ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey",fill="darkgreen") +
  geom_point(data = Shots, aes(x = x, y = y, color = team.name), size = 3) +
  theme_pitch() +
  ggtitle("Shots on goal — France Argentina World Cup 2018")

ggsave("plots/match1_shots.png", width = 8, height = 5)

Events_clean <- Events %>%
  filter(location != "NULL") %>%
  mutate(
    x = map_dbl(location, 1),  # first element = x
    y = map_dbl(location, 2)   # second element = y
  )

write_csv(Events_clean, "../data/match1_events_clean.csv")

Events_clean %>%
  group_by(type.name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

Passes <- Events_clean %>% filter(type.name == "Pass")

ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey",fill="darkgreen") +
  geom_point(data = Passes, aes(x = x, y = y, color = team.name), size = 2, alpha = 0.6) +
  theme_pitch() +
  ggtitle("Pass locations — France Argentina World Cup 2018")

france_passes <- Passes %>% filter(team.name == "France")

player_locations <- france_passes %>%
  group_by(player.name) %>%
  summarise(
    x = round(mean(x),2),
    y = round(mean(y),2),
    n_passes = n()
  )

ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey",fill="darkgreen") +
  geom_point(data = player_locations, aes(x = x, y = y), size = 4) +
  theme_pitch() +
  ggtitle("Average player locations when passing— France Argentina World Cup 2018")

pass_connections <- france_passes %>%
  filter(pass.end_location != "NULL") %>%
  mutate(
    end_x = map_dbl(pass.end_location, 1),
    end_y = map_dbl(pass.end_location, 2)
  ) %>%
  group_by(player.name,
           pass.recipient.name) %>%
  summarise(
    count = n(),
    mean_x = mean(x),
    mean_y = mean(y),
    mean_end_x = mean(end_x),
    mean_end_y = mean(end_y),
    .groups = "drop"
  ) %>%
  filter(count >= 3)  # show only strong connections

ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey",fill="darkgreen") +  
  geom_point(
    data = player_locations,
    aes(x = x, y = 80 - y, size = n_passes),
    color = "blue"
  ) +
  geom_text(
    data = player_locations,
    aes(x = x, y =80 - y, label = player.name),
    nudge_y = 2,
    size = 4
  ) +
  theme_pitch() +
  scale_size(range = c(1, 10)) +
  ggtitle("Passing Network — France")

