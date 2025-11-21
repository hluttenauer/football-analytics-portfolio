# Install if not done yet
install.packages("devtools")
devtools::install_github("statsbomb/StatsBombR")

library(StatsBombR)
library(tidyverse)
library(ggsoccer)

# Download competitions
Comp <- FreeCompetitions()  # list of competitions available

# Pick one (e.g., “FIFA WC 2018”)
Matches <- FreeMatches(Comp%>%filter(competition_id==43))  

# Pick one match
Events <- get.matchFree(Matches%>%filter(match_id==7580))

# How many passes per team?
Events %>%
  filter(type.name == "Pass") %>%
  group_by(team.name) %>%
  summarise(passes = n())

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
  select(team.name,x,y)
ggplot() +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "black") +
  geom_point(data = Shots, aes(x = x, y = y, color = team.name), size = 3) +
  theme_pitch() +
  ggtitle("Shots on goal — Match Example")

ggsave("plots/match1_shots.png", width = 8, height = 5)

