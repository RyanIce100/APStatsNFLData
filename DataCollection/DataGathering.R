#necessary libraries
library("nflreadr")
library("tidyverse")
library("dplyr")

#load the data
draft <- load_draft_picks(seasons = 1987:2020)
ras <- read_csv("rasFootball.csv")

#Select useful draft columns
draft_clean <- draft %>%
  select(
    season, round, pick, team, pfr_player_name, hof, position, college, age, to,
    allpro, probowls, seasons_started, w_av, games)

draft_clean <- draft_clean %>%
  mutate(seasons_played = to - season) # added seasons_played as seasons_started works weirdly
#recode position names for filtering
draft_clean <- draft_clean %>%
  mutate(position = recode(position,
                           "T" = "OT",
                           "C" = "OC", 
                           "G" = "OG",
                           "NT" = "DT",
                           "K" = "PK",
                           "OL" = "OG",
                           "DL" = "DT",
                           "OLB" = "LB",
                           "ILB" = "LB",
                           "KR" = "WR"))


ras_clean <- ras %>%
  rename(pfr_player_name = 2, RAS = 6, position = 3, college = 4)

ras_clean <- ras_clean %>%
  mutate(position = recode(position,
                           "SS" = "S",
                           "FS" = "S"))

#join ras and draft data
final <- draft_clean %>%
  left_join(ras_clean, by = c("pfr_player_name", "position")) %>%
  # Remove players with RAS or round as NA
  filter(!(is.na(RAS) | is.na(round))) %>%
  select(season, round, pick, team, pfr_player_name, position, age,
         hof, allpro, probowls, seasons_started, seasons_played, w_av, games,
         RAS)

final[215, "RAS"] <- 9.74 # keith jones had a duplicate (other player with same name) that was handled incorrectly

# replace na values for w_av with 0
final <- final %>%
  mutate(w_av = replace_na(w_av, 0))

final <- final %>%
  mutate(seasons_played = seasons_played + 1)

final <- final %>%
  mutate(w_av_py = w_av / seasons_played)

#Filter by seasons played
final_seasons <- final |>
  mutate(seasons_played = as.numeric(seasons_played)) |>
  filter(seasons_played > 3)

boundaries_seasons <- fivenum(final_seasons$RAS)
final_seasons <- final_seasons |>
  mutate(stratum = cut(RAS, 
                       breaks = boundaries_seasons,
                       labels = c("Stratum 1",
                                  "Stratum 2",
                                  "Stratum 3",
                                  "Stratum 4"),
                       include.lowest = TRUE))
strat_sample_seasons <- final_seasons %>%
  group_by(stratum) %>%
  sample_n(size = 75, replace = FALSE) %>%
  ungroup()

#Write csv for readability
write.csv(strat_sample_seasons, file = "RAS_sample_data.csv", row.names = FALSE)
