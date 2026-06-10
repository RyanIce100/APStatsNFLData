# select players round 1-3 | Day1/2 of the draft
final_DayOne <- final %>%
  mutate(round = as.numeric(round)) %>%
  filter(round < 4)

#Stratified random sample 
boundaries_DayOne <- fivenum(final_DayOne$RAS)
final_DayOne <- final_DayOne %>%
  mutate(stratum = cut(RAS, 
                       breaks = boundaries_DayOne, 
                       labels = c("Stratum 1", "Stratum 2", "Stratum 3", "Stratum 4"),
                       include.lowest = TRUE))
stratified_sample_One <- final_DayOne %>%
  group_by(stratum) %>%
  sample_n(size = 35, replace = FALSE) %>%
  ungroup()



## redo above but for players round 4-7 | Day 3 of draft

final_DayThree <- final %>%
  mutate(round = as.numeric(round)) %>%
  filter(round > 3)
final_DayThree <- final_DayThree %>%
  mutate(w_av_py = replace_na(w_av_py,0)) # necessary as players w/o na w_av_py did not play a season

#Stratified random sample 
boundaries_Three <- fivenum(final_DayThree$RAS)
final_DayThree <- final_DayThree %>%
  mutate(stratum = cut(RAS, 
                       breaks = boundaries_Three, 
                       labels = c("Stratum 1", "Stratum 2", "Stratum 3", "Stratum 4"),
                       include.lowest = TRUE))
stratified_sample_Three <- final_DayThree %>%
  group_by(stratum) %>%
  sample_n(size = 55, replace = FALSE) %>%
  ungroup()
