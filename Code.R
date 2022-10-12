# Clean Data
sd <- shots %>%
  select (everything()) %>%
  rename (Team = team, X = x, Y = y, 
          Outcome = fgmade) %>%
  mutate (Outcome = recode_factor(factor(Outcome), 
                                  "0" = "Miss",
                                  "1" = "Make")) %>%
# Determine Shot Zones
  mutate (Zone = ifelse( X > 8 & Y <= 7.8 | X <= -8 & Y <= 7.8, "C3",
                         ifelse (X <= 8 & X >= -8 & Y <= 15, "2PT", "NC3")))

# Shot Distribution & eFG%
A <- shots %>%
  filter(team == 'Team A') %>%
  rename (Team = team, X = x, Y = y) %>%
  mutate (Zone = ifelse( X > 8 & Y <= 7.8 | X <= -8 & Y <= 7.8, "C3",
                         ifelse (X <= 8 & X >= -8 & Y <= 15, "2PT", "NC3"))) %>%
  group_by(Team, Zone) %>%
  summarise ('Shot Distribution' = percent(length(fgmade)/280, accuracy = 0.001),
             'eFG%' = (percent((sum(fgmade)+29)/length(fgmade), accuracy = 0.001)))
#
B <- shots %>%
  filter(team == 'Team B') %>%
  rename (Team = team, X = x, Y = y) %>%
  mutate (Zone = ifelse( X > 8 & Y <= 7.8 | X <= -8 & Y <= 7.8, "C3",
                         ifelse (X <= 8 & X >= -8 & Y <= 15, "2PT", "NC3"))) %>%
  group_by(Team, Zone) %>%
  summarise ('Shot Distribution' = percent(length(fgmade)/224, accuracy = 0.001),
             'eFG%' = (percent((sum(fgmade)+20.5)/length(fgmade), accuracy = 0.001)))

# 3PM
'3PM' <- shots %>%
  rename (Team = team, X = x, Y = y) %>%
  mutate (Zone = ifelse( X > 8 & Y <= 7.8 | X <= -8 & Y <= 7.8, "C3",
                         ifelse (X <= 8 & X >= -8 & Y <= 15, "2PT", "NC3"))) %>%
  filter(Zone == 'NC3' | Zone == 'C3') %>%
  group_by(Team)  %>%
  summarise ('3PM' = sum(fgmade), sum(fgmade)*0.5)

