library(tidyverse)
library(dplyr)

# import CSV file into tibble 
# This game file is pretty much useless to be honest
game = read_csv("Big Data Bowl/games.csv")
# Not sure what to use this for honestly
scouting = read_csv("Big Data Bowl/pffScoutingData.csv")
# This will help when looking up the ID's of certain players
players = read_csv("Big Data Bowl/players.csv")
players = players[!duplicated(players),]
# This seems like the most useful file
plays = read_csv("Big Data Bowl/plays.csv")
df_readRPlays = read_csv("Big Data Bowl/pbp_participation_2022.csv", col_types=cols_only(old_game_id='i', play_id ='i', xpass='d'))
# Tracking data
week1 = read_csv("Big Data Bowl/week1.csv")

defensive_end = filter(scouting, scouting$pff_positionLinedUp=="LE"|scouting$pff_positionLinedUp == "RE")
# Calais Campbell tibble
# Gets Las Vegas vs Baltimore game
CampbellGame = game[game$gameId == 2021091300,]
# Gets Campbell's player ID
campbell = scouting[scouting$nflId == 33131,]
# Setting Campbell's regular season statistics to just from the Las Vegas and Baltimore game
campbell = campbell[campbell$gameId == 2021091300,]
# Get's Campbell's total number of hurries from that game
hurries = sum(campbell$pff_hurry)

# Gets Las Vegas vs Baltimore games 
lvbalt = plays[plays$gameId == 2021091300,]
lvbalt1 = week1[week1$gameId == 2021091300,]
lvbalt2 = scouting[scouting$gameId == 2021091300,]
# Sorting by sacks
lvbalt2 = lvbalt2[lvbalt2$pff_sack == 1,]
# Getting rid of na variables
lvbalt2 = lvbalt2 %>%
  filter_at(vars(pff_sack), all_vars(!is.na(.)))
# Getting the play ID of the first sack and applying it to other stuff
lvbalt = lvbalt[lvbalt$playId == 518,]
lvbalt1 = lvbalt1[lvbalt1$playId == 518,]
# Getting the full play description - Maxx Crosby got the sack 
lvbalt$playDescription
# Indexing the players by Maxx Crosby
crosby = players[players$displayName == "Maxx Crosby",]
# Using the NFL ID to get Crosby's tracking data on the play where he got the sack
lvbalt1 = lvbalt1[lvbalt1$nflId == 47889,]
lvbalt1 = lvbalt1 %>%
  filter_at(vars(x), all_vars(!is.na(.)))

# Need to chart out the NFL field and see what Crosby did during this play

# MAX SPEED
SECONDSTOINCLUDE <- 1
YARDSPERSECTOMILESPERHOUR <- 2.04545
df_playsplayersToInclude = filter(scouting, scouting$gameId == 2021091300 & 
                                    scouting$playId == 518 & scouting$nflId == 47889)

df_trackingEdges <- week1  %>%
  
  mutate(time = gsub("T", " ", time)) %>%
  
  #inner joining to tracking to filter for these players
  inner_join(df_playsplayersToInclude)  %>%
  
  #filtering for 1 second after snap only
  group_by(gameId, playId, nflId) %>%
  
  filter(cumsum(event %in% c("ball_snap", "autoevent_ballsnap")) > 0) %>%
  
  filter(as.numeric(difftime(time, min(time), units = "secs")) <=
           SECONDSTOINCLUDE) %>%
  
  ungroup() %>%
  
  
  #calculating max speed on play (or "get off")
  group_by(gameId, playId, nflId) %>%
  
  summarise(maxSpeed = YARDSPERSECTOMILESPERHOUR * max(s, na.rm = T),
            .groups = 'keep') %>%
  
  ungroup() %>%
  
  inner_join(plays, by = c("gameId", "playId")) %>%
  
  left_join(df_readRPlays, by = c("gameId" = "old_game_id",
                                  "playId" = "play_id")) %>%
  
  #removing PATs
  filter(down != 0)


players = players %>%
  filter_at(vars(nflId), all_vars(!is.na(.)))
week1 = week1 %>%
  filter_at(vars(nflId), all_vars(!is.na(.)))
# working on adding player name to other categories
week1 = week1[week1$gameId == 2021091201,]
week1 = week1[week1$playId == 63,]
week1$playerName = NA
week1 = as.data.frame(week1)
k = 1
l = 1
# loops that add the name
while (k < length(week1$nflId)) {
  while (l < length(players$nflId)) {
    if (week1$nflId[k] == players$nflId[l]) {
      week1$playerName[k] = players$displayName[l]
      break;
    }
    else {
      l = l + 1
    }
  }
  k = k + 1
}      

      
