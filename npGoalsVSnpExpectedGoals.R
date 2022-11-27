# Load libraries
library(tidyverse)

# Read csv containing goals and xG figures for each team and save it into a DF
shoot_data_team <- read_csv(file = "npVSnpxG Premier League - Sheet1.csv")
shoot_data_team <- data.frame(shoot_data_team)

View(shoot_data_team)

# Create the npG field defined as Goals minus Penalties
shoot_data_team$npG <- (shoot_data_team$Gls - shoot_data_team$PK)

# Scatter Plot visualisation
ggplot(shoot_data_team, aes(x=npxG., y=npG)) +
  geom_point() +
  geom_text(aes(label=Squad),hjust=-0.1, vjust=-0.1, check_overlap = TRUE) +
  xlab("npxG") +
  ylab("npG") +
  labs(title = "np Goals vs np Expected Goals (per 90 m)",
       subtitle = "English Premier League 2022/2023 GW1-GW7",
       caption = ("Data: FBref
                  By: Naveen Elliott"))
