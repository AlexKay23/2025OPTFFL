source("theme.R")
library(tidyverse)
library(fflr)
library(ggimage)

ffl_id(leagueId = "710908445")
tidy_scores <- fflr::tidy_scores()

matchups_with_margin <- tidy_scores %>%
  inner_join(tidy_scores, by = c("matchupId", "matchupPeriodId"), suffix = c("", "_opp")) %>% # Join the dataset to itself to pair each team with its opponent in the same matchup.
  filter(teamId != teamId_opp) %>% # Then filter out self-pairings and calculate the margin between team and opponent scores.
  filter(totalPoints > 0) %>%  #filter out match ups that have not happened yet
  mutate(margin = totalPoints - totalPoints_opp) %>%
  select(seasonId, matchupPeriodId, matchupId, teamId, abbrev, isHome, totalPoints, isWinner, expectedWins, margin) %>% 
  mutate(luck_category = case_when(
    isWinner == TRUE & margin < 10 ~ "Fortunate Win",
    isWinner == FALSE & margin > -10 ~ "Unfortunate Loss",
    isWinner == TRUE & margin >= 10 ~ "Convincing Win",
    isWinner == FALSE & margin <= -10 ~ "Clear Loss",
    TRUE ~ "Uncategorized")) # categorize how close a match up result is


team_luck_summary <- matchups_with_margin %>% 
  group_by(teamId,abbrev) %>% 
  summarize(fortunate_wins = sum(luck_category == "Fortunate Win"),
            unfortunate_losses = sum(luck_category == "Unfortunate Loss"),
            total_matchups = n()) %>%  #total count of unfortunate wins and losses
  mutate(net_luck = fortunate_wins-unfortunate_losses) #lucky in your match up results overall?

team_margin_stats <- matchups_with_margin %>% 
  group_by(teamId,abbrev) %>% 
  summarize(avg_margin = mean(margin), # what is your avg margin of win/loss
            median_margin = median(margin), # medain margin win/los
            sd_margin = sd(margin)) # the spread/consistency
#overall strength and consistency of match up results 

team_win_efficiency <- matchups_with_margin %>% 
  group_by(teamId,abbrev) %>% 
  summarize(actual_wins = sum(isWinner),
            expectedWins = sum(expectedWins),
            win_diff = actual_wins - expectedWins)
#if positive, overperforamnce/lucky
#if negattive, underperormed or unlucky



leauge_avg_points <- matchups_with_margin %>% 
  summarise(avg_points = mean(totalPoints)) %>% 
  pull(avg_points)

team_def_press <- matchups_with_margin %>% 
  group_by(teamId,abbrev) %>% 
  summarize(avg_opp_points = mean(totalPoints-margin),
            press_diff = avg_opp_points - leauge_avg_points)
#Positive value means stronger than avg, neg = less strong

team_luckiness <- team_luck_summary %>% 
  left_join(team_win_efficiency, by=c("teamId","abbrev")) %>% 
  left_join(team_def_press, by=c("teamId","abbrev")) %>% 
  left_join(team_margin_stats, by=c("teamId","abbrev")) %>% 
  mutate(luck_score = net_luck + win_diff - press_diff)



# negative = less lucky, positive = more lucky


logo <- fflr::league_teams(seasonId = 2025) %>% 
  select(2:4)

ggplot(team_luckiness %>% inner_join(.,logo,by="abbrev"), aes(x=fct_reorder(abbrev, luck_score),y=luck_score))+
  geom_col(fill="darkgreen")+
  geom_image(aes(image = logo),size=0.09)+
  labs(y="<- Unlucky        Lucky ->")+
  optffl_theme()+
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(face = "bold",size = 13),
        axis.title.y = element_text(face = "bold", size = 18))