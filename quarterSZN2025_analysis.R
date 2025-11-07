# Packages and Fonts ####
library(tidyverse)
library(purrr)
library(ggimage)
library(showtext)


font_add(family = "FranklinGothic", regular = "framd.ttf")
font_add(family = "ComicSans", regular = "comic.ttf")
font_add(family = "PalatinoLinotype", regular = "pala.ttf")
font_add(family = "Trebuchet", regular = "trebuc.ttf")
showtext_auto()


library(fflr)
library(nflverse)
library(gt)
library(ggimage)
library(ggrepel)

fflr::ffl_id(leagueId = 710908445)


# Load Data ####

scoring_week <- 5

logo <- league_teams(seasonId = 2025) %>% 
  select(2:4)

all_rost <- team_roster(seasonId = 2025,scoringPeriodId = scoring_week)



rosters <- map(all_rost,as_tibble)
roster_data <- map_dfr(all_rost,~.x)


record_data <- fflr::league_standings(seasonId = 2025) %>% 
  group_by(abbrev) %>% 
  top_n(1,scoringPeriodId) %>% 
  mutate(record = paste(wins,losses, sep = "-")) %>% 
  select(4,18)


## Weekly Scoring Load ####
w1_roster_Qszn <- fflr::team_roster(seasonId = 2025,scoringPeriodId = 1) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
  filter(lineupSlot != "BE")
w2_roster_Qszn <- fflr::team_roster(seasonId = 2025,scoringPeriodId = 2) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
  filter(lineupSlot != "BE") 
 w3_roster_Qszn <- fflr::team_roster(seasonId = 2025,scoringPeriodId = 3) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
   filter(lineupSlot != "BE")
w4_roster_Qszn <- fflr::team_roster(seasonId = 2025,scoringPeriodId = 4) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
   filter(lineupSlot != "BE")
w5_roster_Qszn <- fflr::team_roster(seasonId = 2025,scoringPeriodId = 5) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>%
  filter(lineupSlot != "BE")
w6_roster_Qszn <- fflr::team_roster(seasonId = 2025,scoringPeriodId = 6) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>%
  filter(lineupSlot != "BE")
w7_roster_Qszn <- fflr::team_roster(seasonId = 2025,scoringPeriodId = 7) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>%
  filter(lineupSlot != "BE")
w8_roster_Qszn <- fflr::team_roster(seasonId = 2025,scoringPeriodId = 8) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>%
  filter(lineupSlot != "BE")
w9_roster_Qszn <- fflr::team_roster(seasonId = 2025,scoringPeriodId = 9) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>%
  filter(lineupSlot != "BE")
# w10_roster_Qszn <- fflr::team_roster(scoringPeriodId = 10) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE")
# w11_roster_Qszn <- fflr::team_roster(seasonId = 2025,scoringPeriodId = 11) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE")
# w12_roster_Qszn <- fflr::team_roster(seasonId = 2025,scoringPeriodId = 12) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE")
# w13_roster_Qszn <- fflr::team_roster(seasonId = 2025,scoringPeriodId = 13) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE")
# w14_roster_Qszn <- fflr::team_roster(seasonId = 2025,scoringPeriodId = 14) %>% map(.,as_tibble) %>% map_dfr(.,~.x) %>% 
#   filter(lineupSlot != "BE")

## Weekly Scores Join

qzn_data <- list(w1_roster_Qszn,
     w2_roster_Qszn,
     w3_roster_Qszn,
     w4_roster_Qszn,
     w5_roster_Qszn,
     w6_roster_Qszn,
     w7_roster_Qszn,
     w8_roster_Qszn,
     w9_roster_Qszn) %>% 
  reduce(full_join) %>% distinct() %>% inner_join(.,logo)


# Position Points in Flex Spont ####

## Count of position occurances in the FLEX Spot
qzn_data %>% 
  filter(lineupSlot == "FLEX") %>% 
  count(position) %>% 
  view()

## plot out points of FLEX

ggplot(qzn_data %>% filter(lineupSlot == "FLEX") %>% inner_join(., record_data),
       aes(x=position,y=actualScore,color=record))+geom_point()+
  facet_wrap(~record)

ggplot(qzn_data %>% filter(lineupSlot == "FLEX"),
       aes(x=position,y=actualScore))+geom_boxplot()

ggplot(qzn_data %>% filter(lineupSlot == "FLEX") %>% inner_join(., record_data),
       aes(x=position,y=actualScore))+geom_boxplot()+
  facet_wrap(~record)



ggplot(qzn_data %>% filter(lineupSlot == "FLEX") %>% inner_join(., record_data),
       aes(x=position,y=actualScore,color=position))+geom_point()+
  facet_wrap(~abbrev)



# Margin of Victory ####


fflr::league_standings() %>% 
  group_by(abbrev)

margin_victory <- fflr::tidy_scores() %>% 
  group_by(matchupPeriodId, matchupId) %>% 
  mutate(abs_diff = abs(totalPoints - lag(totalPoints))) %>%
  filter(matchupPeriodId <= 2) %>% 
  ungroup() %>% 
  group_by(matchupPeriodId) %>% 
  mutate(avg_margin = mean(abs_diff,na.rm=TRUE)) %>% 
  ungroup()


mean(margin_victory$abs_diff,na.rm = TRUE)


ggplot(margin_victory, aes(x=expectedWins,y=abs_diff,colour = abbrev))+geom_point()+
  facet_wrap(~isWinner)





 

# JJC Award #####
# Most RB Points
# 
# qzn_data %>% 
#   filter(position == "RB") %>% 
#   group_by(playerId) %>% 
#   mutate(rb_points = sum(actualScore,na.rm = TRUE)) %>% 
#   arrange(desc(rb_points))


qzn_data %>%
  filter(position == "RB") %>%
  group_by(playerId, firstName, lastName, abbrev) %>%  # include abbrev here
  summarize(rb_points = sum(actualScore, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(rb_points)) %>%
  ggplot(aes(x = rb_points,
             y = reorder(paste0(firstName, lastName), rb_points),
             fill = abbrev)) +  # now abbrev is available
  geom_col() +
  labs(x = "RB Points", y = "Player Name", title = "RB Points by Player", fill = "Team") +
  theme_minimal()



qzn_data %>% 
  filter(position == "RB") %>% 
  group_by(abbrev) %>% 
  mutate(rb_points = sum(actualScore, na.rm = TRUE)) %>% 
  arrange(desc(rb_points))

qzn_data %>% 
  filter(position =="RB") %>% 
  group_by(abbrev) %>% 
  summarise(rb_points =sum(actualScore, na.rm = TRUE), .groups = "drop") %>% 
  arrange(desc(rb_points)) %>% 
  ggplot(aes(x=reorder(abbrev,rb_points),y=rb_points))+
  geom_col()+
  labs(x = "Team", y = "RB Points", title = "RB Points by Team") +
  theme_minimal()


qzn_data %>%
  filter(position == "WR") %>%
  group_by(abbrev) %>%
  summarize(wr_points = sum(actualScore, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(wr_points)) %>%
  ggplot(aes(x = reorder(abbrev, wr_points), y = wr_points)) +
  geom_col() +
  labs(x = "Team", y = "WR Points", title = "WR Points by Team") +
  theme_minimal()




# Point Distribution ####

library(ggridges)

weekly_scores <- qzn_data %>% 
  filter(lineupSlot != "BE") %>% 
  group_by(scoringPeriodId,abbrev) %>% 
  mutate(weeklyScore = sum(actualScore,na.rm = TRUE)) %>% 
  mutate(division = case_when(
    abbrev == "KAY" ~ "Oreo",
    abbrev == "BTW" ~ "Pringle",
    abbrev == "CW" ~ "Oreo",
    abbrev == "KAS" ~ "Pringle",
    abbrev == "BWb" ~ "Oreo",
    abbrev == "YARN" ~ "Pringle",
    abbrev == "JOHN" ~ "Oreo",
    abbrev == "OPE" ~ "Pringle",
    abbrev == "CNP" ~ "Pringle",
    abbrev == "JFT" ~ "Pringle",
    abbrev == "JOSh" ~ "Pringle",
    abbrev == "TCC" ~ "Oreo",
    abbrev == "LLL" ~ "Oreo",
    abbrev == "JII" ~ "Oreo",
    abbrev == "GPG" ~ "Pringle",
    abbrev == "EET" ~ "Oreo"
    
    
  ))

ggplot(weekly_scores, aes(x=weeklyScore,fill = abbrev))+
  geom_density()

ggplot(weekly_scores, aes(x=weeklyScore,y=abbrev,group = abbrev,fill = abbrev))+
  geom_density_ridges()+
  facet_wrap(~division,scales = "free_y",nrow = 1)

OPTFFL_team_colors <- c("KAY" = "#909090",
                        "BTW" = "#c9a01d",
                        "CW" = "#fec95f",
                        "KAS" = "#842020",
                        "BWb" = "#1b8842",
                        "JOHN" = "#faaf7d",
                        "JFT" = "pink",
                        "OPE" = "#cc5d3f",
                        "CNP" = "#129b51",
                        "EET" = "#bfe3ce",
                        "JOSh" = "#2d2e2c",
                        "TCC" = "orange",
                        "LLL" = "#0077B5",
                        "JII" = "#562c87",
                        "GPG" = "#fff200",
                        "YARN" = "#ed2246"
)

final_score_density <- ggplot(weekly_scores, aes(x = weeklyScore, y = abbrev, group = abbrev, fill = abbrev)) +
  geom_density_ridges() +
  facet_wrap(~division, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = OPTFFL_team_colors)+
  theme_bw()+
  labs(title = "Weekly Scores Density Plot",x="Weekly Score")+
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#fafafa", color = NA),
        plot.background = element_rect(fill = "#fafafa", color = NA),
        strip.background = element_rect(fill = "#fafafa", color = NA),
        strip.text.x.top = element_text(size=18),
        text = element_text(family = "Trebuchet",face = "bold"),
        plot.title = element_text(hjust = 0.5,size = 24),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=15),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

today <- lubridate::today()
ggsave(plot = final_score_density,filename = paste0("plots/",today,"_ScoreDensityPlot.png"))

ggplot(weekly_scores, aes(x=weeklyScore,y=abbrev,fill = abbrev))+
  geom_boxplot(fill="darkgreen")+
  geom_point()+
  facet_wrap(~division,scales = "free_y")

standings_data <- fflr::league_standings(seasonId = 2025) %>% 
  inner_join(.,logo) %>% 
  mutate(record = paste(wins,losses, sep = "-"))

library(ggforce)
points_4_against <-ggplot(standings_data %>% mutate(playoffStatus = case_when(playoffSeed<9~"Playoffs",
                                                           playoffSeed>8~"Non Playoff")),
       aes(x=pointsFor,y=pointsAgainst))+
  geom_point()+
  geom_circle(
    aes(x0 = pointsFor, y0 = pointsAgainst, r = 25, fill = playoffStatus),
    alpha = 0.45,
    color = NA)+
  scale_fill_manual(values = c("Playoffs" = "green", "Non Playoff" = "red"))+
  geom_image(aes(image = logo),
             size = .15)+
  # geom_text(label = standings_data$record,
  #           nudge_y = 18,
  #           nudge_x = 9,
  #           size = 5,fontface = "bold")+
  labs(title = "Points For vs Points Against",
       x = "Points For",
       y = "Points Against",
       fill = "Playoff Status")+
  geom_vline(xintercept = mean(standings_data$pointsFor),color="darkgreen",linewidth=1.5,alpha=0.4)+
  geom_hline(yintercept = mean(standings_data$pointsAgainst),color="darkgreen",linewidth=1.5,alpha=0.4)+
  annotate("text",y = mean(standings_data$pointsAgainst)+5,x=1100,label="avg pts against")+
  annotate("text",x=mean(standings_data$pointsFor)-5,y=950,label="avg pts For",angle=90)+
  annotate("text",x= 750,y=770,label = "Weak Performances",size=5,fontface="bold")+
  annotate("text",x= 1090, y=770, label = "Easy Schedule",size=5,fontface="bold")+
  annotate("text",x= 1090, y= 1000, label= "Battle Tested",size=5,fontface="bold")+
  annotate("text",x= 750,y=1000,label="Tough Schedule",size=5,fontface="bold")+
  theme_bw()+
  coord_cartesian(
    xlim = range(standings_data$pointsFor) + c(-20, 20),
    ylim = range(standings_data$pointsAgainst) + c(-20, 20)
  )+
  theme(text = element_text(family = "Trebuchet",face = "bold"),
        plot.title = element_text(hjust = 0.5,size = 24),
        axis.title.x = element_text(size = 20,family = "Trebuchet",face = "bold"),
        axis.text.x = element_text(size = 15),
        axis.title.y = element_text(size = 20,family = "Trebuchet",face = "bold"),
        axis.text.y = element_text(size = 15),
        panel.background = element_rect(fill = "#fafafa", color = NA),
        plot.background = element_rect(fill = "#fafafa", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill = "#fafafa", color = NA))

today <- lubridate::today()

ggsave(plot = points_4_against,filename = paste0("plots/",today,"_PointsVsAgainst.png"))




