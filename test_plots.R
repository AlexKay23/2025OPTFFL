# Stacked bar chart
# green = points for 
# red = points against
# order = playoff seed

library(tidyverse)
library(fflr)

fflr::ffl_id(leagueId = 710908445)
fflr::tidy_scores(seasonId = 2025) %>% 
  view()
fflr::draft_recap(seasonId = 2025) %>% 
  group_by(abbrev) %>% 
  summarise(sum = sum(autoDraftTypeId))

fflr::league_standings(seasonId = 2025) %>% 
  view()

league_standings(seasonId = 2025) %>% 
  select(c(abbrev,playoffSeed,pointsAgainst,pointsFor)) %>% 
  pivot_longer(cols = starts_with("points"),
               names_to = "cat",
               values_to = "score") %>% 
  ggplot(.,aes(fill = cat,x=fct_reorder(abbrev,playoffSeed),y=score))+
  geom_bar(position = "fill",stat = "identity")

league_standings(seasonId = 2025) %>% 
  select(c(abbrev,playoffSeed,pointsAgainst,pointsFor)) %>% 
  pivot_longer(cols = starts_with("points"),
               names_to = "cat",
               values_to = "score") %>% 
  ggplot(.,aes(fill = cat,x=fct_reorder(abbrev,playoffSeed),y=score))+
  geom_bar(position = "stack",stat = "identity")

league_standings(seasonId = 2025) %>% 
  select(c(abbrev,playoffSeed,pointsAgainst,pointsFor)) %>% 
  pivot_longer(cols = starts_with("points"),
               names_to = "cat",
               values_to = "score") %>% 
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
    abbrev == "EET" ~ "Oreo")) %>% 
  ggplot(.,aes(fill = cat,x=fct_reorder(abbrev,playoffSeed),y=score))+
  geom_bar(position = "dodge",stat = "identity")+
  facet_wrap(~division,ncol = 1,scales = "free_x")



league_standings(seasonId = 2025) %>% 
  select(c(abbrev,playoffSeed,pointsAgainst,pointsFor)) %>% 
  pivot_longer(cols = starts_with("points"),
               names_to = "cat",
               values_to = "score") %>% 
  mutate(playoffStatus = case_when(playoffSeed<9~"Playoffs",
                                   playoffSeed>8~"Toliet Bowl")) %>% 
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
    abbrev == "EET" ~ "Oreo")) %>% 
  ggplot(.,aes(fill = cat,x=fct_reorder(abbrev,playoffSeed),y=score))+
  geom_bar(position = "dodge",stat = "identity")+
  scale_fill_manual(values = c("pointsAgainst"="darkred","pointsFor" = "darkgreen"))+
  facet_wrap(~playoffStatus,ncol = 1,scales = "free_x")+
  labs(x="Team Abbreviation",y="Points")+
  theme(text = element_text(family = "Trebuchet",face = "bold"),
        strip.background = element_rect(fill = "#fafafa"),
        strip.text.x = element_text(size = 18),
        panel.background = element_rect(fill = "#fafafa"),
        plot.background = element_rect(fill = "#fafafa"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill = "#fafafa"),
        legend.title = element_blank())


scatterplot_data_chart_data <- league_standings(seasonId = 2025) %>% 
  select(c(abbrev,playoffSeed,pointsAgainst,pointsFor)) %>% 
  mutate(playoffStatus = case_when(playoffSeed<9~"PlayoffTeam",
                                   playoffSeed>8~"NotPlayoffTeam"))
  
ggplot(scatterplot_data_chart_data, aes(x = pointsFor, y = pointsAgainst, color = playoffStatus)) +
  geom_text(aes(label = abbrev),size = 6) +
  geom_vline(xintercept = mean(scatterplot_data_chart_data$pointsFor), color = "darkgreen", linewidth = 1.5, alpha = 0.4) +
  geom_hline(yintercept = mean(scatterplot_data_chart_data$pointsAgainst), color = "darkgreen", linewidth = 1.5, alpha = 0.4) +
  labs(x = "Points For", y = "Points Against") +
  theme(
    text = element_text(family = "Trebuchet", face = "bold"),
    strip.background = element_rect(fill = "#fafafa"),
    strip.text.x = element_text(size = 18),
    panel.background = element_rect(fill = "#fafafa"),
    plot.background = element_rect(fill = "#fafafa"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#fafafa"),
    legend.title = element_blank())


ggplot(scatterplot_data_chart_data, aes(x = pointsFor, y = pointsAgainst)) +
  geom_text(aes(label = abbrev, color = playoffStatus), show.legend = FALSE) +  # Text colored by status, no legend
  geom_point(aes(color = playoffStatus), size = 4, alpha = 0) +  # Invisible points to trigger legend
  geom_vline(xintercept = mean(scatterplot_data_chart_data$pointsFor), color = "darkgreen", linewidth = 1.5, alpha = 0.4) +
  geom_hline(yintercept = mean(scatterplot_data_chart_data$pointsAgainst), color = "darkgreen", linewidth = 1.5, alpha = 0.4) +
  labs(x = "Points For", y = "Points Against", color = NULL) +
  theme(
    text = element_text(family = "Trebuchet", face = "bold"),
    strip.background = element_rect(fill = "#fafafa"),
    strip.text.x = element_text(size = 18),
    panel.background = element_rect(fill = "#fafafa"),
    plot.background = element_rect(fill = "#fafafa"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#fafafa"),
    legend.title = element_blank()
  )


source("2025 OPTFFL Connect.R")

standings_table <- league_standings(seasonId = 2025) %>% 
  inner_join(.,logo) %>%
  mutate(record = paste(wins,losses, sep = "-")) %>% 
  mutate(name = case_when(
    abbrev == "KAY" ~ "Alex",
    abbrev == "BTW" ~ "Amanda",
    abbrev == "CW" ~ "Virginia",
    abbrev == "KAS" ~ "Kyle",
    abbrev == "BWb" ~ "Bill",
    abbrev == "YARN" ~ "Ashley",
    abbrev == "JOHN" ~ "John",
    abbrev == "OPE" ~ "JJ",
    abbrev == "CNP" ~ "Cass",
    abbrev == "JFT" ~ "Jeanne",
    abbrev == "JOSh" ~ "Josh",
    abbrev == "TCC" ~ "Ellen",
    abbrev == "LLL" ~ "Luke",
    abbrev == "JII" ~ "Jacob",
    abbrev == "GPG" ~ "Catie",
    abbrev == "EET" ~ "Emily")) %>% 
  select("logo","name","playoffSeed","record") %>% 
  arrange(playoffSeed) %>% 
  gt() %>%
  text_transform(
    locations = cells_body(columns = c(logo)),
    fn = function(x) {
      web_image(url = x, height = 50)
    }
  ) %>%
  cols_label(
    playoffSeed = "Playoff Seed",
    logo = "",
    name = "Name",
    record = "Record"
  ) %>% 
  tab_style(
    style = cell_text(size = px(40)),  # Adjust the size as needed
    locations = cells_body(columns = c(name, playoffSeed, record))
  ) %>% 
  cols_align(
    align = "center",
    columns = c(name, playoffSeed)
  ) %>% 
  tab_options(
    table.background.color = "#fafafa",
    table.border.top.color = "#fafafa",
    table.border.bottom.color = "#fafafa"
  ) %>% 
  tab_style(
    style = cell_text(font = "Trebuchet", weight = "bold", size = px(20)),
    locations = cells_column_labels(columns = c(name,playoffSeed, record))
  ) %>% 
  tab_style(
    style = cell_text(font = "Trebuchet MS", weight = "bold", size = px(20)),
    locations = cells_column_labels(columns = c(name, playoffSeed, record))
  )


today <- lubridate::today()

gtsave(data = standings_table, filename = paste0("plots/",today,"_standings_table.png"))



