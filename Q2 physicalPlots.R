library(tidyverse)
library(fflr)

source("theme.R")


fflr::ffl_id(leagueId = 710908445)
standings <- fflr::league_standings()

data <- inner_join(nfl_players,roster_data) %>% 
  select(1:19)

data <- inner_join(data,standings,by="abbrev")

phys_data <-data %>%
  group_by(abbrev) %>% 
  summarise(mean_weight = mean(weight, na.rm = TRUE),
            mean_height = mean(height, na.rm = TRUE),
            mean_age = mean(age, na.rm = TRUE),
            standing = mean(playoffSeed, na.rm=TRUE),
            pointsAgainst = mean(pointsAgainst, na.rm=TRUE),
            pointsFor = mean(pointsFor, na.rm=TRUE)) %>% 
  inner_join(.,logo) # logo from 2025 OPTFFL Connect.R


height_plot <- ggplot(data = phys_data, aes(x=mean_height ,y=standing))+
  geom_image(aes(image = logo),size = 0.08)+
  geom_smooth(method = "lm",se = F)+
  geom_vline(xintercept = mean(phys_data$mean_height))+
  geom_hline(yintercept = 8)+
  labs(x="Average Height(Inches)",y="Standing",title = "Height & Standing")+
  optffl_theme()

age_plot <- ggplot(data = phys_data, aes(x=mean_age ,y=standing))+
  geom_image(aes(image = logo),size = 0.1)+
  geom_smooth(method = "lm",se = F)+
  geom_vline(xintercept = mean(phys_data$mean_age))+
  geom_hline(yintercept = 8)+
  labs(x="Average Age(Years)",y="Standing",title = "Age & Standing")+
  optffl_theme()

library(cowplot)

plot_grid(height_plot,age_plot, ncol = 2)




# ggplot(data = phys_data, aes(x=mean_height ,y=pointsFor))+
#   geom_text(aes(label = abbrev),size = 6)+
#   geom_vline(xintercept = mean(phys_data$mean_height), linetype = "dashed", color = "red")+
#   geom_hline(yintercept = mean(phys_data$pointsFor),linetype = "dashed",color="red")


library(stats)

lm1 <-lm(standing ~ mean_age+mean_height+mean_weight,data = phys_data)

plot(lm1)



plot(phys_data)
library(GGally)

ggpairs(phys_data,columns = 2:7)





ggplot(roster_data,aes(x=actualScore,fill=abbrev))+geom_histogram()+
  facet_wrap(~position)












