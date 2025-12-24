library(gganimate)

p <- ggplot(
  cumulative_score,
  aes(x = scoringPeriodId,
      y = cumulative_score,
      group = abbrev,
      color = abbrev)   # color only applies to the line
) +
  geom_line(size = 1.2) +
  # logos: drop the color aesthetic here
  geom_image(aes(x = scoringPeriodId, 
                 y = cumulative_score,
                 image = logo), size = 0.05, inherit.aes = FALSE) +
  scale_color_manual(values = OPTFFL_team_colors) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Cumulative Scores by Team",
    x = "Scoring Period",
    y = "Cumulative Score"
  ) +
  theme(legend.position = "none")

anim <- p + transition_reveal(scoringPeriodId)
animate(anim, fps = 20, width = 800, height = 600)


anim_save("cumulative_scores.gif", animation = anim,  width = 800, height = 600)
