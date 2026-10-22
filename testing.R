library(dplyr)
library(ggplot2)

players <- read.csv("C:/Users/hanke/Projects/Big_Data_Bowl_27/data/players.csv")
game_tracking_2023 <- read.csv("C:/Users/hanke/Projects/Big_Data_Bowl_27/data/game_tracking_2023.csv")
player_play <- read.csv("C:/Users/hanke/Projects/Big_Data_Bowl_27/data/player_play.csv")

# This data set is to find all the Will Anderson movement information
will_anderson <- game_tracking_2023 %>%
  filter(nfl_id == 55867)

# This data set is to find every play for Will Anderson and where he lined up
anderson_position = player_play %>%
  filter(nfl_id == 55867)

# Today the goal is to look at one play from 2023

# This code makes a df of the specific play
play_55 <- will_anderson %>%
  filter(
    game_id == 2023111202,
    play_id == 55
  ) %>%
  arrange(time)

# We can find where Anderson started and his direction facing at the snap
play_55 %>%
  filter(
    event == "ball_snap"
  ) %>%
  select(x, y, o, dir) %>%
  print()

# In this area we are making two new columns. 
# body_orientation is the angle of the players body
# direction is the direction the player goes
play_55 <- play_55 %>%
  mutate(body_orientation = case_when(
    o >= 0   & o <= 36  ~ "Left",
    o > 36   & o <= 72  ~ "Forward Left",
    o > 72   & o <= 108 ~ "Forward",
    o > 108  & o <= 144 ~ "Forward Right",
    o > 144  & o <= 180 ~ "Right",
    o > 180  & o <= 216 ~ "Left",
    o > 216  & o <= 252 ~ "Forward Left",
    o > 252  & o <= 288 ~ "Forward",
    o > 288  & o <= 324 ~ "Forward Right",
    o > 324  & o <= 360 ~ "Right",
    TRUE ~ NA_character_ 
  )) %>%
  mutate(direction = case_when(
    dir >= 0   & dir <= 36  ~ "Left",
    dir > 36   & dir <= 72  ~ "Forward Left",
    dir > 72   & dir <= 108 ~ "Forward",
    dir > 108  & dir <= 144 ~ "Forward Right",
    dir > 144  & dir <= 180 ~ "Right",
    dir > 180  & dir <= 216 ~ "Left",
    dir > 216  & dir <= 252 ~ "Forward Left",
    dir > 252  & dir <= 288 ~ "Forward",
    dir > 288  & dir <= 324 ~ "Forward Right",
    dir > 324  & dir <= 360 ~ "Right",
    TRUE ~ NA_character_ 
  ))

# We can find what position Anderson was at for this snap
anderson_position %>%
  filter(
    game_id == "2023111202",
    play_id == "55"
  ) %>%
  select(lined_up_position) %>%
  print()

# Next we can make two plots from this play

# Plot of Anderson's movements on this play, entire field view
ggplot(play_55, aes(x = x, y = y)) +
  geom_path(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  geom_hline(yintercept = 23.61, color = "black", 
             linetype = "dashed", linewidth = 0.5, alpha = 0.5) +
  geom_hline(yintercept = 29.69, color = "black", 
             linetype = "dashed", linewidth = 0.5, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 120), 
                     breaks = seq(0,120, by = 10),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 53.3), expand = c(0, 0)) +
  coord_fixed() +
  labs(
    title = "Will Anderson's Movement - Play 55 Nov 12",
    x = "Field Length (yards)",
    y = "Field Width (yards)"
  ) +
  theme_minimal()

# Zoomed in view of Anderson's play
ggplot(play_55, aes(x = x, y = y)) +
  geom_hline(
    yintercept = 23.61,
    color = "black",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_hline(
    yintercept = 29.69,
    color = "black",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_path(color = "blue", linewidth = 0.7) +
  geom_point(color = "red", size = 1.5) +
  coord_cartesian(
    xlim = range(play_55$x, na.rm = TRUE) + c(-1, 1),
    ylim = range(c(play_55$y, 20.5, 32.7), na.rm = TRUE) +
      c(-1, 1)
  ) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(n = 6)
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 6)
  ) +
  labs(
    title = "Will Anderson's Movement - Play 55",
    x = "x",
    y = "y"
  ) +
  theme_minimal()



# Testing with event start

snap = will_anderson %>%
  filter(
    event == "ball_snap"
  )

snap = snap %>%
  mutate(body_orientation = case_when(
    o >= 0   & o <= 36  ~ "Left",
    o > 36   & o <= 72  ~ "Forward Left",
    o > 72   & o <= 108 ~ "Forward",
    o > 108  & o <= 144 ~ "Forward Right",
    o > 144  & o <= 180 ~ "Right",
    o > 180  & o <= 216 ~ "Left",
    o > 216  & o <= 252 ~ "Forward Left",
    o > 252  & o <= 288 ~ "Forward",
    o > 288  & o <= 324 ~ "Forward Right",
    o > 324  & o <= 360 ~ "Right",
    TRUE ~ NA_character_ 
  ))

anderson_position = anderson_position %>%
  select(game_id, play_id, lined_up_position, pass_result, player_get_off, sack, tackle, assist,
         caused_forced_fumble, tackle_for_loss, time_to_pressure, time_to_qb_hurry)

anderson_position = anderson_position %>%
  filter(!is.na(pass_result), pass_result != "")

anderson_position = anderson_position %>%
  filter(!is.na(player_get_off))

combined_df = anderson_position %>%
  left_join(snap, by = c("game_id", "play_id"))

combined_df = combined_df %>%
  filter(!is.na(x))

combined_df %>%
  count(body_orientation, sort = TRUE) %>%
  print()

anova_model = aov(
  player_get_off ~ body_orientation,
  data = combined_df
)

summary(anova_model)
