library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)

rome = player_play %>%
  filter(nfl_id == 57130)

rome = rome %>%
  select(game_id, play_id, lined_up_position, route_ran, in_motion_at_ball_snap,
         team_coverage_man_zone, team_coverage_type, target, separation_at_pass_forward)

rome %>%
  count(route_ran, sort = TRUE)

rome_results = combine_results %>%
  filter(nfl_id == 57130)

rome_tracking = combine_tracking %>%
  filter(nfl_id == 57130)




# BAG
wr = player_play %>%
  filter(lined_up_position == "WR" |
         lined_up_position == "Slot_WR")

wr = wr %>%
  select(game_id, play_id, nfl_id, lined_up_position, route_ran, in_motion_at_ball_snap,
         team_coverage_man_zone, team_coverage_type, target, separation_at_pass_forward)

wr = wr %>%
  filter(!is.na(separation_at_pass_forward))

wr_combined = wr %>%
  inner_join(
    combine_results %>% select(nfl_id, forty, ten_yd_split, vertical,
                            broad_jump, three_cone, short_shuttle, ngs_athleticism_score),
    by = "nfl_id"
  )

variables = c("forty", "ten_yd_split", "vertical", "broad_jump", "three_cone",
              "short_shuttle", "ngs_athleticism_score")

cor_results = map_dfr(variables, function(var) {
  
  data <- wr_combined %>%
    select(separation_at_pass_forward, all_of(var)) %>%
    filter(complete.cases(.))
  
  test <- cor.test(
    data$separation_at_pass_forward,
    data[[var]],
    method = "pearson"
  )
  
  tibble(
    variable = var,
    n = nrow(data),
    correlation = unname(test$estimate),
    p_value = test$p.value
  )
}) %>%
  mutate(adjusted_p = p.adjust(p_value, method = "BH")) %>%
  arrange(p_value)

cor_results


# try with only targetted plays
wr_target = wr_combined %>%
  filter(target == "TRUE")

cor_results2 = map_dfr(variables, function(var) {
  
  data <- wr_target %>%
    select(separation_at_pass_forward, all_of(var)) %>%
    filter(complete.cases(.))
  
  test <- cor.test(
    data$separation_at_pass_forward,
    data[[var]],
    method = "pearson"
  )
  
  tibble(
    variable = var,
    n = nrow(data),
    correlation = unname(test$estimate),
    p_value = test$p.value
  )
}) %>%
  mutate(adjusted_p = p.adjust(p_value, method = "BH")) %>%
  arrange(p_value)

cor_results2
