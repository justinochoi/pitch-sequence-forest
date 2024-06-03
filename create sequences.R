data22 <- read.csv(file.choose()) 
data23 <- read.csv(file.choose()) 

library(dplyr) 
library(stringr)

data23 <- 
  data23 %>% 
  select(-bat_speed, -swing_length) 
data <- bind_rows(data22, data23)

# Remove bunts, LA outliers, and NA values 
q1 <- quantile(data$launch_angle, .25, na.rm = T) 
q3 <- quantile(data$launch_angle, .75, na.rm = T)
iqr <- IQR(data$launch_angle, na.rm = T) 
la_outliers <- subset(data, data$launch_angle < (q1 - 1.5*iqr) | 
                            data$launch_angle > (q3 + 1.5*iqr))

data_clean <- 
  data %>% 
  filter_at(vars(pitch_type, game_date, batter, pitcher,
                 outs_when_up, inning, pitch_number), 
            all_vars(!is.na(.))) %>% 
  filter(events != 'sac_bunt') %>% 
  filter(balls !=4, strikes !=3) %>% 
  anti_join(la_outliers)

# Add platoon indicator, count, and batted ball type 
# Also merge certain pitch types to avoid redundancy 
data_clean <- 
  data_clean %>% 
  mutate(count = paste(balls, strikes, sep = '-'), 
         platoon_adv = if_else(p_throws != stand, 1, 0), 
         outcome = case_when(launch_angle < -5 ~ 'downer',
                             launch_angle < 10 ~ 'grounder',
                             launch_angle < 25 ~ 'liner',
                             launch_angle < 45 ~ 'flyball',
                             is.na(launch_angle) ~ NA, 
                             .default = 'popup'),
         pitch_type = case_when(pitch_type == 'KC'~'CU',
                                pitch_type == 'FS'~'CH',
                                .default = pitch_type), 
  ) 

# Calculate expected run values 
exp_rv <- 
  data_clean %>% 
  filter(!is.na(outcome)) %>% 
  group_by(count, platoon_adv, outcome) %>% 
  summarize(exp_rv = mean(delta_run_exp, na.rm = T)) 

data_clean <- 
  left_join(data_clean, exp_rv, by = c('count','platoon_adv','outcome')) 

# Indicate pitch seq and run value for each plate appearance 
pa_data <- 
  data_clean %>% 
  filter(pitch_type %in% c('FF','SI','FC','SL','CU','CH')) %>%
  group_by(game_date, pitcher, batter, inning, outs_when_up) %>% 
  mutate(is_last_pitch = if_else(pitch_number == max(pitch_number), 1, 0)) %>%
  summarize(pitch_sequence = str_flatten(pitch_type, collapse="-"),
            seq_length = n(), 
            exp_rv = mean(if_else(is_last_pitch == 1, exp_rv, NA), na.rm = T)
            ) %>% 
  filter(!is.na(exp_rv))

# Find pitch sequences that have occurred min. 5 times 
seq_data <- 
  pa_data %>% 
  group_by(pitch_sequence) %>% 
  summarize(n = n(), 
            seq_length = mean(seq_length), 
            mean_exp_rv = round(mean(exp_rv), 4)) %>% 
  filter(n >= 5)

# Store each pitch of sequence in a column and merge with length and rv data 
model_data <- 
  bind_cols(as.data.frame(str_split(seq_data$pitch_sequence, "-", simplify = T)),
            seq_data$seq_length, seq_data$mean_exp_rv) %>% 
  rename_all(., ~sub('V', paste('pitch'), names(model_data))) %>% 
  mutate(across(pitch1:pitch9, .fns = factor)) 

# Best first pitch '
one_pitch <- 
  test %>% 
  filter(seq_length >= 1) %>% 
  group_by(pitch1) %>% 
  summarize(pred_mean_rv = round(mean(pred_mean_rv),4),
            mean_rv = round(mean(mean_exp_rv),4)) 

# Best two-pitch sequences 
two_pitch <- 
  test %>% 
  filter(seq_length >= 2) %>% 
  group_by(pitch1, pitch2) %>% 
  summarize(pred_mean_rv = round(mean(pred_mean_rv),4), 
            actual_mean_rv = round(mean(mean_exp_rv),4)) 

# Best three-pitch sequences
three_pitch <- 
  test %>% 
  filter(seq_length >= 3) %>%
  group_by(pitch1, pitch2, pitch3) %>%
  summarize(pred_mean_rv = round(mean(pred_mean_rv),4),
            actual_mean_rv = round(mean(mean_exp_rv),4)) 


