
mean_travel <- mean(data_all$Travel_centroid, na.rm = TRUE)
sd_travel <- sd(data_all$Travel_centroid, na.rm = TRUE)
x_orig <- seq(0, 215, length.out = 100)
x_std <- (x_orig - mean_travel) / sd_travel

# 1. vc_status
beta_linear_vc <- log(0.833)
beta_quad_vc <- log(1.069)
beta_linear_lower_vc <- log(0.673)
beta_linear_upper_vc <- log(1.023)
beta_quad_lower_vc <- log(1.014)
beta_quad_upper_vc <- log(1.137)

# 2. COVID_perceived
beta_linear_covid <- log(0.735)
beta_quad_covid <- log(1.065)
beta_linear_lower_covid <- log(0.594)
beta_linear_upper_covid <- log(0.910)
beta_quad_lower_covid <- log(1.006)
beta_quad_upper_covid <- log(1.127)

#  vc_status
log_odds_vc <- beta_linear_vc * x_std + beta_quad_vc * x_std^2
log_odds_lower_vc <- beta_linear_lower_vc * x_std + beta_quad_lower_vc * x_std^2
log_odds_upper_vc <- beta_linear_upper_vc * x_std + beta_quad_upper_vc * x_std^2

prob_vc <- exp(log_odds_vc)/(1 + exp(log_odds_vc))
prob_lower_vc <- exp(log_odds_lower_vc)/(1 + exp(log_odds_lower_vc))
prob_upper_vc <- exp(log_odds_upper_vc)/(1 + exp(log_odds_upper_vc))

# COVID_perceived
log_odds_covid <- beta_linear_covid * x_std + beta_quad_covid * x_std^2
log_odds_lower_covid <- beta_linear_lower_covid * x_std + beta_quad_lower_covid * x_std^2
log_odds_upper_covid <- beta_linear_upper_covid * x_std + beta_quad_upper_covid * x_std^2

prob_covid <- exp(log_odds_covid)/(1 + exp(log_odds_covid))
prob_lower_covid <- exp(log_odds_lower_covid)/(1 + exp(log_odds_lower_covid))
prob_upper_covid <- exp(log_odds_upper_covid)/(1 + exp(log_odds_upper_covid))

p1 <- ggplot() +
  geom_ribbon(aes(x = x_orig, ymin = prob_lower_vc, ymax = prob_upper_vc), 
              alpha = 0.2, fill = "#FF9999") +
  geom_line(aes(x = x_orig, y = prob_vc), 
            color = "#FF0000", size = 1) +
  annotate("text", x = 50, y = 0.8, 
           label = "P overall < 0.001\nP non-linear < 0.001",
           hjust = 0, size = 3) +
  labs(x = "Travel Time (minutes)",
       y = "Predicted Probability",
       title = "Predicted Probability of Vaccination Status by Travel Time",
       caption = "Adjusted for sociodemographic and health-related factors") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, hjust = 0)
  ) +
  scale_x_continuous(breaks = seq(0, 215, by = 50)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, by = 0.1)) +
  coord_cartesian(ylim = c(0.2, 0.8))

ggsave("C:/Users/User/Downloads/Travel_time/probability_vaccination.png", 
       p1, width = 10, height = 6, dpi = 300)

p2 <- ggplot() +
  geom_ribbon(aes(x = x_orig, ymin = prob_lower_covid, ymax = prob_upper_covid), 
              alpha = 0.2, fill = "#9999FF") +
  geom_line(aes(x = x_orig, y = prob_covid), 
            color = "#0000FF", size = 1) +
  annotate("text", x = 50, y = 0.8, 
           label = "P overall < 0.001\nP non-linear < 0.001",
           hjust = 0, size = 3) +
  labs(x = "Travel Time (minutes)",
       y = "Predicted Probability",
       title = "Predicted Probability of COVID-19 Risk Perception by Travel Time",
       caption = "Adjusted for sociodemographic and health-related factors") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 9, hjust = 0)
  ) +
  scale_x_continuous(breaks = seq(0, 215, by = 50)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 1, by = 0.1)) +
  coord_cartesian(ylim = c(0.2, 0.8))


ggsave("C:/Users/User/Downloads/Travel_time/probability_covid_perception.png", 
       p2, width = 10, height = 6, dpi = 300)