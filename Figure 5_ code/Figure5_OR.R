
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

# Calculate OR - vc_status
or_vc <- exp(beta_linear_vc * x_std + beta_quad_vc * x_std^2)
or_lower_vc <- exp(beta_linear_lower_vc * x_std + beta_quad_lower_vc * x_std^2)
or_upper_vc <- exp(beta_linear_upper_vc * x_std + beta_quad_upper_vc * x_std^2)

# Calculate OR - COVID_perceived
or_covid <- exp(beta_linear_covid * x_std + beta_quad_covid * x_std^2)
or_lower_covid <- exp(beta_linear_lower_covid * x_std + beta_quad_lower_covid * x_std^2)
or_upper_covid <- exp(beta_linear_upper_covid * x_std + beta_quad_upper_covid * x_std^2)

# vc_status
p1 <- ggplot() +

  geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +

  geom_ribbon(aes(x = x_orig, ymin = or_lower_vc, ymax = or_upper_vc), 
              alpha = 0.2, fill = "#FF9999") +

  geom_line(aes(x = x_orig, y = or_vc), 

  annotate("text", x = 50, y = max(or_upper_vc) * 0.9, 
           label = "P overall < 0.001\nP non-linear < 0.001",
           hjust = 0, size = 3) +
  labs(x = "Travel Time (minutes)",
       y = "Odds Ratio (95% CI)",
       title = "Association Between Travel Time and Vaccination Status",
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
  scale_y_continuous(breaks = seq(0, ceiling(max(or_upper_vc)), by = 0.5))

ggsave("C:/Users/User/Downloads/Travel_time/OR_vaccination.png", 
       p1, width = 10, height = 6, dpi = 300)

p2 <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
  geom_ribbon(aes(x = x_orig, ymin = or_lower_covid, ymax = or_upper_covid), 
              alpha = 0.2, fill = "#9999FF") +
  geom_line(aes(x = x_orig, y = or_covid), 
            color = "#0000FF", size = 1) +
  annotate("text", x = 50, y = max(or_upper_covid) * 0.9, 
           label = "P overall < 0.001\nP non-linear < 0.001",
           hjust = 0, size = 3) +
  labs(x = "Travel Time (minutes)",
       y = "Odds Ratio (95% CI)",
       title = "Association Between Travel Time and COVID-19 Risk Perception",
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
  scale_y_continuous(breaks = seq(0, ceiling(max(or_upper_covid)), by = 0.5))

ggsave("C:/Users/User/Downloads/Travel_time/OR_covid_perception.png", 
       p2, width = 10, height = 6, dpi = 300)