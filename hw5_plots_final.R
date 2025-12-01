library(readr)
library(tidyverse)
readr::read_csv(
  "https://raw.githubusercontent.com/slicesofdata/dataviz25/main/data/processed/
cleaned-2023-cms-invite.csv")


cms_data <- read_csv("https://raw.githubusercontent.com/slicesofdata/dataviz25/main/data/processed/cleaned-2023-cms-invite.csv")


save_plot_png <- function(
    plot,
    file_name, 
    figs_dir,
    units = "px",
    width = 1600,
    height = 1100,
    dpi = 300) {
  
  file_path <- here::here(figs_dir, file_name)
  
  ggsave(
    filename = file_path,
    plot = plot,
    device = "png",
    units = units,
    width = width,
    height = height,
    dpi = dpi
  )
}

plot_1 <- cms_data %>%
  filter(Distance == "100") %>%
  ggplot(mapping = aes(x = Split50, y = Time, color = Team)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0,40), breaks = seq(0, 40, by = 5)) +
  scale_y_continuous(limits = c(0,80), breaks = seq(0, 80, by = 5)) +
  labs(x = "Split Time (seconds)", y = "Time (seconds)", title = "100m Events") +
  theme_minimal()
plot_1


save_plot_png(plot = plot_1, file_name = "hw5_plot1.png", figs_dir = "src/figs")


plot_2 <- cms_data %>%
  filter(Distance == "100") %>%
  ggplot(mapping = aes(x = Split50, y = Time, color = Team)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  scale_x_continuous(limits = c(0,40), breaks = seq(0, 40, by = 5)) +
  scale_y_continuous(limits = c(0,80), breaks = seq(0, 80, by = 5)) +
  labs(x = "Split Time (seconds)", y = "Time (seconds)", title = "100m Events") +
  coord_equal() + #this changes how the data is perceived - the x axis becomes a lot shorter and more compact
  theme_minimal()
plot_2


save_plot_png(plot = plot_2, file_name = "hw5_plot2.png", figs_dir = "src/figs")


plot_3 <- cms_data %>%
  filter(Distance == "200") %>%
  filter(Event != "Medley") %>%
  filter(Team != "Mixed") %>%
  ggplot(mapping = aes(x = Event, y = Time, color = Team)) +
  stat_summary(fun.data = mean_se, position = position_dodge(width = 0.5), size = 0.25) +
  labs(x = " ", y = "Time (seconds)", title = "200m Events") +
  scale_y_continuous(limits = c(100,150), breaks = seq(100, 150, by = 5)) 
plot_3


save_plot_png(plot = plot_3, file_name = "hw5_plot3.png", figs_dir = "src/figs")


