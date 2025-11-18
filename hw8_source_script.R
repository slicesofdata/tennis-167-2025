library(ggrepel)
library(grid)
library(dplyr)
library(tidyverse)
library(here)
library(tidyverse)
library(ggplot2)
library(scales)

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

cleaned_rankings_data <- readRDS(file = here::here("data", "processed", "cleaned_rankings_data.rds"))
atp_singles_data <- readRDS(file = here::here("data", "processed", "atp_singles.Rds"))


source(here::here("ranking_timeline.R"))



annotations <- tibble::tribble(
  ~ranking_date,        ~rank, ~label,             ~player,
  as.Date("2024-01-28"),   4,   "Australian Open",  "Sinner",
  as.Date("2024-09-06"),   1,   "US Open",          "Sinner",
  as.Date("2022-09-11"),   1,   "US Open",          "Alcaraz",
  as.Date("2023-09-16"),   2,   "Wimbledon",        "Alcaraz",
  as.Date("2024-06-09"),   2,   "French Open",      "Sinner",    
  as.Date("2024-07-14"),   3,   "Wimbledon",        "Alcaraz"
)

new2_ranking_timeline2 <- 
  ggplot(mapping = aes(x = ranking_date, y = rank)) +
  geom_line(data = cleaneddata_sinner, aes(color = "Sinner")) +
  geom_line(data = cleaneddata_alcaraz, aes(color = "Alcaraz")) +
  labs(x = "Date", y = "Ranking (1 = Highest)", color = " ", 
       title = "ATP Singles Ranking: Alcaraz vs Sinner (2022-2025)") +
  scale_color_manual(values = c("Sinner" = "#9C27B0", 
                                "Alcaraz" = "#FF8C00")) +
  scale_x_date(limits = as.Date(c("2022-01-01", Sys.Date()))) +
  scale_y_reverse(limits = c(10,1), 
                  breaks = 10:1,
                  labels = scales::number_format(accuracy = 1)) +
  geom_point(data = annotations, aes(x = ranking_date, y = rank, color = player), size = 2) +
  geom_text_repel(
    data = annotations,
    aes(x = ranking_date, y = rank, label = label),
    size = 2.5,                  
    nudge_x = 10,               
    nudge_y = -0.3,             
    hjust = 0,                   
    vjust = 1,                
    box.padding = 0.1,
    point.padding = 0.1,
    label.size = 0,
    fill = alpha("white", 0.65),
    min.segment.length = 0,
    seed = 42
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",   
    legend.box = "vertical"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3))) 
new2_ranking_timeline2

save_plot_png(plot = new2_ranking_timeline2, file_name = "new2_ranking_timeline_hw.png", figs_dir = "figs")