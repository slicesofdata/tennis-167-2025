library(dplyr)
library(tidyverse)
library(here)
library(tidyverse)
library(ggplot2)
library(scales)

#######################################################################################################################

atp_singles_data <- readRDS(file = here::here("data","processed","atp_singles.Rds")) 
head(atp_singles_data)

#######################################################################################################################

mean_ci <- function(x, level = 0.95) {
  x = na.omit(x)
  m = mean(x)
  se = sd(x) / sqrt(length(x))
  ci = qnorm(1- (1- level) / 2) * se
  return(
    data.frame(y = m, ymin = m- ci, ymax = m + ci)
  )
}

#######################################################################################################################


filtered_atp_singles <- atp_singles_data %>%
  filter(
    winner_name %in% c("Roger Federer", "Novak Djokovic", "Rafael Nadal", 
                       "Jannik Sinner", "Carlos Alcaraz")) %>%
  filter (tourney_level == "G") 
filtered_atp_singles


#graph with one confidence interval
plot_variability <- filtered_atp_singles %>%
  ggplot(mapping = aes(x = minutes, y = winner_name)) +
  stat_summary(fun.data = mean_ci, geom = "pointrange") +
  labs(x = "Average Match Time (minutes)", y = "",
       title = "Average Match Time (Minutes) at a Grand Slam") +
  theme_minimal()
plot_variability


#graph with three different confidence intervals
plot_variability <- filtered_atp_singles %>%
  ggplot(mapping = aes(x = minutes, y = winner_name)) +
  stat_summary(fun.data = function(x) mean_ci(x, level = 0.99),
             geom = "errorbarh", color = "gray60", size = 0.8, height = 0.2) +
  stat_summary(fun.data = function(x) mean_ci(x, level = 0.95),
               geom = "errorbarh", color = "blue", size = 1, height = 0.2) +
  stat_summary(fun.data = function(x) mean_ci(x, level = 0.85),
               geom = "errorbarh", color = "red", size = 1.2, height = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 2, color = "black") +
  labs(x = "Average Match Time (minutes)", y = "",
       title = "Average Match Time (Minutes) at a Grand Slam") +
  theme_minimal()
plot_variability



