################################################################################
# Script Name: Homework 3
# Author: Lindsay Eisenman
# GitHub: lindsayeisenman
# Date Created: 9/25/25
#
# Purpose: This script ...
#
################################################################################

################################################################################
# Note: When sourcing script files, if you do not want objects
# available in this script, use the source() function along with
# the local = TRUE argument. By default, source() will make
# objects available in the current environment.

################################################################################
# Load necessary libraries/source any function directories
library(here)
library(ggplot2)

################################################################################
#Create function

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
################################################################################
# Test function

my_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() 
my_plot 

save_plot_png(plot = my_plot, file_name = "wt_vs_mpg.png", figs_dir = "figs")

################################################################################
# End of script
