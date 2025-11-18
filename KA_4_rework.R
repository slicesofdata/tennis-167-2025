library(ggridges)
library(tidyverse)
library(ggplot2)

#Code that I wrote down
plot_1 <- ggplot2::diamonds %>%
  ggplot(mapping = aes(x = factor(clarity), y = price)) +
  geom_density_ridges(scale = 1) +
  coord_flip() #I forgot to add the "()" after coord_flip
plot_1 


#For ggridges, x neds to be a numeric variable and y needs to be categorical (I switched them)
#Do not need to factor clarity because it is already a character
#Did not need to use coordinate flip
plot_1 <- ggplot2::diamonds %>%
  ggplot(mapping = aes(x = price, y = clarity)) +
  geom_density_ridges(scale = 1) 
plot_1





  geom_density_ridges2(scale = 1, rel_min_height = 0.0005) +
  coord_flip()

  



