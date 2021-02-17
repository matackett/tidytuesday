## DuBuois Challenge

# TidyTuesday link: https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-02-16/readme.md
# Style guide: https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdff
## Freed Slaves

# link to original image: https://github.com/ajstarks/dubois-data-portraits/blob/master/challenge/challenge04/original-plate-51.jpg

library(tidyverse)
library(ggrepel)
library(showtext)

# add Public Sans Font
font_add_google("Public Sans", "public_sans")
font_add_google("Gochi Hand", "gochi")
font_add_google("Big Shoulders Display", "shoulders")


freed_slaves <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/freed_slaves.csv')

freed_long <- freed_slaves %>%
  pivot_longer(cols = c("Slave", "Free"), 
               names_to  = "status", 
               values_to = "percent")

freed_long <- freed_long %>%
  mutate(percent_label = paste0(percent, "%"))

ggplot(data = freed_long, aes(x = Year, y = percent, fill = status)) +
  geom_area() +
  theme_void() + 
  theme(plot.background = element_rect(fill = "#e4d2c1")) +
  scale_fill_manual(values=c("#00aa00", "#000000")) +
  theme(legend.position="none") +
  geom_text(data = freed_long %>% filter(status == "Free", Year != 1870), 
            aes(label  = percent_label, y = 100  - percent + 2.5 )) +
  geom_text(data = freed_long %>% filter(status == "Free", Year == 1870), 
            aes(label  = percent_label, y = 91.5)) +
  annotate(geom = "text", label  = "SLAVES \n ENCLAVES", color = "white", 
           x = 1830, y = 50, family = "shoulders") +
  annotate(geom = "text", label = "FREE - LIBRE", color = "#000000", 
           x = 1830, y = 95)  +
  labs(title = "PROPORTION OF FREEMAN AND SLAVES AMONG AMERICAN NEGROS \n 
       PROPORTION DES NEGRES LIBRES ET ESCLAVES EN AMERIQUE \n", 
       subtitle = "DONE BY ATLANTA UNIVERSITY") +
  theme(plot.subtitle = element_text(family = "shoulders", 
                                     hjust = 0.5))

options <- list(
  GeomCol=list(fill_style="hachure",  angle_noise=0.8, gap_noise=0.2))
get_rough_chart(p, options)




