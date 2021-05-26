# Library loading
library(tidyverse)
library(ggtext)
library(ggthemes)
library(ggstream)
library(lubridate)
library(magick)
library(ggforce)

# Mario Kart Logo
png_logo <- image_read("./data/MarioKart64.png")
img_logo <- grid::rasterGrob(png_logo, interpolate = TRUE)

# Anotations
annotations <- tibble(year = c(1999.7, 2019.3),
                      n = c(3000, -2000),
                      hjust = c(0, 1),
                      vjust = c(0, 1),
                      label = c(paste0("Mario Kart gains popularity in ",
                                       "<b style='color:#FFCD00;'>Australia</b>"),
                                paste0("Covid Pandemics motivated people to play Mario Kart 64 again, ",
                                       "specially in <b style='color:#B80E3C;'>North America</b>"))
)

# Dirvers data
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')

# Records data
records <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv') %>%
  inner_join(drivers %>% select(player, nation), by = "player") %>%
  drop_na(nation) %>%
  mutate(year = year(date),
         quarter = month(date) %/% 3,
         shortcut = if_else(shortcut=="Yes", "Shortcut", "No shortcut"),
         nation = if_else(nation %in% c("Canada", "USA"), "North America",
                          if_else(nation == "Australia", "Australia", "Europe"))) %>%
  group_by(year, quarter, type, nation) %>%
  summarise(n = n(),
            date = min(date)) %>%
  mutate(n = if_else(type == "Single Lap", n, -n))

# Plot
ggplot(data = records, aes(x = year,
                           y = n,
                           fill = nation)) +
  geom_textbox(data = annotations, 
               aes(label = label, 
                   hjust = hjust,
                   vjust = vjust,),
               halign = 0,
               width = unit(0.2, "npc"),
               fill = "gray60") +
  geom_col(data = records %>% filter(type == "Single Lap")) +
  geom_col(data = records %>% filter(type == "Three Lap"),) +
  geom_hline(yintercept = 0,
             color = "white") +
  scale_y_continuous(labels = abs(seq(-5000,5000,2500)),
                     breaks= seq(-5000,5000,2500),
                     limits = c(-5000, 5000)) +
  scale_fill_manual(values = c("#B80E3C", "#FFCD00", "#003399"),
                     breaks = unique(records$nation)) +
  annotation_custom(img, 
                    ymin = 3000, ymax = 5000,
                    xmin = 2015, xmax = 2021) +
  theme_fivethirtyeight() +
  labs(x = NULL,
       y = paste0("<img src='data/arrow_rot.png'‚ height='7'/> &nbsp;",
                  "**Three** laps records &nbsp;",
                  "<img src='data/Mario.png'‚ height='20'/> &nbsp;",
                  "**Single** lap records &nbsp;",
                  "<img src='data/arrow.png'‚ height='7'/>"),
       fill = NULL,
       title = "Mario Kart 64 World Records by player nationality",
       subtitle = paste0("Number of records set per year by players from",
                         "<b style='color:#B80E3C;'>North America</b>","
                         , <b style='color:#FFCD00;'>Australia</b>","
                         , and <b style='color:#003399;'>Europe</b>."),
       caption = "**Data**: mkwrs.com | **Viz**: @spiousas") +
  theme(text = element_text(colour = "white"),
        legend.position = "none",
        plot.caption = element_markdown(),
        plot.subtitle = element_markdown(),
        axis.title.y = element_markdown(hjust = 0.5, halign = 0.5),
        plot.background = element_rect(fill = "#636C71"),
        panel.background = element_rect(fill = "#636C71"),
        plot.caption.position = 'plot',
        plot.title.position = 'plot')

ggsave("../img.png", dpi = 300, width = 20, height = 15, units = "cm")
