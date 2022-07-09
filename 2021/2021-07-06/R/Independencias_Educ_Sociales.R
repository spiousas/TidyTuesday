# Manejo de paquetes con Pacman
pacman::p_load(BBmisc, tidyverse, hablar, ggflags, sf, rnaturalearth, janitor, 
               lubridate, here, ggbump, countrycode, ggtext, colorspace, extrafont)
options(stringsAsFactors = F)

# Bajo la data de TidyTuesday
tuesdata <- tidytuesdayR::tt_load('2021-07-06')
tuesdata <- tidytuesdayR::tt_load(2021, week = 28)

holidays <- tuesdata$holidays

rm(tuesdata)

# Los países de Sudamérica
south_america <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia",
                   "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname",
                   "Uruguay", "Venezuela", "France")


bb = st_bbox(c(xmin = -88, xmax = -28, ymin = -56, ymax = 12), crs = sf::st_crs (4267))
bb_sf <- bb %>% st_as_sfc()

# Filtro el mapa
mapSA <- rnaturalearthdata::countries50 %>% 
  st_as_sf() %>% 
  st_crop(xmin = -88, xmax = -28, ymin = -56, ymax = 12) %>% 
  filter(admin %in% south_america) %>% 
  select(c(admin, geometry)) %>%
  left_join(holidays %>% # Agrego los datos de las independencias de los países de sudamérica
              filter(country %in% south_america) %>%
              select(c(country, date_parsed, independence_from)), 
            by = c("admin" = "country")) %>%
  mutate(independence_from = case_when(independence_from == "Netherlands" ~ "Países Bajos", # Renombro
                                       independence_from == "Spain" ~ "España",
                                       independence_from == "Spanish Empire" ~ "España",
                                       independence_from == "United Kingdom" ~ "Reino Unido",
                                       independence_from == "Spanish Empire[72]" ~ "España",
                                       independence_from == "United Kingdom of Portugal, Brazil and the Algarves" ~ "Portugal",
                                       independence_from == "United Provinces of the Rio de la Plata"  ~ "Prov. U. del Río de la Plata",
                                       TRUE ~ independence_from)) %>%
  filter(date_parsed != "1809-08-10") # Filtro una fecha de Ecuador que no va


# Genero la geometría para el ggbump
countries_date <- st_geometry(mapSA[mapSA$admin != "France",]) %>% 
  st_point_on_surface() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  bind_cols(tibble(countries_cap = normalize(rank(mapSA[mapSA$admin != "France",]$date_parsed), range = c(12, -56), method = "range"),
                   country = mapSA[mapSA$admin != "France",]$admin,
                   date = stamp("31-12-1999")(mapSA[mapSA$admin != "France",]$date_parsed),
                   independence_from = mapSA[mapSA$admin != "France",]$independence_from,
                   xend = -95,#-28,
                   x_axis_start = xend + .01),
            country_id = countrycode(mapSA[mapSA$admin != "France",]$admin,
                                     origin = "country.name",
                                     destination = "genc2c") %>% tolower())


# Seteo el tema
theme_set(
  theme_void(
    ## increase size of all text elements
    base_size = 12, 
    ## set custom font family for all text elements
    base_family = "Gotham Medium")
)

# Escala de colores para los países de los que nos independizamos
cols = c("#5CE1E6", "#7ED957", "#FFA306", "#C00C1E", "white")

# La figura
ggplot() + 
  # Mapa
  geom_sf(data = mapSA, 
          aes(fill = independence_from), 
          size = .6, color = "#1D242C", alpha = .8) +
  # Líneas
  geom_sigmoid(data = countries_date, 
               aes(x = X, y = Y, xend = x_axis_start, yend = countries_cap, group = country, color = independence_from), 
               alpha = 1.5, smooth = 10, size = 1) +
  # Puntos
  geom_point(data = countries_date, 
             aes(x = X, y = Y, fill = independence_from), 
             color = "#1D242C",
             shape = 21,
             alpha = .8, size = 3) +
  # Texto
  geom_text(data = countries_date, 
            aes(x = x_axis_start - 3 , y = countries_cap, label = as.character(date)), 
            family = "Gotham Medium",
            color = "white",
            hjust = 1, size = 3.5) +
  # Banderas
  geom_flag(data = countries_date, 
            aes(x = x_axis_start, y = countries_cap, country = country_id),
            size = 10) +
  # Tema y estética
  coord_sf(clip = "off") +
  scale_color_manual(values = cols,
                     breaks = c("España", "Portugal", "Países Bajos", "Reino Unido", "Prov. U. del Río de la Plata"),
                     guide = guide_legend(title = "Declara la independencia de:", 
                                          title.position = "top",
                                          ncol = 1)) +
  scale_fill_manual(values = cols,
                    breaks = c("España", "Portugal", "Países Bajos", "Reino Unido", "Prov. U. del Río de la Plata"),
                    guide = NULL) +
  labs(subtitle = str_wrap("Fechas de la independencia de los países de sudamérica ordenadas cronológicamente 
                           (de arriba hacia abajo). El color de la línea codifica de qué país obtuvo su independencia.
                           ", 90),
       caption = "**Fuente:** #TidyTuesday | **Viz:** @spiousas") +
  theme(plot.margin = margin(.2, 2, .2, 2, "cm"),
        legend.position = c(.8,.13),
        legend.title = element_text(color = "white", size = 10, family = "Gotham Medium"),
        legend.text = element_text(color = "white", size = 9, family = "Gotham Medium"),
        plot.background = element_rect(fill = "#1D242C", colour = "#1D242C"),
        plot.title = element_text(color = "white", size = 20, family = "Gotham Bold", hjust = 0.5),
        plot.title.position = "panel",
        plot.subtitle = element_text(color = "white", size = 10, family = "Gotham Medium", hjust = 0.5),
        plot.caption = element_markdown(color = "white", size = 9, family = "Gotham Light", hjust = 0.5),
        plot.caption.position = "panel")

ggsave(here("./2021/2021-07-06/indep_ed_sociales.png"), width = 2000, height = 2100, units = "px")
   
