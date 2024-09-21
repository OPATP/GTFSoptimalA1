# Universidade Federal do Ceará
# Programa de Pós-Graduação em Engenharia de Transportes
# Nelson de O. Quesado Filho
# Julho de 2024
rm(list = ls()); gc()

# preambulo ----
#remotes::install_github('OPATP/GTFSwizard', force = T, update = 'always')
library(tidyverse)
library(tidylog)

library(GTFSwizard)

library(aopdata)

options(java.parameters = "-Xmx6G")
library(r5r)
library(sf)

rstudioapi::getActiveDocumentContext() %>% 
  .$path %>% 
  dirname() %>% 
  setwd(); setwd('..')

theme_gtfswizard <-
  hrbrthemes::theme_ipsum(base_family = "Times New Roman",
                          axis_title_face = 'bold') +
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 11, color = '#283c42'),
        axis.title.y = ggplot2::element_text(size = 11, color = '#283c42'),
        axis.text.x = ggplot2::element_text(size = 10),
        axis.text.y = ggplot2::element_text(size = 10),
        legend.text = ggplot2::element_text(size = 10))

# ts data ----
bus.gtfs.file <- 'data/gtfs/baseline.gtfs.zip'
bus.gtfs <- read_gtfs(bus.gtfs.file)

metro.gtfs.file <- 'data/gtfs/metrofor.baseline.gtfs.zip'
metro.gtfs <- read_gtfs(metro.gtfs.file)

# lu data ----
for.data <-
  read_grid(city = 'Fortaleza') %>%
  left_join(read_landuse(city = 'Fortaleza'))

origins <-
  for.data %>% 
  mutate(pop = P013 + P014 + P015) %>% # 19 a 69 anos (?)
  filter(R003 <= 6 & !pop == 0) %>% # até o 6 decil de renda
  select(id = id_hex) %>% 
  st_centroid()

destinations <-
  for.data %>% 
  filter(T002 + T003 > 0) %>% # empregos de baixa e media escolaridade
  select(id = id_hex) %>% 
  st_centroid()

shp.bairro <- read_sf('data/bairros', crs = 4326) %>%
  select(geometry)

ggplot() +
  geom_sf(data = shp.bairro) +
  geom_sf(data = filter(for.data, id_hex %in% destinations$id),
          aes(fill = T002 + T003),
          color = NA) +
  #geom_sf(data = GTFSwizard::as_shapes_sf(gtfs$shapes), color = 'black', linewidth = .25) +
  viridis::scale_fill_viridis(option = 'H') +
  theme_gtfswizard +
  theme(legend.position = 'none') +
  ggspatial::annotation_scale(location = "bl", text_cex = 0.8, bar_cols = c("#333333", 'white')) +
  ggspatial::annotation_north_arrow(location = "tr",
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c('white', '#333333'),
                                                                                      line_col = '#333333',
                                                                                      text_col = '#333333'))

ggplot() +
  geom_sf(data = shp.bairro) +
  geom_sf(data = filter(for.data,
                        id_hex %in% origins$id), aes(fill = P013 + P014 + P015), color = NA) +
  #geom_sf(data = GTFSwizard::as_shapes_sf(gtfs$shapes), color = 'black', linewidth = .25) +
  viridis::scale_fill_viridis(option = 'H') +
  theme_gtfswizard +
  theme(legend.position = 'none') +
  ggspatial::annotation_scale(location = "bl", text_cex = 0.8, bar_cols = c("#333333", 'white')) +
  ggspatial::annotation_north_arrow(location = "tr",
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c('white', '#333333'),
                                                                                      line_col = '#333333',
                                                                                      text_col = '#333333'))

# TTM ----
list.files('data/r5rcore', 'zip', full.names = T) %>% 
  unlink()

final.gtfs <- 
  merge_gtfs(bus.gtfs, metro.gtfs)

plot(final.gtfs)

explore_gtfs(final.gtfs)

final.gtfs

write_gtfs(final.gtfs, 'data/r5rcore/gtfs.zip')
r5rcore <- setup_r5('data/r5rcore', overwrite = TRUE)

final.gtfs$calendar$end_date # data da simulacao

ttm <-
  travel_time_matrix(
    r5r_core = r5rcore,
    origins = origins,
    destinations = destinations,
    progress = TRUE,
    mode = 'TRANSIT',
    departure_datetime = dmy_hms("13/12/2021 06:30:00"),
    time_window = 60,
    percentiles = 50, # decidir com moraes e justificar
    max_walk_time = 15,
    max_trip_duration = 120,
    draws_per_minute = 1
  ) %>% 
  tibble %>% 
  setNames(c('from_id', 'to_id', 'travel_time'))

data.table::fwrite(ttm, 'data/performance/ttm/ttm.baseline.csv')
  
# baseline accessibility ----
od <- 
  tibble(from_id = origins$id) %>% 
  group_by(from_id) %>% 
  reframe(to_id = destinations$id) %>% 
  filter(!from_id == to_id)

sigma <- 50.95931 # 50% dos empregos alcançáveis em 60 minutos
rvmethod::gaussfunc(60, 0, 50.95931)

accessibility <- 
  left_join(od, ttm) %>% 
  mutate(travel_time = if_else(is.na(travel_time), Inf, travel_time)) %>% 
  filter(!from_id == to_id) %>% 
  rename(id_hex = to_id) %>% 
  left_join(for.data) %>% 
  group_by(from_id, travel_time) %>% 
  reframe(jobs = sum(T002) + sum(T003)) %>%
  mutate(dec.jobs = jobs * rvmethod::gaussfunc(travel_time, 0, sigma)) %>%
  rename(id_hex = from_id) %>% 
  left_join(for.data) %>% 
  mutate(pop = P013 + P014 + P015) %>% 
  group_by(id_hex, pop) %>% 
  reframe(cum.jobs = sum(dec.jobs)) %>% 
  left_join(for.data %>% select(id_hex, geom)) %>%
  st_as_sf()

ggplot() +
  geom_sf(data = shp.bairro) +
  geom_sf(data = accessibility, aes(fill = cum.jobs), color = NA) +
  viridis::scale_fill_viridis(option = 'F') +
  theme_gtfswizard +
  #theme(legend.position = 'none') +
  ggspatial::annotation_scale(location = "bl", text_cex = 0.8, bar_cols = c("#333333", 'white')) +
  ggspatial::annotation_north_arrow(location = "tr",
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c('white', '#333333'),
                                                                                      line_col = '#333333',
                                                                                      text_col = '#333333'))
ggsave('figs/baseline.scenario.png')

# critical accessibility ----
critical.access <-
  rep(accessibility$cum.jobs, accessibility$pop) %>%
  quantile(., .5)

ggplot() +
  geom_sf(data = shp.bairro) +
  geom_sf(data = accessibility, aes(fill = 'non-critical zones'), color = NA) +
  geom_sf(data = accessibility %>% filter(cum.jobs <= critical.access), aes(fill = 'critical zones'), color = NA) +
  #geom_sf(data = GTFSwizard::as_shapes_sf(gtfs$shapes), color = 'black', linewidth = .25) +
  scale_fill_manual(values = c('firebrick', '#444444')) +
  theme_gtfswizard +
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  labs(title = 'Baseline Scenario') +
  ggspatial::annotation_scale(location = "bl", text_cex = 0.8, bar_cols = c("#333333", NA)) +
  ggspatial::annotation_north_arrow(location = "tr", style = ggspatial::north_arrow_fancy_orienteering(fill = c(NA, '#333333'), line_col = '#333333', text_col = '#333333'))

tibble(
  scenario = 'baseline',
  people.critical = accessibility %>% filter(cum.jobs <= critical.access) %>% 
    .$pop %>% 
    sum,
  max.bus.fleet = GTFSwizard::get_fleet(bus.gtfs, method = 'by.hour') %>% # adicionar filter_time
    .$fleet %>% 
    max,
  total.bus.distance = GTFSwizard::get_distances(bus.gtfs, method = 'by.route') %>% # adicionar filter_time
    reframe(total.distance = trips * average.distance) %>% 
    .$total.distance %>% 
    sum,
  max.metro.fleet = GTFSwizard::get_fleet(metro.gtfs, method = 'by.hour') %>% # adicionar filter_time
    .$fleet %>% 
    max,
  total.metro.distance = GTFSwizard::get_distances(metro.gtfs, method = 'by.route') %>% # adicionar filter_time
    reframe(total.distance = trips * average.distance) %>% 
    .$total.distance %>% 
    sum
) %>% 
  write_csv('data/performance/baseline.scenario.performance.csv')

critical.access %>% 
  write_lines('data/critical.access.txt')
