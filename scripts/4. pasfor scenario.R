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
bus.gtfs.file <- 'data/gtfs/pasfor.gtfs.zip'
bus.gtfs <- read_gtfs(bus.gtfs.file)

metro.gtfs.file <- 'data/gtfs/metrofor.future.gtfs.zip'
metro.gtfs <- read_gtfs(metro.gtfs.file) #%>% get_shapes()

# lu data ----
shp.bairro <- read_sf('data/bairros', crs = 4326) %>%
  select(geometry)

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

# TTM ----
list.files('data/r5rcore', 'zip', full.names = T) %>% 
  unlink()

write_gtfs(bus.gtfs, 'data/r5rcore/bus.gtfs.zip')
write_gtfs(metro.gtfs, 'data/r5rcore/metro.gtfs.zip')

r5rcore <- setup_r5('data/r5rcore', overwrite = TRUE)

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

data.table::fwrite(ttm, 'data/performance/ttm/ttm.pasfor.csv')
  
# do nothing accessibility ----
od <- 
  tibble(from_id = origins$id) %>% 
  group_by(from_id) %>% 
  reframe(to_id = destinations$id) %>% 
  filter(!from_id == to_id)

sigma <- 50.95931 # 50% dos empregos alcançáveis em 60 minutos

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

# critical accessibility ----
critical.access <-
  read_lines('data/critical.access.txt') %>% 
  as.numeric()

ggplot() +
  geom_sf(data = shp.bairro) +
  geom_sf(data = accessibility, aes(fill = 'non-critical zones'), color = NA) +
  geom_sf(data = accessibility %>% filter(cum.jobs <= critical.access), aes(fill = 'critical zones'), color = NA) +
  #geom_sf(data = GTFSwizard::as_shapes_sf(gtfs$shapes), color = 'black', linewidth = .25) +
  scale_fill_manual(values = c('firebrick', '#444444')) +
  theme_gtfswizard +
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  labs(title = 'Pasfor Scenario') +
  ggspatial::annotation_scale(location = "bl", text_cex = 0.8, bar_cols = c("#333333", NA)) +
  ggspatial::annotation_north_arrow(location = "tr", style = ggspatial::north_arrow_fancy_orienteering(fill = c(NA, '#333333'), line_col = '#333333', text_col = '#333333'))
ggsave('figs/pasfors.scenario.png')

tibble(
  scenario = 'pasfor',
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
  write_csv('data/performance/pasfor.scenario.performance.csv')

# intervention ----
pasfor.int <-
  tribble(
  ~route_id, ~Parecer,
  4, "Eliminada",
  16, "Eliminada",
  17, "Eliminada",
  19, "Seccionada",
  21, "Seccionada",
  22, "Eliminada",
  25, "Seccionada",
  27, "Eliminada",
  29, "Eliminada",
  30, "Seccionada",
  38, "Seccionada",
  41, "Eliminada",
  50, "Eliminada",
  53, "Eliminada",
  67, "Eliminada",
  68, "Eliminada",
  69, "Eliminada",
  70, "Seccionada",
  72, "Eliminada",
  76, "Seccionada",
  77, "Seccionada",
  78, "Eliminada",
  79, "Eliminada",
  130, "Eliminada",
  132, "Eliminada",
  138, "Eliminada",
  140, "Eliminada",
  141, "Eliminada",
  145, "Eliminada",
  150, "Eliminada",
  177, "Eliminada",
  182, "Eliminada",
  186, "Eliminada",
  215, "Eliminada",
  220, "Eliminada",
  226, "Eliminada",
  244, "Eliminada",
  286, "Eliminada",
  300, "Eliminada",
  302, "Seccionada",
  303, "Seccionada",
  304, "Eliminada",
  305, "Seccionada",
  307, "Seccionada",
  308, "Seccionada",
  310, "Seccionada",
  365, "Eliminada",
  368, "Eliminada",
  369, "Seccionada",
  371, "Eliminada",
  374, "Eliminada",
  375, "Seccionada",
  377, "Seccionada",
  384, "Seccionada",
  387, "Eliminada",
  388, "Eliminada",
  392, "Seccionada",
  395, "Seccionada",
  405, "Seccionada",
  406, "Eliminada",
  411, "Eliminada",
  421, "Eliminada",
  456, "Seccionada",
  466, "Seccionada",
  513, "Eliminada",
  600, "Eliminada",
  601, "Seccionada",
  602, "Eliminada",
  604, "Seccionada",
  706, "Seccionada",
  709, "Eliminada",
  710, "Seccionada",
  711, "Eliminada",
  712, "Eliminada",
  713, "Seccionada",
  725, "Eliminada",
  728, "Seccionada",
  752, "Eliminada",
  753, "Eliminada",
  754, "Seccionada",
  755, "Eliminada",
  757, "Eliminada",
  762, "Eliminada",
  806, "Seccionada",
  813, "Eliminada",
  816, "Eliminada",
  820, "Eliminada",
  823, "Eliminada",
  831, "Eliminada",
  833, "Seccionada",
  901, "Eliminada",
  905, "Seccionada",
  80, "Eliminada",
  82, "Eliminada",
  84, "Eliminada",
  86, "Eliminada",
  87, "Eliminada",
  89, "Eliminada",
  91, "Eliminada",
  92, "Eliminada",
  93, "Eliminada",
  94, "Eliminada",
  96, "Seccionada",
  97, "Eliminada",
  98, "Eliminada",
  101, "Eliminada",
  106, "Seccionada",
  111, "Seccionada",
  112, "Seccionada",
  114, "Seccionada",
  115, "Seccionada",
  120, "Seccionada",
  127, "Eliminada",
  129, "Eliminada",
  605, "Seccionada",
  609, "Eliminada",
  610, "Seccionada",
  611, "Seccionada",
  612, "Seccionada",
  613, "Seccionada",
  625, "Seccionada",
  627, "Seccionada",
  633, "Seccionada",
  634, "Eliminada",
  640, "Seccionada",
  644, "Eliminada",
  649, "Seccionada",
  656, "Eliminada",
  660, "Seccionada",
  663, "Eliminada",
  666, "Seccionada",
  668, "Eliminada",
  670, "Eliminada",
  685, "Eliminada",
  690, "Eliminada",
  703, "Seccionada",
  907, "Eliminada",
  909, "Eliminada",
  916, "Eliminada",
  917, "Eliminada",
  988, "Nova",
  989, "Nova",
  990, "Nova",
  991, "Nova",
  992, "Nova",
  993, "Nova",
  994, "Nova",
  995, "Nova",
  996, "Nova",
  997, "Nova",
  998, "Nova",
  999, "Nova")

bus.baseline.gtfs <- read_gtfs( 'data/gtfs/baseline.gtfs.zip')

bus.baseline.gtfs$trip %>% 
  select(shape_id, route_id) %>% 
  group_by(route_id) %>% 
  reframe(shape_id = shape_id[1]) %>% 
  left_join(
    bus.baseline.gtfs$shapes %>% as_shapes_sf()
  ) %>% 
  mutate(route_id = route_id %>% as.double()) %>% 
  left_join(pasfor.int) %>% 
  filter(Parecer == 'Eliminada') %>% 
  bind_rows(
    bus.gtfs$trip %>% 
      select(shape_id, route_id) %>% 
      group_by(route_id) %>% 
      reframe(shape_id = shape_id[1]) %>% 
      left_join(
        bus.gtfs$shapes %>% as_shapes_sf()
      ) %>% 
      mutate(route_id = route_id %>% as.double()) %>% 
      left_join(pasfor.int) %>% 
      mutate(Parecer = if_else(is.na(Parecer), 'Mantida', Parecer))
  ) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(data = shp.bairro, fill = NA) +
  geom_sf(aes(color = Parecer)) +
  geom_sf(data = as_shapes_sf(metro.gtfs$shapes)) +
  facet_wrap(ncol = 1, .~Parecer) +
  theme_linedraw() +
  theme(legend.position = 'bottom') 
ggsave('figs/pasfor.int.png', width = 9, height = 10)
