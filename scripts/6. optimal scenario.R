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
  ggplot2::theme(title = ggplot2::element_text(size = 11, color = '#283c42'),
                 axis.title.x = ggplot2::element_text(size = 11, color = '#283c42'),
        axis.title.y = ggplot2::element_text(size = 11, color = '#283c42'),
        axis.text.x = ggplot2::element_text(size = 10),
        axis.text.y = ggplot2::element_text(size = 10),
        legend.text = ggplot2::element_text(size = 10))

# ts data ----
bus.gtfs.file <- 'data/gtfs/proposition.gtfs.zip'
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

data.table::fwrite(ttm, 'data/performance/ttm/ttm.optimal.csv')
  
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
  labs(title = 'Optimal Scenario') +
  ggspatial::annotation_scale(location = "bl", text_cex = 0.8, bar_cols = c("#333333", NA)) +
  ggspatial::annotation_north_arrow(location = "tr", style = ggspatial::north_arrow_fancy_orienteering(fill = c(NA, '#333333'), line_col = '#333333', text_col = '#333333'))
ggsave('figs/optimal.scenario.png')

tibble(
  scenario = 'optimal',
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
  write_csv('data/performance/optimal.scenario.performance.csv')

# intervention ----
bus.baseline.gtfs <- read_gtfs( 'data/gtfs/baseline.gtfs.zip')

get_frequency(bus.baseline.gtfs) %>% 
  rename(bl.freq = daily.frequency) %>% 
  left_join(
    get_frequency(bus.gtfs) %>% 
      rename(opt.freq = daily.frequency)
  ) %>% 
  mutate(reduction = (bl.freq - opt.freq)/bl.freq) %>% 
  left_join(
    as_shapes_sf(bus.baseline.gtfs)$shapes %>% 
      left_join(
        bus.baseline.gtfs$trips %>% 
          select(route_id, shape_id) %>% 
          group_by(route_id) %>% 
          reframe(shape_id = shape_id[1]) %>% 
          distinct()
      )
  ) %>% 
  st_as_sf() %>%
  mutate(reduction = if_else(reduction == 0, 'Mantida', 'Reduzida')) %>% 
  ggplot() +
  geom_sf(data = shp.bairro, fill = 'white') +
  geom_sf(aes(color = reduction)) +
  geom_sf(data = as_shapes_sf(metro.gtfs$shapes)) +
  facet_wrap(ncol = 1, .~reduction) +
  theme_linedraw() +
  theme(legend.position = 'bottom', legend.title = element_blank()) 
ggsave('figs/optimal.int.1.png', width = 9, height = 10)

gradient <- 
  get_frequency(bus.baseline.gtfs) %>% 
  rename(bl.freq = daily.frequency) %>% 
  left_join(
    get_frequency(bus.gtfs) %>% 
      rename(opt.freq = daily.frequency)
  ) %>% 
  mutate(reduction = (bl.freq - opt.freq)/bl.freq) %>% 
  left_join(
    as_shapes_sf(bus.baseline.gtfs)$shapes %>% 
      left_join(
        bus.baseline.gtfs$trips %>% 
          select(route_id, shape_id) %>% 
          group_by(route_id) %>% 
          reframe(shape_id = shape_id[1]) %>% 
          distinct()
      )
  ) %>% 
  st_as_sf()
  

ggplot() +
  geom_sf(data = shp.bairro, fill = 'white') +
  geom_sf(data = filter(gradient, reduction == 0), color = 'gray85') +
  geom_sf(data = as_shapes_sf(metro.gtfs$shapes) %>% filter(!shape_id %in% c('shape-7', 'shape-8', 'shape-10'))) +
  geom_sf(data = filter(gradient, !reduction == 0), aes(color = reduction, linetype = 'Metro\nSysrem'), alpha = .75) +
  scale_color_gradient(low = 'gray80', high = 'red', breaks = c(.1, .5, .9), labels = c('10%', '50%', '90%')) +
  theme_gtfswizard +
  labs(title = 'Impacted routes', color = 'Proportion\nof trips\nreduced', linetype = '')
ggsave('figs/optimal.int.2.png', dpi = 320, scale = .75)

gradient %>% 
  ggplot() +
  geom_histogram(aes(x = reduction), fill = 'firebrick') +
  scale_x_percent(limits = c(.01, 1)) +
  geom_vline(aes(xintercept = .206, linetype = 'Average Trip\nReduction')) +
  theme_gtfswizard +
  theme(legend.title = element_blank()) +
  scale_linetype_manual(values = 'dashed') +
  labs(x = 'Trip reduction', y = '# of Routes', title = 'Trip reduction bar plot')
ggsave('figs/optimal.trip.hist.png', dpi = 320, scale = .7, width = 12, height = 5)

set.seed(2);
gradient2 <-
  gradient$reduction %>% 
  #sort %>% 
  kmeans(centers = 3) %>% 
  .$cluster %>% 
  bind_cols(gradient, cluster = .)

gradient2 %>% 
  filter(reduction == 0) %>% 
  group_by(cluster) %>% 
  reframe(bl.freq = sum(bl.freq),
          opt.freq = sum(opt.freq),
          trip.red = bl.freq - opt.freq,
          n = n(),
          min.red = min(reduction),
          mean.red = mean(reduction),
          max.red = max(reduction)) %>% 
  kableExtra::kbl() %>% 
  kableExtra::kable_classic()

gradient2 %>% 
  reframe(bl.freq = sum(bl.freq),
          opt.freq = sum(opt.freq),
          trip.red = bl.freq - opt.freq,
          n = n(),
          min.red = min(reduction),
          mean.red = mean(reduction),
          max.red = max(reduction)) %>% 
  kableExtra::kbl() %>% 
  kableExtra::kable_classic()
