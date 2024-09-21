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

rstudioapi::getActiveDocumentContext() %>% 
  .$path %>% 
  dirname() %>% 
  setwd(); setwd('..')

# gtfs filter regular bus baseline ----
baseline.full.gtfs <- 
  read_gtfs('data/gtfs/baseline.full.gtfs.zip')

explore_gtfs(baseline.full.gtfs)

plot_calendar(baseline.full.gtfs)

filter_date(baseline.full.gtfs) %>%
  explore_gtfs()

filter_date(baseline.full.gtfs) %>%
  get_frequency(method = 'detailed') %>%
  group_by(hour) %>%
  reframe(average.frequency = mean(frequency)) %>%
  arrange(-average.frequency) %>%
  filter(hour <= 10)

filter_date(baseline.full.gtfs) %>%
  filter_time(from = '06:00:00', to = '10:59:59') %>%
   explore_gtfs()

baseline.gtfs <-
  baseline.full.gtfs %>%
  GTFSwizard::filter_date() %>%
  GTFSwizard::filter_time(from = '06:00:00', to = '10:59:59')

# baseline.gtfs <- 
#   baseline.full.gtfs %>% 
#   GTFSwizard::filter_date() %>% 
#   GTFSwizard::filter_time(from = '06:30:00', to = '09:30:30')

baseline.gtfs %>% 
  GTFSwizard::write_gtfs('data/gtfs/baseline.gtfs.zip')

# gtfs filter pasfor ----
pasfor.full.gtfs <- 
  read_gtfs('data/gtfs/pasfor.full.gtfs.zip')

explore_gtfs(pasfor.full.gtfs)

plot_calendar(pasfor.full.gtfs)

filter_date(pasfor.full.gtfs) %>% 
  explore_gtfs()

filter_date(pasfor.full.gtfs) %>% 
  get_frequency(method = 'detailed') %>% 
  group_by(hour) %>% 
  reframe(average.frequency = mean(frequency)) %>% 
  arrange(-average.frequency) %>% 
  filter(hour <= 10)

filter_date(pasfor.full.gtfs) %>% 
  filter_time(from = '06:00:00', to = '10:59:59') %>% 
  explore_gtfs()

pasfor.gtfs <-
  pasfor.full.gtfs %>%
  filter_date() %>%
  filter_time(from = '06:00:00', to = '10:59:59')

# pasfor.gtfs <- 
#   pasfor.full.gtfs %>% 
#   filter_date() %>% 
#   filter_time(from = '06:30:00', to = '09:00:00')

pasfor.gtfs %>% 
  write_gtfs('data/gtfs/pasfor.gtfs.zip')

# gtfs filter metro baseline ----
metrofor.baseline.full.gtfs <- 
  read_gtfs('data/gtfs/metrofor.baseline.full.gtfs.zip') 

explore_gtfs(metrofor.baseline.full.gtfs)

plot_calendar(metrofor.baseline.full.gtfs)

filter_date(metrofor.baseline.full.gtfs, date = "2021-12-13") %>%
  explore_gtfs()

filter_date(metrofor.baseline.full.gtfs, date = "2021-12-13") %>% 
  get_frequency(method = 'detailed') %>% 
  group_by(hour) %>% 
  reframe(average.frequency = mean(frequency)) %>% 
  arrange(-average.frequency) %>% 
  filter(hour <= 10)

filter_date(metrofor.baseline.full.gtfs, date = "2021-12-13") %>% 
  filter_time(from = '06:00:00', to = '10:59:59') %>% 
  explore_gtfs()

metrofor.baseline.gtfs <-
  metrofor.baseline.full.gtfs %>%
  filter_date(date = "2021-12-13") %>%
  filter_time(from = '06:00:00', to = '10:59:59')

metrofor.baseline.gtfs %>%
  write_gtfs('data/gtfs/metrofor.baseline.gtfs.zip')

# gtfs filter metro future ----
metrofor.future.full.gtfs <- 
  read_gtfs('data/gtfs/metrofor.future.full.gtfs.zip') %>% 
  get_shapes()

explore_gtfs(metrofor.future.full.gtfs)

plot_calendar(metrofor.future.full.gtfs)

filter_date(metrofor.future.full.gtfs, date = "2021-12-13") %>%
  explore_gtfs()

filter_date(metrofor.future.full.gtfs, date = "2021-12-13") %>% 
  get_frequency(method = 'detailed') %>% 
  group_by(hour) %>% 
  reframe(average.frequency = mean(frequency)) %>% 
  arrange(-average.frequency) %>% 
  filter(hour <= 10)

filter_date(metrofor.future.full.gtfs, date = "2021-12-13") %>% 
  filter_time(from = '06:00:00', to = '10:59:59') %>% 
  explore_gtfs()

metrofor.future.gtfs <-
  metrofor.future.full.gtfs %>%
  filter_date(date = "2021-12-13") %>%
  filter_time(from = '06:00:00', to = '10:59:59')

metrofor.future.gtfs %>%
  write_gtfs('data/gtfs/metrofor.future.gtfs.zip')
