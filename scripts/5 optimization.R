# Universidade Federal do Ceará
# Programa de Pós-Graduação em Engenharia de Transportes
# Nelson de O. Quesado Filho
# Julho de 2024
rm(list = ls()); gc()

# preambulo ----
#remotes::install_github('OPATP/GTFSwizard', force = T, update = 'always')
library(tidyverse)
#library(tidylog)

library(GTFSwizard)
library(aopdata)
options(java.parameters = '-Xmx2G')
library(r5r)

library(GA)

rstudioapi::getActiveDocumentContext() %>% 
  .$path %>% 
  dirname() %>% 
  setwd(); setwd('..')

# data ----
bl.psfr.prfmnc <- 
  data.table::fread('data/performance/pasfor.performance.csv') %>% 
  bind_rows(data.table::fread('data/performance/baseline.performance.csv'), .) %>% 
  tibble

baseline.gtfs <- 
  read_gtfs('data/gtfs/baseline.gtfs.zip')

trip.distances <- 
  get_distances(baseline.gtfs, method = 'by.trip') %>% 
  select(route_id, trip_id, distance)

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

# funcao objetivo (minimizar, restrio) ----
# x <- 
#   readRDS('data/solutions/test2.rds') %>% 
#   .@solution %>%
  # as.numeric()

critical.access <-
  read_lines('data/critical.access.txt') %>% 
  as.numeric()  

od <- 
  tibble(from_id = origins$id) %>% 
    group_by(from_id) %>% 
    reframe(to_id = destinations$id) %>% 
    filter(!from_id == to_id)

# fitness_function(x)

fitness_function <- function(x) {
  proposal <- 
    trip.distances[if_else(x == 1, T, F),]
  
  if(as.numeric(sum(proposal$distance)) > bl.psfr.prfmnc$total.distance[2]) {
    
    accessibility <- Inf
    
  } else {
    
    proprosed.gtfs <- 
      filter_trip(baseline.gtfs, trip = proposal$trip_id, keep = TRUE)
    
    write_gtfs(proprosed.gtfs, 'data/r5rcore/gtfs.zip')
    
    r5rcore <- setup_r5('data/r5rcore', overwrite = TRUE)
    
    ttm <-
      travel_time_matrix(
        r5r_core = r5rcore,
        origins = origins,
        destinations = destinations,
        progress = FALSE,
        mode = 'TRANSIT',
        departure_datetime = dmy_hms("13/12/2021 06:30:00"),
        time_window = 60,
        percentiles = 50, # decidir com moraes e justificar
        max_walk_time = 15,
        max_trip_duration = 120,
        n_threads = 3,
        draws_per_minute = 1
      ) %>% 
      tibble %>% 
      setNames(c('from_id', 'to_id', 'travel_time'))
    
    r5r::stop_r5()
    
    accessibility <- 
      full_join(od, ttm) %>% 
      mutate(travel_time = if_else(is.na(travel_time), Inf, travel_time)) %>% 
      filter(!from_id == to_id) %>% 
      rename(id_hex = to_id) %>% 
      left_join(for.data) %>% 
      group_by(from_id, travel_time) %>% 
      reframe(jobs = sum(T002) + sum(T003)) %>%
      mutate(dec.jobs = jobs / (travel_time)) %>%
      rename(id_hex = from_id) %>% 
      left_join(for.data) %>% 
      mutate(pop = P013 + P014 + P015) %>% 
      group_by(id_hex, pop) %>% 
      reframe(cum.jobs = sum(dec.jobs)) %>% 
      filter(cum.jobs <= critical.access) %>% 
      .$pop %>% 
      sum
    
    gc()
    
  }
  
  return(accessibility * -1)
  
}

# fitness_function(x)
# Inf*-1

# ga ----
initial_guess <- 
  readRDS('data/solutions/test9.rds') %>%
  .@population

# initial_guess2 <-
#   rbind(initial_guess, trip.distances$trip_id %in% trips.min.iv %>% as.numeric())

# fitness_function(initial_guess2[51,])

#readRDS('data/solutions/test9.rds') %>% .@fitnessValue

solution <-
  ga(type = "binary",
     fitness = fitness_function,
     nBits = nrow(trip.distances), 
     popSize = 51,
     maxiter = 4,
     pmutation = .1,
     pcrossover = .8,
     elitism = 11,
     #optim = TRUE,
     #run = 100, # criterio para parar antes do final
     #keepBest = TRUE,
     suggestions = initial_guess,
     names = trip.distances$trip_id
  )

solution %>%
  write_rds('data/solutions/test10.rds')

solution@solution
solution@fitnessValue
bl.psfr.prfmnc

# peneira ----
peneira <- 
  lapply(initial_guess, fitness_function)

# resolver o problema no braço ####
budget <- 
  bl.psfr.prfmnc[2, 4] %>% unlist

# min iv
min.iv <- group_by(trip.distances, route_id) %>% mutate(n = 1:n()) %>% filter(n %in% 1:2)

budget.min.iv <- budget - as.numeric(sum(min.iv$distance))

trips.min.iv <- 
  trip.distances %>% 
  filter(!trip_id %in% min.iv$trip_id) %>% 
  arrange(-distance) %>% 
  mutate(cost = cumsum(as.numeric(distance)),
         keep = cost <= budget.min.iv) %>% 
    filter(keep == T) %>% 
    .$trip_id %>% 
    c(., min.iv$trip_id)
    
# min
min <- group_by(trip.distances, route_id) %>% mutate(n = 1:n()) %>% filter(n == 1)
budget.min <- budget - as.numeric(sum(min$distance))

# testando se alguma zona saiu 
origins$id %in% ttm$from_id %>% sum() - length(origins$id)