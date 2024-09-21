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
options(java.parameters = '-Xmx2G')
library(r5r)

library(parallel)

rstudioapi::getActiveDocumentContext() %>% 
  .$path %>% 
  dirname() %>% 
  setwd(); setwd('..')

# ts data ----
gtfs.files <-
  list.files('data/gtfs/modelos.reducao.trip/', '.zip', full.names = TRUE)

# cost ----
fwrite <- 
  data.table::fwrite

# cost.proxy <-
#   function(x) {
#     
#     gtfs <- 
#       read_gtfs(x)
#     
#     dat <- 
#       tibble(
#         scenario = paste(x),
#         max.fleet = GTFSwizard::get_fleet(gtfs, method = 'by.hour') %>%
#           .$fleet %>% 
#           max,
#         total.distance = GTFSwizard::get_distances(gtfs, method = 'by.route') %>%
#           reframe(total.distance = trips * average.distance) %>% 
#           .$total.distance %>% 
#           sum 
#       )
#     
#     fwrite(dat, file = paste0(str_remove(x, 'gtfs.zip'), 'prfmnc.csv'))
#     
#     gc()
#     
#   }

cost.proxy <- function(x) {
  gtfs <- tryCatch({
    read_gtfs(x)
  }, error = function(e) {
    message("Error reading GTFS file")
    return(NULL)  # Return NULL if there is an error in reading the GTFS file
  })
  
  if (is.null(gtfs)) {
    return(NULL)  # Skip processing if gtfs is NULL due to an error in read_gtfs
  }
  
  dat <- 
    tibble(
      scenario = paste(x),
      max.fleet = GTFSwizard::get_fleet(gtfs, method = 'by.hour') %>%
        .$fleet %>% 
        max,
      total.distance = GTFSwizard::get_distances(gtfs, method = 'by.route') %>%
        reframe(total.distance = trips * average.distance) %>% 
        .$total.distance %>% 
        sum 
    )
  
  fwrite(dat, file = paste0(str_remove(x, 'gtfs.zip'), 'prfmnc.csv'))
  
  gc()
}

gtfs.done <- 
  list.files('data/gtfs/modelos.reducao.trip/', '.csv', full.names = TRUE) %>% 
  str_replace('.prfmnc.csv', '.gtfs.zip')

gtfs.remain <-
  gtfs.files[!gtfs.files %in% gtfs.done]

cl <-
  makeCluster(detectCores() - 1)

clusterExport(cl, c("gtfs.remain", "cost.proxy", 'read_gtfs', 'get_fleet', 'get_distances', 'fwrite')) # Export necessary variables and functions to the cluster

costs <- 
  parLapply(cl, gtfs.remain, cost.proxy) # Use parLapply to apply the function in parallel

stopCluster(cl) # Stop the cluster

gc()

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

rm(for.data)

# ttm ----
gtfs.files <-
  list.files('data/gtfs/modelos.reducao.trip/', '.zip', full.names = TRUE)

datetime <- dmy_hms("13/12/2021 06:30:00")

baseline.ttm <- 
  read_csv('data/performance/ttm.baseline.csv') %>% 
  rename(baseline.ttm = travel_time)

save.ttm <-
  function(x) {
    
    file.copy(x, to = 'data/r5rcore/gtfs.zip', overwrite = TRUE)
    r5rcore <- setup_r5('data/r5rcore', overwrite = TRUE)
    
    ttm <-
      travel_time_matrix(
        r5r_core = r5rcore,
        origins = origins,
        destinations = destinations,
        progress = FALSE,
        mode = 'TRANSIT',
        departure_datetime = datetime,
        time_window = 60,
        percentiles = 50, # decidir com moraes e justificar
        max_walk_time = 15,
        max_trip_duration = 120,
        draws_per_minute = 1
      )
    
    dif.ttm <- 
      left_join(ttm, baseline.ttm) %>% 
      mutate(diff.ttm = travel_time_p50 - baseline.ttm) %>% 
      filter(!diff.ttm == 0)
    
    data.table::fwrite(dif.ttm, file = paste0(str_remove(x, 'gtfs.zip'), 'ttm.csv'))
    
    gc()
    
}

lapply(gtfs.files[1:2], save.ttm)


# performance together ----
csv.files <-
  list.files('data/gtfs/modelos.reducao.trip/', '.csv', full.names = TRUE)

dat <- 
  lapply(csv.files, data.table::fread) %>% 
  unlist %>% 
  tibble(values = .,
         names = rep(c('gtfs', 'fleet', 'length'), times = 11783)) %>% 
  pivot_wider(names_from = names,
              values_from = values,
              values_fn = list) %>% 
  unnest(cols = c('gtfs', 'fleet', 'length')) %>% 
  mutate_at(2:3, as.numeric)

dat  