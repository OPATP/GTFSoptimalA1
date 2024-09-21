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

# ts data ----
gtfs.file <- 'data/gtfs/baseline.gtfs.zip'

gtfs <- GTFSwizard::read_gtfs(gtfs.file)

# by parLapply ----
library(parallel)

trips <- gtfs$trips$trip_id

filter.n.save <-
  function(x) {
    
    new.gtfs <- 
      filter_trip(gtfs = gtfs, trip = x, keep = FALSE)
    
    new.gtfs %>% 
      write_gtfs(paste0('data/gtfs/modelos.reducao.trip/', x, '.gtfs.zip'))
    
    gc()
    
  }

trips.done <- 
  list.files('data/gtfs/modelos.reducao.trip/', '.zip') %>% 
  str_remove('.gtfs.zip')

trips.remain <-
  trips[!trips %in% trips.done]

# trips.remain <- str_remove_all(gtfs.remain, 'data/gtfs/modelos.reducao.trip//|.gtfs.zip')

write_gtfs <-
  write_gtfs

cl <-
  makeCluster(detectCores() - 3)

clusterExport(cl, c("trips.remain", "filter.n.save", "gtfs", "filter_trip", 'write_gtfs')) # Export necessary variables and functions to the cluster

parLapply(cl, trips.remain, filter.n.save) # Use parLapply to apply the function in parallel

stopCluster(cl) # Stop the cluster

gc()
