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

od <- 
  tibble(from_id = origins$id) %>% 
  group_by(from_id) %>% 
  reframe(to_id = destinations$id) %>% 
  filter(!from_id == to_id)

# ts data ----
shp.metro.future <- 
  read_gtfs('data/gtfs/metrofor.future.gtfs.zip') %>% 
  .$shapes %>% 
  as_shapes_sf()
# restriction ----
performance.files <- list.files('data/performance/', 'performance.csv', full.names = T)

critical.access <-
  read_lines('data/critical.access.txt') %>% 
  as.numeric()

performance <- NULL

for (i in performance.files) {
  performance <- 
    performance %>% 
    bind_rows(read_csv(i))
}

performance %>% 
  kableExtra::kbl() %>% 
  kableExtra::kable_classic()

performance %>%
  mutate(scenario = str_replace(scenario, 'baseline', 'Baseline') %>% 
           str_replace('donothing', 'Do nothing') %>% 
           str_replace('optimal', 'Optimal') %>% 
           str_replace('pasfor', 'Pasfor')) %>% 
  ggplot() +
  geom_point(aes(people.critical, total.bus.distance/1000, color = scenario), size = 5) +
  theme_gtfswizard +
  hrbrthemes::scale_x_comma(big.mark = '.', limits = c(380000, 520000)) +
  hrbrthemes::scale_y_comma(big.mark = '.', limits = c(123000, 140000)) +
  labs(x = 'People under Critical Levels of Accessibility (individuals)', y = 'Total System Distance (km)', color = 'Scenarios') +
  theme_minimal()
#ggsave('figs/point.scenarios.png', scale = .75)

# accessibility distribution BASELINE ----
ttm.baseline <- 
  read_csv('data/performance/ttm/ttm.baseline.csv')

sigma <- 50.95931 # 50% dos empregos alcançáveis em 60 minutos

accessibility <- 
  left_join(od, ttm.baseline) %>% 
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
  reframe(cum.jobs.bl = sum(dec.jobs)) %>% 
  left_join(for.data %>% select(id_hex, geom))

# accessibility distribution DO NOTHING ----
ttm.donothing <- 
  read_csv('data/performance/ttm/ttm.donothing.csv')

accessibility <- 
  left_join(od, ttm.donothing) %>% 
  mutate(travel_time = if_else(is.na(travel_time), Inf, travel_time)) %>% 
  filter(!from_id == to_id) %>% 
  rename(id_hex = to_id) %>% 
  left_join(for.data) %>% 
  group_by(from_id, travel_time) %>% 
  reframe(jobs = sum(T002) + sum(T003)) %>%
  mutate(dec.jobs = jobs * rvmethod::gaussfunc(travel_time, 0, sigma)) %>%
  rename(id_hex = from_id) %>% 
  group_by(id_hex) %>% 
  reframe(cum.jobs.dn = sum(dec.jobs)) %>% 
  left_join(accessibility)


# accessibility distribution PASFOR ----
ttm.pasfor <- 
  read_csv('data/performance/ttm/ttm.pasfor.csv')

accessibility <- 
  left_join(od, ttm.pasfor) %>% 
  mutate(travel_time = if_else(is.na(travel_time), Inf, travel_time)) %>% 
  filter(!from_id == to_id) %>% 
  rename(id_hex = to_id) %>% 
  left_join(for.data) %>% 
  group_by(from_id, travel_time) %>% 
  reframe(jobs = sum(T002) + sum(T003)) %>%
  mutate(dec.jobs = jobs * rvmethod::gaussfunc(travel_time, 0, sigma)) %>%
  rename(id_hex = from_id) %>% 
  group_by(id_hex) %>% 
  reframe(cum.jobs.pf = sum(dec.jobs)) %>% 
  left_join(accessibility)

# accessibility distributino OPTIMAL ----
ttm.optimal <-
  data.table::fread('data/performance/ttm/ttm.optimal.csv')

accessibility <- 
  left_join(od, ttm.optimal) %>% 
  mutate(travel_time = if_else(is.na(travel_time), Inf, travel_time)) %>% 
  filter(!from_id == to_id) %>% 
  rename(id_hex = to_id) %>% 
  left_join(for.data) %>% 
  group_by(from_id, travel_time) %>% 
  reframe(jobs = sum(T002) + sum(T003)) %>%
  mutate(dec.jobs = jobs * rvmethod::gaussfunc(travel_time, 0, sigma)) %>%
  rename(id_hex = from_id) %>% 
  group_by(id_hex) %>% 
  reframe(cum.jobs.opt = sum(dec.jobs)) %>% 
  left_join(accessibility)

# comparing spatial impacts ----
accessibility %>% 
  mutate(cum.jobs.bl = if_else(cum.jobs.bl < critical.access, 'critical', 'not critical'),
         cum.jobs.dn = if_else(cum.jobs.dn < critical.access & cum.jobs.bl == 'critical', 'critical',
                               if_else(cum.jobs.dn < critical.access & cum.jobs.bl == 'not critical', 'new critical',
                                       if_else(cum.jobs.dn > critical.access & cum.jobs.bl == 'critical', 'new not critical',
                                               'not critical'))),
         cum.jobs.pf = if_else(cum.jobs.pf < critical.access & cum.jobs.bl == 'critical', 'critical',
                               if_else(cum.jobs.pf < critical.access & cum.jobs.bl == 'not critical', 'new critical',
                                       if_else(cum.jobs.pf > critical.access & cum.jobs.bl == 'critical', 'new not critical',
                                               'not critical'))),
         cum.jobs.opt = if_else(cum.jobs.opt < critical.access & cum.jobs.bl == 'critical', 'critical',
                               if_else(cum.jobs.opt < critical.access & cum.jobs.bl == 'not critical', 'new critical',
                                       if_else(cum.jobs.opt > critical.access & cum.jobs.bl == 'critical', 'new not critical',
                                               'not critical')))
         ) %>% 
  pivot_longer(cols = c(2, 3, 4), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor')) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(data = shp.bairro) +
  geom_sf(aes(fill = cum.jobs), color = NA) +
  geom_sf(data = . %>% filter(cum.jobs %in% c('new critical', 'new not critical')), aes(fill = cum.jobs), color = 'white') +
  geom_sf(data = shp.metro.future) +
  theme_linedraw() +
  theme(legend.title = element_blank(), legend.position = 'bottom') +
  scale_fill_manual(values = c('firebrick', 'red', 'green3', 'gray40')) +
  labs(title = 'Distribuição espacial do efeito da intervenção') +
  facet_wrap(scenario~., ncol = 3)
ggsave('figs/impact.spatial.distribution2.png', scale = 1.5, dpi = 600)

accessibility %>% 
  mutate_at(2:4, function(x){x - .$cum.jobs.bl}) %>% 
  pivot_longer(cols = c(2, 3, 4), names_to = 'scenario', values_to = 'cum.jobs') %>%
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor'),
         tot.access = cum.jobs * pop) %>% 
  #rstatix::identify_outliers(., variable = 'tot.access') %>% filter(is.extreme & tot.access > 0) %>% arrange(tot.access)
  mutate(tot.access = ifelse(tot.access > 13461493, 13461493, ifelse(tot.access < -13461493 , -13461493, tot.access))) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(data = shp.bairro) +
  geom_sf(aes(fill = tot.access), color = NA) +
  geom_sf(data = shp.metro.future) +
  theme_linedraw() +
  theme(legend.title = element_blank(), legend.position = 'none') +
  scale_fill_gradient2(low = 'firebrick', mid = 'white', high = 'green4', midpoint = 0,
                       limits = c(-13461493, 13461493)
                       ) +
  labs(title = 'Distribuição espacial do efeito', subtitle = 'Accessibilitty x Population') +
  facet_wrap(scenario~., ncol = 3)
ggsave('figs/impact.spatial.distribution4.png', scale = 1.75, dpi = 600)



# comparing acessibility distribution ----
population <- sum(accessibility$pop)

perc.label <- 
  performance %>%
  mutate(people.critical.perc = (people.critical/population * 100) %>%
           round(., 1) %>%
           paste0(., '%\n', format(people.critical, big.mark = "."), ' ind.')) %>% 
  #select(scenario, people.critical.perc, max.bus.fleet, total.bus.distance) %>% 
  mutate(fleet.norm = max.bus.fleet / max(max.bus.fleet),
           distance.norm = total.bus.distance / max(total.bus.distance)) %>% 
  mutate(scenario = c('Baseline', 'Do nothing', 'Optimal', 'Pasfor'))

tot.jobs <- 
  filter(for.data, id_hex %in% destinations$id) %>% 
  mutate(trab = .$T002 + .$T003) %>% 
  .$trab %>% 
  sum()


accessibility %>% 
  pivot_longer(cols = c(2, 3, 4, 6), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor') %>% str_replace('cum.jobs.bl', 'Baseline'),
         cum.jobs = cum.jobs/tot.jobs) %>% 
  ggplot +
  geom_histogram(aes(x = cum.jobs, weight = pop), bins = 60, fill = 'gray75') +
  geom_histogram(data = . %>% filter(cum.jobs < critical.access/tot.jobs), aes(x = cum.jobs, weight = pop, fill = 'Individuals\nin critical\naccessibility\nconditions'), bins = 60) +
  geom_vline(aes(color = paste0('Median\naccessibility\nof ', format(round(critical.access/tot.jobs*100, 1), big.mark = "."), '%'), xintercept = critical.access/tot.jobs), linetype = 'dashed', linewidth = 1) +
  geom_text(data = perc.label, aes(x = .38, y = 10000, label = people.critical.perc), color = 'white') +
  # geom_point(data = perc.label, aes(x = 2500, 40000, size = total.bus.distance/max(total.bus.distance)), color = 'blue') +
  # geom_text(data = perc.label, aes(x = 3500, 40000, label = paste0('Bus\nlength\n', format(round(total.bus.distance/1000), big.mark = '.'), 'km'))) +
  # geom_point(data = perc.label, aes(x = 12000, 40000, size = total.metro.distance/max(total.metro.distance)), color = 'green') +
  # geom_text(data = perc.label, aes(x = 13000, 40000, label = paste0('Metro\nlength\n', format(round(total.metro.distance/1000), big.mark = '.'), 'km'))) +
  facet_grid(scenario~.) +
  theme_linedraw() +
  labs(x = 'Accessibility (reachable)', y = 'Low-income individuals from 19 to 69 yo', color = '', fill = '') +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  scale_x_percent() +
  scale_fill_manual(values = 'red') +
  #scale_size(limits = c(.25, 1), guide = 'none') +
  scale_color_manual(values = 'black')
ggsave('figs/problem.distribution.png', width = 6.5, height = 8)

# CDF ----
accessibility %>% 
  pivot_longer(cols = c(2, 3, 4, 6), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  mutate(cum.jobs = cum.jobs/tot.jobs) %>% 
  group_by(scenario, cum.jobs) %>% 
  reframe(pop = sum(pop)) %>% 
  arrange(scenario, cum.jobs) %>% 
  group_by(scenario) %>% 
  mutate(cumpop = cumsum(pop)) %>%
  mutate(cumpop = cumpop/max(cumpop)) %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor') %>% str_replace('cum.jobs.bl', 'Baseline')) %>% 
  ggplot() +
  geom_line(aes(y = cum.jobs, x = cumpop, color = scenario), linewidth = .75) +
  theme_gtfswizard +
  scale_x_percent() +
  scale_y_percent() +
  labs(titble = 'CDF', y = 'Accessibility (reachable jobs)', x = 'Population (%)', color = '') +
  theme(legend.position = 'bottom')
ggsave('figs/cdf1.png', scale = .75)

# boxplot ----
for.data %>% 
  filter(id_hex %in% accessibility$id_hex) %>% 
  select(id_hex, R003) %>% 
  left_join(accessibility, .) %>% 
  mutate_at(2:4, function(x){x - .$cum.jobs.bl}) %>% 
  mutate(R003 = factor(R003, levels = 1:6)) %>% 
  pivot_longer(cols = c(2, 3, 4), names_to = 'scenario', values_to = 'cum.jobs') %>%
  mutate(cum.jobs = cum.jobs/tot.jobs) %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor')) %>% 
  ggplot() +
  geom_boxplot(aes(x = R003, y = cum.jobs, color = R003, group = R003), fill = 'white', outliers = FALSE) +
  geom_hline(aes(yintercept = 0), linetype = 'dashed') +
  theme_gtfswizard +
  viridis::scale_color_viridis(option = 'H', discrete = T) +
  scale_y_percent() +
  labs(x = 'Income decile', y = 'Accessibility difference (reachable jobs)', color = 'Income\ndecile') +
  facet_wrap(.~scenario, ncol = 3) #+
  #theme(legend.position = 'bottom')
ggsave('figs/boxplot.png', width = 9, height = 4)

# Quantas pessoas em cada decil de renda está um situação de problema? ----
accessibility %>% 
  mutate(cum.jobs.bl = if_else(cum.jobs.bl < critical.access, 'critical', 'not critical'),
         cum.jobs.dn = if_else(cum.jobs.dn < critical.access & cum.jobs.bl == 'critical', 'critical',
                               if_else(cum.jobs.dn < critical.access & cum.jobs.bl == 'not critical', 'new critical',
                                       if_else(cum.jobs.dn > critical.access & cum.jobs.bl == 'critical', 'new not critical',
                                               'not critical'))),
         cum.jobs.pf = if_else(cum.jobs.pf < critical.access & cum.jobs.bl == 'critical', 'critical',
                               if_else(cum.jobs.pf < critical.access & cum.jobs.bl == 'not critical', 'new critical',
                                       if_else(cum.jobs.pf > critical.access & cum.jobs.bl == 'critical', 'new not critical',
                                               'not critical'))),
         cum.jobs.opt = if_else(cum.jobs.opt < critical.access & cum.jobs.bl == 'critical', 'critical',
                                if_else(cum.jobs.opt < critical.access & cum.jobs.bl == 'not critical', 'new critical',
                                        if_else(cum.jobs.opt > critical.access & cum.jobs.bl == 'critical', 'new not critical',
                                                'not critical')))
  ) %>% 
  pivot_longer(cols = c(2, 3, 4), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor')) %>% 
  left_join(for.data %>% 
              filter(id_hex %in% accessibility$id_hex) %>% 
              select(id_hex, R003)) %>% 
  mutate(cum.jobs = cum.jobs %>% str_remove('new ')) %>% 
  group_by(R003, scenario, cum.jobs) %>% 
  reframe(pop = sum(pop, na.rm = T)) %>% 
  #mutate(pop = if_else(cum.jobs == 'not critical', -pop, pop)) %>% 
  ggplot() +
  geom_col(aes(x = R003, y = pop, fill = cum.jobs), position = 'stack') +
  facet_grid(.~scenario) +
  theme_gtfswizard +
  scale_y_comma(big.mark = '.') +
  scale_x_continuous(breaks = 1:6) +
  scale_fill_manual(values = c('firebrick', 'green4')) +
  labs(x = 'Income decile', fill = '', y = 'Population')
ggsave('figs/individuals.problem.income.png', dpi = 600)

# impact vs. accessibility distribution on baseline ----
accessibility %>% 
  mutate_at(2:4, function(x){x - .$cum.jobs.bl}) %>% 
  pivot_longer(cols = c(2, 3, 4), names_to = 'scenario', values_to = 'cum.jobs') %>%
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor'),
         cum.jobs.bl = cum.jobs.bl) %>% # a zona mais beneficiada deve ser a que possui menores niveis de cessibilidade independente quantidade de pessoas, pois nao se pode aceitar a reducao da acessibilidade de individuos em prol do aumento médio da acessibilidade
  #rstatix::identify_outliers(., variable = 'tot.access') %>% filter(is.extreme & tot.access > 0) %>% arrange(tot.access)
  #mutate(tot.access = ifelse(tot.access > 13461493, 13461493, ifelse(tot.access < -13461493 , -13461493, tot.access))) %>% 
  #st_as_sf() %>% 
  ggplot() +
  geom_point(aes(x = cum.jobs.bl, y = cum.jobs)) +
  theme_linedraw() +
  theme(legend.title = element_blank(), legend.position = 'none') +
  geom_smooth(aes(x = cum.jobs.bl, y = cum.jobs), method = 'lm') +
  #scale_x_log10() +
  # scale_fill_gradient2(low = 'firebrick', mid = 'white', high = 'green4', midpoint = 0,
  #                      limits = c(-13461493, 13461493)
  # ) +
  #labs(title = 'Distribuição espacial do efeito', subtitle = 'Accessibilitty x Population') +
  facet_wrap(scenario~., ncol = 3)
ggsave('figs/scatterplot.png', scale = 1.75, dpi = 600)

# accessibility poverty threshold sensibility analisys ----
thresholds <- 
  tibble(poverty.threshold = seq(0, 1, .05),
         reachable.jobs = poverty.threshold * tot.jobs)

accessibility %>% 
  select(-geom) %>% 
  pivot_longer(c(2:4, 6), names_to = 'scenario', values_to = 'reach.jobs') %>% 
  bind_cols(pivot_wider(thresholds, names_from = 1, values_from = 2)) %>% 
  pivot_longer(5:25, names_to = 'poverty.threshold', values_to = 'jobs.threshold') %>% 
  mutate(poor = if_else(reach.jobs - jobs.threshold > 0, 'not-access-poor', 'access-poor')) %>% 
  filter(poor == 'access-poor') %>% 
  group_by(scenario, poverty.threshold) %>%
  reframe(pop = sum(pop)/population) %>% 
  mutate(poverty.threshold = as.numeric(poverty.threshold),
         scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor') %>% str_replace('cum.jobs.bl', 'Baseline')) %>% 
  ggplot() +
  geom_line(aes(x = poverty.threshold, y = pop, color = scenario), linewidth = 1) +
  theme_gtfswizard +
  scale_x_percent() +
  scale_y_percent() +
  theme(legend.position = 'bottom') +
  labs(x = 'Poverty Threshold (% jobs reachable)', y = 'Low-income individuals from 19 to 69 yo (%)', color = '')
ggsave('figs/cdf2.png', scale = .75)

# sigma sensibility analisys ----
#resultados
# sigma <- 50.95931 # 50% nos empregos alcançáveis em 60 minutos
# sigma <- 25.479655 # 50% nos empregos alcançáveis em 30 minutos
# sigma <- 12.739827 # 50% nos empregos alcançáveis em 15 minutos
rvmethod::gaussfunc(105, 0, 89.17879)

access.from.sigma <- function(sigma) {
  
  ttm.baseline <- 
    read_csv('data/performance/ttm/ttm.baseline.csv')
  
  accessibility <- 
    left_join(od, ttm.baseline) %>% 
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
    reframe(cum.jobs.bl = sum(dec.jobs)) %>% 
    left_join(for.data %>% select(id_hex, geom))
  
  ttm.donothing <- 
    read_csv('data/performance/ttm/ttm.donothing.csv')
  
  accessibility <- 
    left_join(od, ttm.donothing) %>% 
    mutate(travel_time = if_else(is.na(travel_time), Inf, travel_time)) %>% 
    filter(!from_id == to_id) %>% 
    rename(id_hex = to_id) %>% 
    left_join(for.data) %>% 
    group_by(from_id, travel_time) %>% 
    reframe(jobs = sum(T002) + sum(T003)) %>%
    mutate(dec.jobs = jobs * rvmethod::gaussfunc(travel_time, 0, sigma)) %>%
    rename(id_hex = from_id) %>% 
    group_by(id_hex) %>% 
    reframe(cum.jobs.dn = sum(dec.jobs)) %>% 
    left_join(accessibility)
  
  ttm.pasfor <- 
    read_csv('data/performance/ttm/ttm.pasfor.csv')
  
  accessibility <- 
    left_join(od, ttm.pasfor) %>% 
    mutate(travel_time = if_else(is.na(travel_time), Inf, travel_time)) %>% 
    filter(!from_id == to_id) %>% 
    rename(id_hex = to_id) %>% 
    left_join(for.data) %>% 
    group_by(from_id, travel_time) %>% 
    reframe(jobs = sum(T002) + sum(T003)) %>%
    mutate(dec.jobs = jobs * rvmethod::gaussfunc(travel_time, 0, sigma)) %>%
    rename(id_hex = from_id) %>% 
    group_by(id_hex) %>% 
    reframe(cum.jobs.pf = sum(dec.jobs)) %>% 
    left_join(accessibility)
  
  ttm.optimal <-
    data.table::fread('data/performance/ttm/ttm.optimal.csv')
  
  accessibility <- 
    left_join(od, ttm.optimal) %>% 
    mutate(travel_time = if_else(is.na(travel_time), Inf, travel_time)) %>% 
    filter(!from_id == to_id) %>% 
    rename(id_hex = to_id) %>% 
    left_join(for.data) %>% 
    group_by(from_id, travel_time) %>% 
    reframe(jobs = sum(T002) + sum(T003)) %>%
    mutate(dec.jobs = jobs * rvmethod::gaussfunc(travel_time, 0, sigma)) %>%
    rename(id_hex = from_id) %>% 
    group_by(id_hex) %>% 
    reframe(cum.jobs.opt = sum(dec.jobs)) %>% 
    left_join(accessibility) %>% 
      mutate(sigma)
  
  return(accessibility)
}

sigmas <- c('min15' = 12.739827, 'min30' = 25.479655, 'min45' = 38.21948, 'min60' = 50.95931, 'min75' = 63.699135, 'min90' = 76.43896, 'min105' = 89.17879, 'min120' = 101.91862)

var.sigma <- lapply(sigmas, access.from.sigma)

# sigma scenarios
accessibility.sigma <-
  data.table::rbindlist(var.sigma) %>% 
  tibble %>% 
    mutate(sigma.scenario = if_else(round(sigma) == round(12.73983), 'min15', 
                              if_else(round(sigma) == round(25.47966), 'min30', 
                                      if_else(round(sigma) == round(38.21948), 'min45',
                                              if_else(round(sigma) == round(50.95931), 'min60', 
                                                      if_else(round(sigma) == round(63.69913), 'min75',
                                                              if_else(round(sigma) == round(76.43896), 'min90',
                                                                      if_else(round(sigma) == round(89.17879), 'min105', 'min120')
                                                              )
                                                      )
                                              )
                                      )
                              )
    ) %>% factor(., levels = c('min15', 'min30', 'min45', 'min60', 'min75', 'min90', 'min105', 'min120'))
    )

rep(accessibility$cum.jobs, accessibility$pop) %>%
  quantile(., .5)

sigmas.critical.access <- 
  lapply(var.sigma, function(x) {
  rep(x$cum.jobs.bl, x$pop) %>%
    quantile(., .5)
} ) %>% 
  unlist %>% 
  data.frame %>% 
  rownames_to_column() %>% 
  mutate(rowname = str_remove(rowname, '.50%')) %>% 
  setNames(c('sigma.scenario', 'critical.access')) %>% 
  as_tibble()

sigma.performance <- 
  accessibility.sigma %>% 
  select(-geom) %>% 
  left_join(sigmas.critical.access, by = 'sigma.scenario') %>% 
  pivot_longer(c(2:4, 6), names_to = 'scenario', values_to = 'accessibility') %>% 
  filter(accessibility <= critical.access) %>%
  group_by(scenario, sigma.scenario, critical.access) %>% 
  reframe(critical.pop = sum(pop),
          perc.critical.pop = critical.pop/population) %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor') %>% str_replace('cum.jobs.bl', 'Baseline'),
         sigma.scenario = factor(sigma.scenario, levels = c('min15', 'min30', 'min45', 'min60', 'min75', 'min90', 'min105', 'min120')))

# graficos
sigma.performance %>% 
  ggplot() +
  geom_line(aes(x = sigma.scenario, y = perc.critical.pop, color = scenario, group = scenario), linewidth = 1) +
  theme_gtfswizard +
  scale_y_percent(limits = c(.4, .6)) +
  labs(x = 'Sigma Scenario', y = 'Low-income individuals from 19 to 69 yo\nunder critical accessibility levels (%)', color = '', subtitle = 'Critical accessibility is different between sigma scenarios.') +
  theme(legend.position = 'bottom')
ggsave('figs/sigma.scenarios.line.png', scale = .75)

accessibility.sigma %>% 
  pivot_longer(cols = c(2, 3, 4, 6), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor') %>% str_replace('cum.jobs.bl', 'Baseline'),
         cum.jobs = cum.jobs/tot.jobs) %>% 
  left_join(sigma.performance, by = 'sigma.scenario') %>% 
  ggplot +
  geom_histogram(aes(x = cum.jobs, weight = pop), bins = 60, fill = 'gray75') +
  geom_histogram(data = . %>% filter(cum.jobs < critical.access/tot.jobs), aes(x = cum.jobs, weight = pop, fill = 'Individuals\nin critical\naccessibility\nconditions'), bins = 60) +
  geom_vline(data = sigma.performance, aes(color = paste0('Median\naccessibility\nof ', format(round(critical.access/tot.jobs*100, 1), big.mark = "."), '%'), xintercept = critical.access/tot.jobs), linetype = 'dashed', linewidth = 1) +
  ggrepel::geom_text_repel(data = sigma.performance, aes(x = critical.access/tot.jobs, y = critical.pop, label = paste0(round(perc.critical.pop*100, 1), '%\n', format(critical.pop, big.mark = '.'))), color = 'black') +
  facet_grid(scenario~sigma.scenario, scales = 'free') +
  theme_linedraw() +
  labs(x = 'Accessibility (reachable)', y = 'Low-income individuals from 19 to 69 yo', color = '', fill = '') +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  scale_x_percent() +
  scale_fill_manual(values = 'red')
ggsave('figs/sigma.hist.free.png', scale = 1.5)

# for loop
# varios mapas criticcal not critical

etiquetas1 <- accessibility.sigma$sigma %>% unique
#i <- 1

for (i in 1:length(etiquetas1)) {
  
  critical.access2 <- 
    sigma.performance[sigma.performance$sigma.scenario == names(sigmas)[i], 3] %>% 
    max(.$critical.access)
  
  accessibility.sigma %>% 
    filter(sigma == etiquetas1[i]) %>% 
    mutate(cum.jobs.bl = if_else(cum.jobs.bl < critical.access2, 'critical', 'not critical'),
           cum.jobs.dn = if_else(cum.jobs.dn < critical.access2 & cum.jobs.bl == 'critical', 'critical',
                                 if_else(cum.jobs.dn < critical.access2 & cum.jobs.bl == 'not critical', 'new critical',
                                         if_else(cum.jobs.dn > critical.access2 & cum.jobs.bl == 'critical', 'new not critical',
                                                 'not critical'))),
           cum.jobs.pf = if_else(cum.jobs.pf < critical.access2 & cum.jobs.bl == 'critical', 'critical',
                                 if_else(cum.jobs.pf < critical.access2 & cum.jobs.bl == 'not critical', 'new critical',
                                         if_else(cum.jobs.pf > critical.access2 & cum.jobs.bl == 'critical', 'new not critical',
                                                 'not critical'))),
           cum.jobs.opt = if_else(cum.jobs.opt < critical.access2 & cum.jobs.bl == 'critical', 'critical',
                                  if_else(cum.jobs.opt < critical.access2 & cum.jobs.bl == 'not critical', 'new critical',
                                          if_else(cum.jobs.opt > critical.access2 & cum.jobs.bl == 'critical', 'new not critical',
                                                  'not critical')))
    ) %>% 
    pivot_longer(cols = c(2, 3, 4), names_to = 'scenario', values_to = 'cum.jobs') %>% 
    mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor')) %>% 
    st_as_sf() %>% 
    ggplot() +
    geom_sf(data = shp.bairro) +
    geom_sf(aes(fill = cum.jobs), color = NA) +
    geom_sf(data = . %>% filter(cum.jobs %in% c('new critical', 'new not critical')), aes(fill = cum.jobs), color = 'white') +
    geom_sf(data = shp.metro.future) +
    theme_linedraw() +
    theme(legend.title = element_blank(), legend.position = 'bottom') +
    scale_fill_manual(values = c('firebrick', 'red', 'green3', 'gray40')) +
    labs(title = names(sigmas)[i], subtitle = 'Distribuição espacial do efeito da intervenção') +
    facet_wrap(scenario~., ncol = 3)
  ggsave(paste0('figs/impact.spatial.distribution', names(sigmas)[i], '.png'), scale = 1.5, dpi = 600)
  
}


# for loop
# varios mapas criticcal not critical

etiquetas1 <- accessibility.sigma$sigma %>% unique
#i <- 1

for (i in 1:length(etiquetas1)) {
  
  critical.access2 <- 
    sigma.performance[sigma.performance$sigma.scenario == names(sigmas)[i], 3] %>% 
    max(.$critical.access)
  
  accessibility.sigma %>% View
    filter(sigma == etiquetas1[i]) %>% 
    mutate_at(2:4, function(x){x - .$cum.jobs.bl}) %>% 
    pivot_longer(cols = c(2, 3, 4), names_to = 'scenario', values_to = 'cum.jobs') %>%
    mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor'),
           tot.access = cum.jobs * pop) %>% 
    #rstatix::identify_outliers(., variable = 'tot.access') %>% filter(is.extreme & tot.access > 0) %>% arrange(tot.access)
    mutate(tot.access = ifelse(tot.access > 2*13461493, 2*13461493, ifelse(tot.access < -2*13461493 , -2*13461493, tot.access))) %>% 
    st_as_sf() %>% 
    ggplot() +
    geom_sf(data = shp.bairro) +
    geom_sf(aes(fill = tot.access), color = NA) +
    geom_sf(data = shp.metro.future) +
    theme_linedraw() +
    theme(legend.title = element_blank(), legend.position = 'none') +
    scale_fill_gradient2(low = 'firebrick', mid = 'white', high = 'green4', midpoint = 0,
                         limits = c(-2*13461493, 2*13461493)
    ) +
    labs(title = names(sigmas)[i], subtitle = c('Distribuição espacial do efeito\nAccessibilitty x Population')) +
    facet_wrap(scenario~., ncol = 3)
  ggsave(paste0('figs/2impact.spatial.distribution', names(sigmas)[i], '.png'), scale = 1.75, dpi = 600)
    
}
