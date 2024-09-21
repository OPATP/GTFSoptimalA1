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
  hrbrthemes::scale_x_comma(big.mark = '.', limits = c(415000, 520000)) +
  hrbrthemes::scale_y_comma(big.mark = '.', limits = c(123000, 140000)) +
  labs(x = 'People under Critical Levels of Accessibility (individuals)', y = 'Total System Distance (km)', color = 'Scenarios') +
  theme_minimal()
ggsave('figs/point.scenarios.png', scale = .75)

# accessibility distribution BASELINE ----
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
  mutate(dec.jobs = jobs / (travel_time)) %>%
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
  mutate(dec.jobs = jobs / (travel_time)) %>%
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
  mutate(dec.jobs = jobs / (travel_time)) %>%
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
  mutate(dec.jobs = jobs / (travel_time)) %>%
  rename(id_hex = from_id) %>% 
  group_by(id_hex) %>% 
  reframe(cum.jobs.opt = sum(dec.jobs)) %>% 
  left_join(accessibility)

# comparing spatial impacts ----
accessibility %>% 
  pivot_longer(cols = c(2, 3, 4, 6), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(data = shp.bairro) +
  geom_sf(aes(fill = cum.jobs), color = NA) +
  viridis::scale_fill_viridis(option = 'F') +
  facet_wrap(.~scenario) +
  #theme(legend.position = 'none') +
  theme_linedraw() +
  ggspatial::annotation_scale(location = "bl", text_cex = 0.8, bar_cols = c("#333333", 'white')) +
  ggspatial::annotation_north_arrow(location = "tr",
                                    style = ggspatial::north_arrow_fancy_orienteering(fill = c('white', '#333333'),
                                                                                      line_col = '#333333',
                                                                                      text_col = '#333333'))

critical.access <-
  read_lines('data/critical.access.txt') %>% 
  as.numeric()

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
  #,rstatix::identify_outliers(., variable = 'cum.jobs') %>%
  mutate(cum.jobs = ifelse(cum.jobs > 700, 700, ifelse(cum.jobs < -700 , -700, cum.jobs))) %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor')) %>% 
  st_as_sf() %>% 
  mutate(geom = st_centroid(geom)) %>% 
  ggplot() +
  geom_sf(data = shp.bairro) +
  geom_sf(aes(color = cum.jobs, size = pop/max(pop))) +
  scale_size_continuous(limits = c(0, 1), range = c(.1, 1.25)) +
  theme_linedraw() +
  theme(legend.title = element_blank(), legend.position = 'none') +
  scale_color_gradient2(low = 'firebrick', mid = 'white', high = 'green4', midpoint = 0, limits = c(-700, 700)) +
  labs(title = 'Distribuição espacial do efeito da intervenção') +
  facet_wrap(scenario~., ncol = 1)
ggsave('figs/impact.spatial.distribution3.png', scale = 1.75, dpi = 600)

accessibility %>% 
  mutate_at(2:4, function(x){x - .$cum.jobs.bl}) %>% 
  pivot_longer(cols = c(2, 3, 4), names_to = 'scenario', values_to = 'cum.jobs') %>%
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor'),
         tot.access = cum.jobs * pop) %>% 
  #rstatix::identify_outliers(., variable = 'tot.access') %>% filter(is.extreme & tot.access < 0) %>% arrange(-tot.access)
  mutate(tot.access = ifelse(tot.access > 327349, 327349, ifelse(tot.access < -327349 , -327349, tot.access))) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(data = shp.bairro) +
  geom_sf(aes(fill = tot.access), color = NA) +
  geom_sf(data = shp.metro.future) +
  theme_linedraw() +
  theme(legend.title = element_blank(), legend.position = 'none') +
  scale_fill_gradient2(low = 'firebrick', mid = 'white', high = 'green4', midpoint = 0,
                       limits = c(-327349, 327349)
                       ) +
  labs(title = 'Distribuição espacial do efeito', subtitle = 'Accessibilitty x Population') +
  facet_wrap(scenario~., ncol = 3)
ggsave('figs/impact.spatial.distribution4.png', scale = 1.75, dpi = 600)


# comparing acessibility distribution ----

population <- sum(accessibility$pop)

perc.label <- 
  performance %>%
  mutate(people.critical.perc = (people.critical/population * 100) %>%
           round(., 2) %>%
           paste0(., '%\n', format(people.critical, big.mark = "."), ' ind.')) %>% 
  #select(scenario, people.critical.perc, max.bus.fleet, total.bus.distance) %>% 
  mutate(fleet.norm = max.bus.fleet / max(max.bus.fleet),
           distance.norm = total.bus.distance / max(total.bus.distance)) %>% 
  mutate(scenario = c('Baseline', 'Do nothing', 'Optimal', 'Pasfor'))

filter(for.data, id_hex %in% destinations$id) %>% 


accessibility %>% 
  pivot_longer(cols = c(2, 3, 4, 6), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor') %>% str_replace('cum.jobs.bl', 'Baseline')) %>% 
  ggplot +
  geom_histogram(aes(x = cum.jobs, weight = pop), bins = 60, fill = 'gray75') +
  geom_histogram(data = . %>% filter(cum.jobs < critical.access), aes(x = cum.jobs, weight = pop, fill = 'Individuals\nin critical\naccessibility\nconditions'), bins = 60) +
  geom_vline(aes(color = paste('Median\naccessibility\nof', format(round(critical.access, 0), big.mark = ".")), xintercept = critical.access), linetype = 'dashed', linewidth = 1) +
  geom_text(data = perc.label, aes(x = 6750, y = 20000, label = people.critical.perc), color = 'white') +
  # geom_point(data = perc.label, aes(x = 2500, 40000, size = total.bus.distance/max(total.bus.distance)), color = 'blue') +
  # geom_text(data = perc.label, aes(x = 3500, 40000, label = paste0('Bus\nlength\n', format(round(total.bus.distance/1000), big.mark = '.'), 'km'))) +
  # geom_point(data = perc.label, aes(x = 12000, 40000, size = total.metro.distance/max(total.metro.distance)), color = 'green') +
  # geom_text(data = perc.label, aes(x = 13000, 40000, label = paste0('Metro\nlength\n', format(round(total.metro.distance/1000), big.mark = '.'), 'km'))) +
  facet_grid(scenario~.) +
  theme_linedraw() +
  labs(x = 'Accessibility', y = 'Low-income individuals from 19 to 69 yo', color = '', fill = '') +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  scale_fill_manual(values = 'red') +
  scale_size(limits = c(.25, 1), guide = 'none') +
  scale_color_manual(values = 'black') +
  theme(axis.text.x = element_blank(), legend.position = 'bottom')
ggsave('figs/problem.distribution.png', scale = 1.25)

# CDF ----
accessibility %>% 
  pivot_longer(cols = c(2, 3, 4, 6), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  group_by(scenario, cum.jobs) %>% 
  reframe(pop = sum(pop)) %>% 
  arrange(scenario, cum.jobs) %>% 
  group_by(scenario) %>% 
  mutate(cumpop = cumsum(pop),
         cumcum.jobs = cumsum(cum.jobs)) %>% 
  mutate(cumpop = cumpop/max(cumpop),
         cumcum.jobs = cumcum.jobs/max(cumcum.jobs)) %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor') %>% str_replace('cum.jobs.bl', 'Baseline')) %>% 
  ggplot() +
  geom_line(aes(x = cum.jobs, y = cumpop, color = scenario)) +
  theme_gtfswizard +
  labs(titble = 'CDF', x = 'Accessibility (cumulative jobs)', y = 'Population (%)', color = '') +
  theme(legend.position = 'bottom')
ggsave('figs/cdf1.png')


accessibility %>% 
  pivot_longer(cols = c(2, 3, 4, 6), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  group_by(scenario, cum.jobs) %>% 
  reframe(pop = sum(pop)) %>% 
  arrange(scenario, cum.jobs) %>% 
  group_by(scenario) %>% 
  mutate(cumpop = cumsum(pop),
         cumcum.jobs = cumsum(cum.jobs)) %>% 
  mutate(cumpop = cumpop/max(cumpop),
         cumcum.jobs = cumcum.jobs/max(cumcum.jobs)) %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor') %>% str_replace('cum.jobs.bl', 'Baseline')) %>% 
  ggplot() +
  geom_abline(intercept = 0, slope = 1) +
  geom_line(aes(x = cumcum.jobs, y = cumpop, color = scenario)) +
  theme_gtfswizard +
  labs(titble = 'CDF', x = 'Accessibility (%)', y = 'Population (%)', color = '') +
  theme(legend.position = 'bottom')
ggsave('figs/cdf2.png')

# boxplot ----
for.data %>% 
  filter(id_hex %in% accessibility$id_hex) %>% 
  select(id_hex, R003) %>% 
  left_join(accessibility, .) %>% 
  mutate_at(2:4, function(x){x - .$cum.jobs.bl}) %>% 
  mutate(R003 = factor(R003, levels = 1:6)) %>% 
  pivot_longer(cols = c(2, 3, 4), names_to = 'scenario', values_to = 'cum.jobs') %>%
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor')) %>% 
  ggplot() +
  geom_hline(aes(yintercept = 0), linetype = 'dashed') +
  geom_boxplot(aes(x = R003, y = cum.jobs, color = R003, group = R003), fill = NA, outliers = FALSE) +
  theme_gtfswizard +
  viridis::scale_color_viridis(option = 'H', discrete = T) +
  labs(x = 'Decil de renda', y = 'Accessibility difference (cumulative jobs)', color = 'Decil de renda') +
  facet_wrap(.~scenario, ncol = 1)
ggsave('figs/boxplot.png', width = 9, height = 10)
