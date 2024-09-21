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

library(shadowtext)

rstudioapi::getActiveDocumentContext() %>% 
  .$path %>% 
  dirname() %>% 
  setwd(); setwd('..')

theme_gtfswizard <-
  hrbrthemes::theme_ipsum(base_family = "Times New Roman",
                          axis_title_face = 'bold') +
  ggplot2::theme(title = ggplot2::element_text(size = 11, color = '#283c42'),,
                 axis.title.x = ggplot2::element_text(size = 11, color = '#283c42'),
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

tot.jobs <- 
  filter(for.data, id_hex %in% destinations$id) %>% 
  mutate(trab = .$T002 + .$T003) %>% 
  .$trab %>% 
  sum()

# ts data ----
shp.metro.future <- 
  read_gtfs('data/gtfs/metrofor.future.gtfs.zip') %>% 
  .$shapes %>% 
  as_shapes_sf() %>% 
  filter(!shape_id %in% c('shape-10', 'shape-8', 'shape-9'))

shp.metro.all <- 
  read_gtfs('data/gtfs/metrofor.future.gtfs.zip') %>% 
  .$shapes %>% 
  as_shapes_sf() %>% 
  filter(!shape_id %in% c('shape-10', 'shape-8', 'shape-9')) %>% 
  mutate(scenario = if_else(!shape_id == 'shape-7', list(c('Baseline (a)', 'Do nothing (b)', 'Optimal (c)', 'Pasfor (d)')), list(c('Do nothing (b)', 'Optimal (c)', 'Pasfor (d)')))) %>% 
  unnest(cols = 'scenario')

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
  mutate(cum.jobs.bl = if_else(cum.jobs.bl < critical.access, '\nCritical\n', '\nNot\ncritical\n'),
         cum.jobs.dn = if_else(cum.jobs.dn < critical.access & cum.jobs.bl == '\nCritical\n', '\nCritical\n',
                               if_else(cum.jobs.dn < critical.access & cum.jobs.bl == '\nNot\ncritical\n', '\nNew\ncritical\n',
                                       if_else(cum.jobs.dn > critical.access & cum.jobs.bl == '\nCritical\n', '\nNew not\ncritical\n',
                                               '\nNot\ncritical\n'))),
         cum.jobs.pf = if_else(cum.jobs.pf < critical.access & cum.jobs.bl == '\nCritical\n', '\nCritical\n',
                               if_else(cum.jobs.pf < critical.access & cum.jobs.bl == '\nNot\ncritical\n', '\nNew\ncritical\n',
                                       if_else(cum.jobs.pf > critical.access & cum.jobs.bl == '\nCritical\n', '\nNew not\ncritical\n',
                                               '\nNot\ncritical\n'))),
         cum.jobs.opt = if_else(cum.jobs.opt < critical.access & cum.jobs.bl == '\nCritical\n', '\nCritical\n',
                               if_else(cum.jobs.opt < critical.access & cum.jobs.bl == '\nNot\ncritical\n', '\nNew\ncritical\n',
                                       if_else(cum.jobs.opt > critical.access & cum.jobs.bl == '\nCritical\n', '\nNew not\ncritical\n',
                                               '\nNot\ncritical\n')))
         ) %>% 
  pivot_longer(cols = c(2, 3, 4, 6), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.bl', 'Baseline (a)') %>% str_replace('cum.jobs.dn', 'Do nothing (b)') %>% str_replace('cum.jobs.opt', 'Optimal (c)') %>% str_replace('cum.jobs.pf', 'Pasfor (d)')) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(data = shp.bairro, color = NA) +
  geom_sf(aes(fill = cum.jobs), color = NA) +
  geom_sf(data = . %>% filter(cum.jobs %in% c('New\ncritical', 'New not\ncritical')), aes(fill = cum.jobs), color = NA, linewidth = .38) +
  geom_sf(data = shp.metro.all, aes(linetype = 'Metro\nsystem')) +
  theme_gtfswizard +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c('lightpink', 'red', 'green4', 'darkseagreen2')) +
  labs(title = "Spatial distribution of intervention's effect over the problem") +
  facet_wrap(.~scenario, nrow = 2)
ggsave('figs/impact.spatial.distribution2.png', scale = 1.25, dpi = 320)

accessibility %>% 
  mutate_at(2:4, function(x){x - .$cum.jobs.bl}) %>% 
  pivot_longer(cols = c(2, 3, 4), names_to = 'scenario', values_to = 'cum.jobs') %>%
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing (a)') %>% str_replace('cum.jobs.opt', 'Optimal (b)') %>% str_replace('cum.jobs.pf', 'Pasfor (c)'),
         tot.access = cum.jobs * pop) %>% 
  #rstatix::identify_outliers(., variable = 'tot.access') %>% filter(is.extreme & tot.access > 0) %>% arrange(tot.access)
  mutate(tot.access = ifelse(tot.access > 14000000, 14000000, ifelse(tot.access < -14000000 , -14000000, tot.access))) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(data = shp.bairro, color = NA) +
  geom_sf(aes(fill = tot.access), color = NA) +
  geom_sf(data = shp.metro.future) +
  theme_gtfswizard +
  theme(legend.title = element_blank(), legend.position = 'bottom', legend.text = element_text(hjust = c(1, NA, 0))) +
  scale_fill_gradient2(low = 'firebrick', mid = 'white', high = 'green4', midpoint = 0,
                       limits = c(-14000000, 14000000),
                       breaks = c(-13000000, 0, 13000000),
                       labels = c('high\naccessibility\nlosses', 'no\nimpact', 'high\naccessibility\ngains'),
                       ) +
  labs(title = "Spatial distribution of intervention's effect over accessibility distribution") +
  facet_wrap(scenario~., ncol = 3)
ggsave('figs/impact.spatial.distribution4.png', scale = 1.5, dpi = 320)



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
  mutate(scenario = c('Baseline (a)', 'Do nothing (b)', 'Optimal (c)', 'Pasfor (d)'))

tot.jobs <- 
  filter(for.data, id_hex %in% destinations$id) %>% 
  mutate(trab = .$T002 + .$T003) %>% 
  .$trab %>% 
  sum()

accessibility %>% 
  pivot_longer(cols = c(2, 3, 4, 6), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing (b)') %>% str_replace('cum.jobs.opt', 'Optimal (c)') %>% str_replace('cum.jobs.pf', 'Pasfor (d)') %>% str_replace('cum.jobs.bl', 'Baseline (a)'),
         cum.jobs = cum.jobs/tot.jobs) %>% 
  ggplot +
  geom_histogram(aes(x = cum.jobs, weight = pop), bins = 60, fill = 'gray75') +
  geom_histogram(data = . %>% filter(cum.jobs < critical.access/tot.jobs), aes(x = cum.jobs, weight = pop, fill = 'Individuals\nin critical\naccessibility\nconditions'), bins = 60) +
  geom_vline(aes(color = paste0('Critical\naccessibility\nthershold\nof ', format(round(critical.access/tot.jobs*100, 1), big.mark = "."), '%'), xintercept = critical.access/tot.jobs), linetype = 'dashed', linewidth = 1) +
  geom_text(data = perc.label, aes(x = .38, y = 10000, label = people.critical.perc), color = 'white') +
  # geom_point(data = perc.label, aes(x = 2500, 40000, size = total.bus.distance/max(total.bus.distance)), color = 'blue') +
  # geom_text(data = perc.label, aes(x = 3500, 40000, label = paste0('Bus\nlength\n', format(round(total.bus.distance/1000), big.mark = '.'), 'km'))) +
  # geom_point(data = perc.label, aes(x = 12000, 40000, size = total.metro.distance/max(total.metro.distance)), color = 'green') +
  # geom_text(data = perc.label, aes(x = 13000, 40000, label = paste0('Metro\nlength\n', format(round(total.metro.distance/1000), big.mark = '.'), 'km'))) +
  facet_wrap(.~scenario, ncol = 2) +
  theme_gtfswizard +
  labs(title = 'Accessibility distribution', x = 'Accessibility (reachable jobs)', y = 'Low-income individuals from 19 to 69 yo', color = '', fill = '') +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  scale_x_percent() +
  scale_fill_manual(values = 'red') +
  scale_color_manual(values = 'black')
ggsave('figs/problem.distribution.png', width = 7.5, height = 5, scale = 1.45)

# accessibility poverty threshold sensibility analisys ----
population <- sum(accessibility$pop)

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
  #geom_line(aes(x = poverty.threshold, y = pop, color = scenario), linewidth = .8, alpha = .7) +
  geom_line(aes(x = poverty.threshold, y = pop, color = scenario)) +
  geom_point(aes(x = poverty.threshold, y = pop, color = scenario)) +
  theme_gtfswizard +
  scale_x_percent() +
  scale_y_percent() +
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  labs(x = 'Poverty Threshold (% jobs reachable)', y = 'Low-income individuals\nfrom 19 to 69 yo (%)', title = 'Poverty Threshold Sensibility Analisys')
ggsave('figs/cdf2.png', scale = .75)

# palma ratio ----
accessibility %>% 
  pivot_longer(cols = c(2, 3, 4, 6), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  select(-geom, -id_hex) %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor') %>% str_replace('cum.jobs.bl', 'Baseline')) %>% 
  arrange(scenario, cum.jobs) %>% 
  group_by(scenario) %>% 
  mutate(cumpop = cumsum(pop),
         percentile = cumpop/sum(pop)) %>% 
  filter(percentile <= .4 | percentile >= .9) %>% 
  mutate(class = if_else(percentile <= .4, 'bt40', 'tp10')) %>% 
  group_by(scenario, class) %>% 
  reframe(access = mean(cum.jobs)) %>% 
  group_by(scenario) %>% 
  reframe(palma.ratio = access[2] / access[1]) %>% 
  kableExtra::kbl() %>% 
  kableExtra::kable_classic()

# gini index ----
gini.dat <- 
  accessibility %>% 
  pivot_longer(cols = c(2, 3, 4, 6), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  select(-geom) %>% 
  mutate(tot.access = pop * cum.jobs,
         cum.jobs = cum.jobs/tot.jobs) %>% 
  arrange(scenario, cum.jobs) %>% 
  group_by(scenario) %>% 
  mutate(cum.tot.access = cumsum(tot.access), 
         cumpop = cumsum(pop)) %>%
  mutate(cum.tot.access = cum.tot.access/max(cum.tot.access), 
         cumpop = cumpop/max(cumpop)) %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor') %>% str_replace('cum.jobs.bl', 'Baseline'))
  

gini.dat %>% 
  ggplot() +
  geom_abline(aes(intercept = 0, slope = 1, linetype = 'Equality\nline')) +
  geom_vline(aes(xintercept = .5, linetype = 'Critical\naccessibility\nlevel')) +
  geom_line(aes(y = cum.tot.access, x = cumpop, color = scenario)) +
  theme_gtfswizard +
  scale_x_percent() +
  scale_y_percent() +
  scale_linetype_manual(values = c('dashed', 'solid')) +
  labs(title = 'Lorenz curve', y = 'Cumulative accessibility', x = 'Cumulative population', color = '', linetype = '') +
  theme(legend.position = 'bottom')
ggsave('figs/lorenz.png', scale = 1)

Ts <- (.5 * .5) / 2
Tns <- .5 * .75

gini.dat %>% 
  group_by(scenario, cum.tot.access) %>% 
  reframe(cumpop = cumpop[n()],
          critical = if_else(cumpop <= .5, 'Bs', 'Bns')) %>%
  arrange(scenario, cumpop) %>% 
  group_by(scenario, critical) %>% 
  mutate(dx = lead(cumpop) - cumpop,
         ybar = (lead(cum.tot.access) + cum.tot.access) / 2,
         area = dx * ybar) %>% 
  group_by(scenario, critical) %>% 
  reframe(area = sum(area, na.rm = T)) %>% 
  pivot_wider(names_from = 'critical', values_from = 'area') %>% 
  group_by(scenario) %>% 
  reframe(B = Bns + Bs,
         As = Ts - Bs,
         Asn = Tns - Bns,
         A = As + Asn,
         gini = A/(A+B),
         severity = As/(A+B),
         severity.perc = severity/gini)

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
  pivot_longer(cols = c(2, 3, 4, 6), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing (a)') %>% str_replace('cum.jobs.opt', 'Optimal (b)') %>% str_replace('cum.jobs.pf', 'Pasfor (c)')) %>% 
  left_join(for.data %>% 
              filter(id_hex %in% accessibility$id_hex) %>% 
              select(id_hex, R003)) %>% 
  mutate(cum.jobs = cum.jobs %>% str_remove('new ')) %>% 
  group_by(R003, scenario, cum.jobs) %>% 
  reframe(pop = sum(pop, na.rm = T)) %>% 
  pivot_wider(names_from = scenario, values_from = pop) %>% 
  mutate_at(3:5, function(x){x - .$cum.jobs.bl}) %>%
  pivot_longer(cols = c(3:5), names_to = 'scenario', values_to = 'diff.pop') %>% 
  filter(cum.jobs == 'critical') %>% 
  ggplot() +
  geom_col(aes(x = R003, y = diff.pop), fill = 'firebrick', position = 'stack') +
  geom_shadowtext(aes(x = R003, y = diff.pop/2, label = format(paste0(round(diff.pop/cum.jobs.bl*100, 1), '%'), vjust = -1))) +
  facet_grid(.~scenario) +
  theme_gtfswizard +
  scale_y_comma(big.mark = '.') +
  scale_x_continuous(breaks = 1:6) +
  labs(x = 'Income decile', fill = '', y = 'Difference in population', title = 'Difference in population under critical accessibility levels by income decile and scenario')
ggsave('figs/diff.individuals.problem.income.png', dpi = 320, scale = 1.5, height = 4)

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
  pivot_longer(cols = c(2, 3, 4, 6), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.bl', 'Baseline (a)') %>% str_replace('cum.jobs.dn', 'Do nothing (b)') %>% str_replace('cum.jobs.opt', 'Optimal (c)') %>% str_replace('cum.jobs.pf', 'Pasfor (d)')) %>% 
  left_join(for.data %>% 
              filter(id_hex %in% accessibility$id_hex) %>% 
              select(id_hex, R003)) %>% 
  mutate(cum.jobs = cum.jobs %>% str_remove('new ')) %>% 
  group_by(R003, scenario, cum.jobs) %>% 
  reframe(pop = sum(pop, na.rm = T)) %>% 
  group_by(R003, scenario) %>% 
  mutate(perc.pop = (pop / sum(pop) * 100) %>% 
           round(1) %>% paste0(., '%'),
         y = if_else(cum.jobs == 'not critical', pop/2, pop/2 + lead(pop))) %>% 
  ggplot() +
  geom_col(aes(x = R003, y = pop, fill = cum.jobs), position = 'stack') +
  geom_shadowtext(aes(x = R003, y = y, label = perc.pop)) +
  facet_grid(.~scenario) +
  theme_gtfswizard +
  scale_y_comma(big.mark = '.') +
  scale_x_continuous(breaks = 1:6) +
  theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('firebrick', 'green4')) +
  labs(x = 'Income decile', fill = '', y = 'Population', title = 'Population by accessibility levels, income decile and scenario')
ggsave('figs/individuals.problem.income.png', dpi = 320, scale = 1.75, height = 4)

# boxplot ----
for.data %>% 
  filter(id_hex %in% accessibility$id_hex) %>% 
  select(id_hex, R003) %>% 
  left_join(accessibility, .) %>%
  mutate(blcritical = if_else(cum.jobs.bl > critical.access, 'Critical (i)', 'Not critical (ii)'),
         dncritical = if_else(cum.jobs.dn > critical.access, 'Critical (i)', 'Not critical (ii)'),
         pfcritical = if_else(cum.jobs.pf > critical.access, 'Critical (i)', 'Not critical (ii)'),
         optcritical = if_else(cum.jobs.opt > critical.access, 'Critical (i)', 'Not critical (ii)')) %>% 
  mutate_at(2:4, function(x){x - .$cum.jobs.bl}) %>% 
  select(-geom) %>% 
  mutate(R003 = factor(R003, levels = 1:6)) %>% 
  pivot_longer(cols = c(2, 3, 4), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  mutate(critical = if_else(scenario == 'cum.jobs.opt', optcritical, 
                            if_else(scenario == 'cum.jobs.bl', blcritical,
                                    if_else(scenario == 'cum.jobs.dn', dncritical, pfcritical)))) %>% 
  mutate(cum.jobs = cum.jobs/tot.jobs) %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing (a)') %>% str_replace('cum.jobs.opt', 'Optimal (b)') %>% str_replace('cum.jobs.pf', 'Pasfor (c)')) %>% 
  ggplot() +
  geom_hline(aes(yintercept = 0), linetype = 'dashed') +
  geom_boxplot(aes(x = R003, y = cum.jobs, color = R003, group = R003, weight = pop), fill = NA, outliers = FALSE) +
  #geom_violin(aes(x = R003, y = cum.jobs, fill = R003, group = R003, weight = pop), color = NA, outliers = FALSE) +
  theme_gtfswizard +
  viridis::scale_color_viridis(option = 'H', discrete = T) +
  scale_y_percent() +
  labs(x = 'Income decile', y = 'Accessibility difference (reachable jobs)', color = 'Income\ndecile') +
  facet_wrap(critical~scenario, ncol = 3)
ggsave('figs/boxplot.png', width = 9, height = 6)

# impact vs. accessibility distribution on baseline ----
accessibility %>% 
  mutate_at(2:4, function(x){(x - .$cum.jobs.bl)/tot.jobs}) %>% 
  pivot_longer(cols = c(2, 3, 4), names_to = 'scenario', values_to = 'cum.jobs') %>%
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor'),
         cum.jobs.bl = cum.jobs.bl/tot.jobs) %>% # a zona mais beneficiada deve ser a que possui menores niveis de cessibilidade independente quantidade de pessoas, pois nao se pode aceitar a reducao da acessibilidade de individuos em prol do aumento médio da acessibilidade
  left_join(., select(for.data, id_hex, R003) %>% tibble() %>% select(-geom)) %>% 
  ggplot() +
  geom_point(aes(x = cum.jobs.bl, y = cum.jobs, color = as_factor(R003)), alpha = .5) +
  theme_gtfswizard +
  theme(legend.position = 'bottom') +
  geom_smooth(aes(x = cum.jobs.bl, y = cum.jobs, group = R003), color = 'black', method = 'lm') +
  facet_grid(scenario~R003) +
  scale_x_percent() +
  scale_y_percent(limits = c(-.1, .1)) +
  labs(x = 'Baseline Accessibility (reachable jobs)', y = 'Accessibility Difference (reachable jobs)', title = 'Project impact by scenario, baseline accessibility and income decile', subtitle = 'Income decile', color = 'Income\ndecile')
ggsave('figs/scatterplot.png', scale = 1.2, dpi = 600, height = 6)

# sigma sensibility analisys ----
#resultados
# sigma <- 50.95931 # 50% nos empregos alcançáveis em 60 minutos
# sigma <- 25.479655 # 50% nos empregos alcançáveis em 30 minutos
# sigma <- 12.739827 # 50% nos empregos alcançáveis em 15 minutos
#rvmethod::gaussfunc(105, 0, 89.17879)

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

population <- sum(accessibility$pop)
  
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
  labs(x = 'Sigma Scenario', y = 'Low-income individuals from\n19 to 69 yo under critical\naccessibility levels (%)', color = '', title = 'Problem magnitude to sigma variation') +
  theme(legend.position = 'bottom')
ggsave('figs/sigma.scenarios.line.png', height = 4.5)

accessibility.sigma %>% 
  pivot_longer(cols = c(2, 3, 4, 6), names_to = 'scenario', values_to = 'cum.jobs') %>% 
  mutate(scenario = scenario %>% str_replace('cum.jobs.dn', 'Do nothing') %>% str_replace('cum.jobs.opt', 'Optimal') %>% str_replace('cum.jobs.pf', 'Pasfor') %>% str_replace('cum.jobs.bl', 'Baseline'),
         cum.jobs = cum.jobs/tot.jobs) %>% 
  left_join(sigma.performance, by = 'sigma.scenario') %>% 
  ggplot +
  geom_histogram(aes(x = cum.jobs, weight = pop), bins = 60, fill = 'gray75') +
  geom_histogram(data = . %>% filter(cum.jobs < critical.access/tot.jobs), aes(x = cum.jobs, weight = pop, fill = 'Individuals\nin critical\naccessibility\nconditions'), bins = 60) +
  geom_vline(data = sigma.performance, aes(color = paste0('Median\naccessibility\nof ', format(round(critical.access/tot.jobs*100, 1), big.mark = "."), '%'), xintercept = critical.access/tot.jobs), linetype = 'dashed', linewidth = 1) +
  ggrepel::geom_text_repel(data = sigma.performance, aes(x = critical.access/tot.jobs, y = critical.pop * 2, label = paste0(round(perc.critical.pop*100, 1), '%\n', format(critical.pop, big.mark = '.'))), color = 'black') +
  facet_grid(scenario~sigma.scenario, scales = 'free') +
  theme_gtfswizard +
  theme(legend.title = element_blank(), legend.position = 'bottom') +
  labs(x = 'Accessibility (reachable)', y = 'Low-income individuals from 19 to 69 yo', title = 'Problem distribution to sigma variation') +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  scale_x_percent() +
  scale_fill_manual(values = 'red')
ggsave('figs/sigma.hist.free.png', scale = 2)

