library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)

coast <- rnaturalearth::ne_countries( scale = 50, returnclass = 'sf')
coast <- st_crop(coast, xmin = -95, ymin = 43, xmax = -30, ymax = 72)
plot(st_geometry(coast))
saveRDS(coast, 'coast.RDS')


# -----
moon_nao <- data.frame(
  Date = c(seq.Date(as.Date("2018-01-01"), by =1, length.out = 90),
           seq.Date(as.Date("2019-01-01"), by =1, length.out = 90)),
  Year = rep(c('2018','2019'), each = 90),
  Moon = NA
  )

moon_nao$Moon <- suncalc::getMoonIllumination(moon_nao$Date)$fraction
nao <- readRDS('E:/TBMU-Winter-Behaviour/data/nao.RDS')
saveRDS(merge(moon_nao, nao), 'moon_nao.RDS')


get_correct_temp <- function(T0, T1, s, bs) {
  T0 - ((T0 - T1)/(1 - exp(-bs/s)))
}

theme_set(theme_light())

the_files <- list.files('D:/TBMU-COE-Moult/data/classified', full.names = T)[c(1:24,26:40)]

dat <- data.frame()
for (i in 1:length(the_files)) {
  print(i)
  temp <- readRDS(the_files[i]) %>% 
    # filter(Mon %in% c(9:12,1:5), DOY %in% c(244,274,305,335,1,32,60,91,121)) 
  filter(Mon %in% c(1:3), DOY %in% seq(1, 365, 5), Date < as.Date('2018-12-31', tz = 'UTC')) 
  
  dat <- rbind(dat, temp)
}

saveRDS(dat, 'tdr_combined_small.RDS')

# -----

dat <- readRDS('tdr_combined_small.RDS')
bb <- unique(dat$Band)

dd <- dat %>% 
  select(Band, Date, Time, Temp, Depth, Light, Behaviour, In_bout) %>% 
  filter(Band %in% bb)
saveRDS(dd, 'tdr_combined_small.RDS')

table(dd$Band)
# -----

bb <- unique(dd$Band)
locs <- data.frame()
for (i in 1:length(bb)) {
  loc_files <- list.files('D:/Geolocators/analysis/probGLS/Coats_2017_2019', pattern = 'RDS',
                          full.names = T)
  
  temp <- data.frame()
  idx <- grep(bb[i], loc_files)
  for (j in idx) {
    temp <- rbind(temp, as.data.frame(readRDS(loc_files[j])$most))
  }
  
  temp <- temp %>% 
    as.data.frame() %>% 
    filter(type == 1) %>% 
    mutate(
      Band = bb[i],
      Date = as.Date(dtime)
    ) %>% 
    select(Band, Date, lon, lat)
  head(temp)
  locs <- rbind(locs, temp)
}
saveRDS(locs, 'gls_locations.RDS')

# -----

dat <- readRDS('tdr_combined_small.RDS')
locs <- readRDS('gls_locations.RDS')

bb <- unique(dat$Band)[1:2]
dd <- unique(dat$Date)[1:3]

dat_example <- subset(dat, dat$Band %in% bb & dat$Date %in% dd)
locs_example <- subset(locs, dat$Band %in% bb)

saveRDS(dat_example,'tdr_example.RDS')
saveRDS(locs_example, 'locations_example.RDS')


