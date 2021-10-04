library(dplyr)
library(ggplot2)
library(shiny)
library(sf)

Sys.setenv(tz = 'UTC')

theme_set(theme_light())
theme_update(
  text = element_text(size = 14)
)

coast <- readRDS('coast.RDS')
dat <- readRDS('tdr_example.RDS')
locs <- readRDS('locations_example.RDS')
moon_nao <- readRDS('moon_nao.RDS')

# -----

get_correct_temp <- function(T0, T1, s, bs) {
  T0 - ((T0 - T1)/(1 - exp(-bs/s)))
}

plot_daily_dives <- function(data, bird, date, min_time, max_time) {
  
  tt <- data %>%
    filter(Band == bird, Date == date) %>%  
    mutate(
      Light_prop = ifelse(Behaviour %in% c('Diving','Resting'), NA, Light - 100),
      Light_prop = imputeTS::na_interpolation(Light_prop),
      Light_prop = Light_prop/100,
      Light_prop = ifelse(Light_prop > 1, 1, Light_prop),
      Light_prop = ifelse(Light_prop < 0, 0, Light_prop),
      Light_prop = zoo::rollapply(Light_prop, 60, min, partial = T),
      Light_prop = 1 - Light_prop
    )
  
  myGrid <- expand.grid(Depth = seq(-10, 200, 1),
                        Time = seq.POSIXt(min(tt$Time), max(tt$Time), by = '10 min'))
  myGrid <- merge(myGrid, tt[,c('Time', 'Light_prop')])
  
  ggplot(tt, aes(x = Time, y = Depth)) +
    geom_tile(data = myGrid, aes(x = Time, y = Depth, fill = Light_prop), alpha = 0.7) +
    geom_line() +
    geom_vline(xintercept = c(min_time,max_time), linetype = 2) +
    scale_y_continuous(trans = 'reverse', expand = c(0,0)) +
    scale_x_datetime(expand = c(0,0), date_labels = "%H:%M") +
    coord_cartesian(xlim = c(min_time,max_time), ylim = c(200, -10)) +
    scale_fill_viridis_c(begin = 0.2, end = 1, option = 'cividis', direction = -1) +
    labs(x = 'Time (UTC)', y = 'Depth(m)',
         title = paste('Dive activity of', bird, 'on', date),
         subtitle = 'Shading indicates time of day (yellow - day, blue - night)') +
    guides(fill = F)
}

plot_dive_profile <- function(data = dat, lag_time = 4,
                              bird = '99608459', date = as.Date('2018-03-03'),
                              min_time = as.POSIXct('2018-03-03 00:30:00', tz = 'UTC'), 
                              max_time = as.POSIXct('2018-03-03 23:30:00', tz = 'UTC')) {
  
  tt <- data %>%
    filter(Band == bird, Date == date) %>% 
    filter(Time >= min_time, Time <= max_time) %>% 
    mutate(
      Temp_cor = get_correct_temp(T0 = lag(Temp, 1), T1 = Temp , s = 10, bs = lag_time)
    ) %>%
    filter(In_bout == 1) %>%
    select(Band, Time, Temp_cor, Temp, Depth)
  
  p <- ggplot(tt, aes(y = Temp_cor, x = Depth)) +
    geom_path(col = grey(0.5), alpha = 0.5, size = 0.5) +
    geom_smooth(se = F, method = 'gam') +
    geom_vline(xintercept = 0) +
    coord_flip() +
    scale_x_continuous(trans = 'reverse') +
    scale_y_continuous(lim = c(min(tt$Temp_cor) - 1, max(tt$Temp_cor) + 1)) +
    labs(y = 'Temperature (\u00B0C)', x = 'Depth (m)',
         title = paste('Temperature profile'),
         subtitle = 'Grey lines are individual dives, blue line is mean profile estimated with a GAM')
  
  p
}

plot_light_profile <- function(data = dat, bird = '118600758', date = as.Date('2018-01-01'),
                               min_time = as.POSIXct('2018-01-01 00:30:00', tz = 'UTC'), 
                               max_time = as.POSIXct('2018-01-01 23:30:00', tz = 'UTC')) {
  
  tt <- data %>%
    filter(Band == bird, Date == date) %>% 
    filter(Time >= min_time, Time <= max_time) %>% 
    # mutate(
    #   Temp_cor = get_correct_temp(T0 = lag(Temp, 1), T1 = Temp , s = 10, bs = lag_time)
    # ) %>%
    filter(In_bout == 1) %>%
    select(Band, Time, Light, Temp, Depth)
  
  p <- ggplot(tt, aes(y = Light, x = Depth)) +
    geom_path(col = grey(0.5), alpha = 0.5, size = 0.5) +
    geom_smooth(se = F, method = 'gam') +
    geom_vline(xintercept = 0) +
    coord_flip() +
    scale_x_continuous(trans = 'reverse') +
    scale_y_continuous(lim = c(0,350)) +
    labs(y = 'Light', x = 'Depth (m)',
         title = paste('Light profile'),
         subtitle = 'Grey lines are individual dives, blue line is mean profile')
  
  p
}

plot_location <- function(data = locs, bird, 
                          date, base = coast) {
  
  temp <- data %>% 
    filter(Band == bird)
  
  ggplot() +
    geom_sf(data = base, fill = grey(0.7), col = 'transparent', size = 0) +
    geom_path(data = temp, aes(x = lon, y = lat), col = 'blue', linetype = 3, size = 0.5) +
    geom_point(aes(x = -82.01, y = 62.95), size = 4, col = 'red', shape = 15) +
    geom_point(data = temp[temp$Date == date,], aes(x = lon, y = lat), col = 'blue', size = 4) +
    scale_x_continuous(expand = c(0,0), breaks = seq(-100, 0, 20)) +
    scale_y_continuous(expand = c(0,0), breaks = seq(0, 90, 10)) +
    labs(x = '', y = '',
         title = paste('GLS estimated track for', bird),
         subtitle = paste0('Blue point shows the location on ', date, ', red square is colony'))
  
}

plot_moon <- function(date) {
  yy <- strftime(date,'%Y')
  
  temp <- subset(moon_nao, moon_nao$Year == yy)
  
  moon <- ggplot(temp, aes(x = Date, y = Moon)) +
    geom_line() +
    geom_point(data = temp[temp$Date == date,], aes(x = Date, y = Moon), size = 3) +
    labs(x = 'Date', y = 'Moon illumination')
  
  nao <- ggplot(temp, aes(x = Date, y = NAO)) +
    geom_line() +
    geom_point(data = temp[temp$Date == date,], aes(x = Date, y = NAO), size = 3) +
    scale_y_continuous(lim = c(-2,2)) +
    labs(x = 'Date', y = 'NAO')
  
  cowplot::plot_grid(moon, nao, nrow = 2)
}


# -----

plot_temp_raster <- function(data = dat, bird = '99670779', date = as.Date('2018-03-17'),
                             min_time = as.POSIXct('2018-03-17 00:30:00', tz = 'UTC'), 
                             max_time = as.POSIXct('2018-03-17 23:30:00', tz = 'UTC')) {
  
  tt <- data %>%
    filter(Band == bird, Date == date, Behaviour == 'Diving') %>% 
    filter(Time >= min_time, Time <= max_time) %>% 
    mutate(
      Depth_class = (floor(Depth/5) * 5) - 2.5
    ) %>% group_by(Depth_class) %>% 
    summarise(
      Dur = (n() * 10)/(60),
      Temp = mean(Temp)
    ) 
  
  temp_diff <- max(tt$Temp) - min(tt$Temp)
  if (temp_diff > 3) {
    temp_min <- floor(min(tt$Temp))
    temp_max <- ceiling(max(tt$Temp))
  } else {
    temp_min <- (min(tt$Temp) + temp_diff/2) - 1.5
    temp_max <- (min(tt$Temp) + temp_diff/2) + 1.5
  }
  
  ggplot(tt, aes(y = Dur, x = Depth_class, fill = Temp)) +
    geom_bar(stat = 'identity', width = 5) +
    scale_x_reverse() +
    coord_flip() +
    scale_fill_viridis_c(option = 'B', lim = c(temp_min, temp_max)) +
    labs(title = 'Time spent in each 5 m depth class',
         subtitle = 'Bar colour indicates mean water temperature',
         y = 'Time (min)', x = 'Depth (m)',
         fill = 'Temp (\u00B0C)') +
    theme(
      legend.background = element_blank()
    )
  
}


