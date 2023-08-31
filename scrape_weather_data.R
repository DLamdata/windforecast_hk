library(curl)
library(XML)
library(data.table)
library(lubridate)
library(ggplot2)
library(scales)

# Scrape and plot Met.no forecasts----

locationforecast_classic <- function (lat, lon, 
                                      elevation = NULL, location = NULL, exact = TRUE, 
                                      tz = "Asia/Hong_Kong", key = NULL) 
{
  if (!is.null(location)) {
    latlon = as.numeric(rev(geocode(location = location, 
                                    source = "google")))
    if (any(is.null(latlon))) 
      stop("Error: no match location")
    lat = latlon[1]
    lon = latlon[2]
    elevation = jsonlite::fromJSON(
      paste0("http://maps.googleapis.com/maps/api/elevation/json?locations=", lat, ",", lon, "&sensor=false&key=", key)
      )$results[[1]]$elevation
  }
  if (any(!is.numeric(c(lat, lon, elevation)))) 
    stop("Error: lat, lon and elevation have to be numeric")
  lat = lat[1]
  lon = lon[1]
  elevation = elevation[1]
  msl = ifelse(is.null(elevation), 
               "", 
               paste0(";msl=", round(elevation, digits = 0)))
  url = paste0("https://api.met.no/weatherapi/locationforecast/2.0/classic?lat=", 
               lat, ";lon=", lon, msl)
  x = paste0(paste0(readLines(url, warn = F), collapse = "\n"), "\n")
  x = xmlRoot(xmlParse(x))
  
  # 2022-11-01 DL added termin, runended 
  termin = lubridate::with_tz(
    time = strptime(x = xpathSApply(x, "//model/@termin"), 
                    format = "%Y-%m-%dT%H:%M:%S", 
                    tz = "GMT"), 
    tzone = tz
  )
  names(termin) <- NULL # otherwise data.frame() has a warning
  runended = lubridate::with_tz(
    time = strptime(x = xpathSApply(x, "//model/@runended"),
                    format = "%Y-%m-%dT%H:%M:%S", 
                    tz = "GMT"), 
    tzone = tz
  )
  names(runended) <- NULL
  timefrom = lubridate::with_tz(
    time = strptime(x = xpathSApply(x, "//time/@from"), 
                    format = "%Y-%m-%dT%H:%M:%S", 
                    tz = "GMT"), 
    tzone = tz
  )
  timeto = lubridate::with_tz(
    time = strptime(x = xpathSApply(x, "//time/@to"), 
                    format = "%Y-%m-%dT%H:%M:%S", 
                    tz = "GMT"), 
    tzone = tz
  )
  tdiff = difftime(timeto, timefrom, units = "hours")
  if (exact) {
    v = which(tdiff == 0)
    alltime = xpathSApply(x, "//time")
    allexact = sapply(v, function(y) alltime[[y]])
    temp = sapply(allexact, function(z) list(point = xmlToList(z[[1]])))
    temperature = as.numeric(sapply(1:length(temp), function(x) {
      x1 = temp[[x]]$temperature["value"]
      ifelse(!is.null(x1), x1, NA)
    }))
    windDirection = as.numeric(sapply(1:length(temp), function(x) {
      x1 = temp[[x]]$windDirection["deg"]
      ifelse(!is.null(x1), x1, NA)
    }))
    windSpeed_mps = as.numeric(sapply(1:length(temp), function(x) {
      x1 = temp[[x]]$windSpeed["mps"]
      ifelse(!is.null(x1), x1, NA)
    }))
    windSpeed_beaufort = as.numeric(sapply(1:length(temp), 
                                           function(x) {
                                             x1 = temp[[x]]$windSpeed["beaufort"]
                                             ifelse(!is.null(x1), x1, NA)
                                           }))
    windSpeed_name = sapply(1:length(temp), function(x) {
      x1 = temp[[x]]$windSpeed["name"]
      ifelse(!is.null(x1), x1, NA)
    })
    windGust = as.numeric(sapply(1:length(temp), function(x) {
      x1 = temp[[x]]$windGust["mps"]
      ifelse(!is.null(x1), x1, NA)
    }))
    humidity = as.numeric(sapply(1:length(temp), function(x) {
      x1 = temp[[x]]$humidity["value"]
      ifelse(!is.null(x1), x1, NA)
    }))
    pressure = as.numeric(sapply(1:length(temp), function(x) {
      x1 = temp[[x]]$pressure["value"]
      ifelse(!is.null(x1), x1, NA)
    }))
    cloudiness = as.numeric(sapply(1:length(temp), function(x) {
      x1 = temp[[x]]$cloudiness["percent"]
      ifelse(!is.null(x1), x1, NA)
    }))
    lowClouds = as.numeric(sapply(1:length(temp), function(x) {
      x1 = temp[[x]]$lowClouds["percent"]
      ifelse(!is.null(x1), x1, NA)
    }))
    mediumClouds = as.numeric(sapply(1:length(temp), function(x) {
      x1 = temp[[x]]$mediumClouds["percent"]
      ifelse(!is.null(x1), x1, NA)
    }))
    highClouds = as.numeric(sapply(1:length(temp), function(x) {
      x1 = temp[[x]]$highClouds["percent"]
      ifelse(!is.null(x1), x1, NA)
    }))
    dewpointTemperature = as.numeric(sapply(1:length(temp), 
                                            function(x) {
                                              x1 = temp[[x]]$dewpointTemperature["value"]
                                              ifelse(!is.null(x1), x1, NA)
                                            }))
    data.frame(forecast_termin = termin, runended = runended, time = timefrom[v], temperature = temperature, 
               windDirection = windDirection, windSpeed_mps = windSpeed_mps, 
               windSpeed_beaufort = windSpeed_beaufort, windSpeed_name = windSpeed_name, 
               windGust = windGust, humidity = humidity, pressure = pressure, 
               cloudiness = cloudiness, lowClouds = lowClouds, 
               mediumClouds = mediumClouds, highClouds = highClouds, 
               dewpointTemperature = dewpointTemperature)
  } else {
    v = which(tdiff != 0)
    alltime = xpathSApply(x, "//time")
    allexact = sapply(v, function(y) alltime[[y]])
    temp = sapply(allexact, function(z) list(point = xmlToList(z[[1]])))
    precipitation = as.numeric(sapply(1:length(temp), function(x) {
      x1 = temp[[x]]$precipitation["value"]
      ifelse(!is.null(x1), x1, NA)
    }))
    minTemperature = as.numeric(sapply(1:length(temp), function(x) {
      x1 = temp[[x]]$minTemperature["value"]
      ifelse(!is.null(x1), x1, NA)
    }))
    maxTemperature = as.numeric(sapply(1:length(temp), function(x) {
      x1 = temp[[x]]$maxTemperature["value"]
      ifelse(!is.null(x1), x1, NA)
    }))
    weather_id = sapply(1:length(temp), function(x) {
      x1 = temp[[x]]$symbol["id"]
      ifelse(!is.null(x1), x1, NA)
    })
    data.frame(forecast_termin = termin, runended = runended, timefrom = timefrom[v], timeto = timeto[v], 
               interval = tdiff[v], precipitation = precipitation, 
               minTemperature = minTemperature, maxTemperature = maxTemperature, 
               weather_id = weather_id, stringsAsFactors = F)
  }
}

hourly_day_start <- function(x) {
  ifelse(is.na(shift(x))|day(x)!=shift(day(x)), 
         format(x, "%H:%M\n%d %b"), 
         format(x, "%H:%M"))
}  

coords <- data.table(
  loc = c("Stanley", "HVRC", "Ngong_Ping"), 
  lat = c(22.2204, 22.2723, 22.258611), 
  lon = c(114.2127, 114.1095, 113.912778)
  )

dt_list <- lapply(
  coords$lat, 
  FUN = locationforecast_classic, coords$lon
)

dt_list <- lapply(
  dt_list, 
  FUN = function(x) setDT(x)[ , `:=` (windSpeed_kmh = windSpeed_mps*(60*60)/1000, 
                                      row_id = seq_len(.N))]
)

names(dt_list) <- coords$loc

for(i in coords$loc) dt_list[[i]][ , location := i]

dt <- rbindlist(dt_list)

forecast_termin_HKT <- min(unique(dt$forecast_termin))

# time_period = format(time_period_date, '%Y%m%d')

data.table::fwrite(x = dt, 
                   file = file.path('data', paste0('hongkong_classic_', format(forecast_termin_HKT, "%Y%m%dT%H%M"), '.csv')))

# Plot forecast
midnights <- seq(
  as.POSIXct(paste0(Sys.Date(), "00:00:00"), tz="Asia/Hong_Kong"), 
  by = "1 days", 
  length.out = 10
)

dt[ , location := factor(location, levels = c("Stanley", "HVRC", "Ngong_Ping"))]

plot_wind_overlay <- function(data = NULL, 
                              hours = 48, # n_breaks = 20
                              hours_per_break = 3) {
  
  ggplot(data = data[time <= (midnights[1] + hours*60*60)], 
         mapping = aes(x = time, y = windSpeed_kmh, color = location)) + 
    geom_vline(xintercept = as.numeric(midnights), linetype = "solid", color = "grey80") + # Set to numeric because ggplotly fails to plot POSIXct objects
    geom_point(size = 2, alpha = 0.5) + 
    geom_line(alpha = 0.5) + 
    scale_x_datetime(breaks = seq.POSIXt(from = midnights[1], 
                                         to = midnights[1] + 60*60*24*10, 
                                         by = 60*60*hours_per_break), 
                     labels = hourly_day_start, 
                     limits = c(midnights[1], midnights[1] + 60*60*hours), 
                     expand = expansion(c(0.02,0.02),0)) + 
    scale_y_continuous(breaks = seq(0,500,by=10), 
                       limits = c(0,NA), 
                       expand=expansion(c(0,0.05),0)) + 
    theme_bw() + 
    theme(axis.text.x = element_text(hjust=0), 
          panel.grid.major.y = element_line(color = "grey84"), 
          panel.grid.major.x = element_line(color = "grey84", 
                                            linetype = "dashed", 
                                            linewidth = 0.4)) + 
    labs(title = paste0("met.no forecast at ", forecast_termin_HKT, " HKT"), 
         subtitle = "Forecast is based on the HRES model from ECMWF.", 
         x = "Date and Time (HKT)", 
         color = "Location")
  
}

ggsave(paste0('plots/','wind_5d_', format(forecast_termin_HKT, '%Y%m%dT%H%M'), '.png'), 
       plot = plot_wind_overlay(data = dt, hours = 120, hours_per_break = 6), 
       width = 12, height = 4)

ggsave(paste0('plots/','wind_3d_', format(forecast_termin_HKT, '%Y%m%dT%H%M'), '.png'), 
       plot = plot_wind_overlay(data = dt, hours = 72, hours_per_break = 6), 
       width = 8, height = 4)


# Scrape Open-Meteo Ensemble API----

curr_hour <- lubridate::floor_date(Sys.time(), unit = "hours")

attr(curr_hour, "tzone") <- "Asia/Hong_Kong"

ensemble_csv = data.table::fread(
  input = 'https://ensemble-api.open-meteo.com/v1/ensemble?latitude=22.2204&longitude=114.2127&hourly=temperature_2m,relativehumidity_2m,rain,windspeed_10m,windgusts_10m&timezone=Asia%2FSingapore&models=icon_seamless,gfs_seamless,ecmwf_ifs04&format=csv', 
  skip = 3
)

ensemble_csv[ , `:=` (latitude = 22.2204, 
                      longitude = 114.2127)]

data.table::fwrite(x = ensemble_csv, 
                   file = file.path('data', 'ensemble', paste0('ensemble_', format(curr_hour, '%Y%m%dT%H%M'), '.csv')))

windcols = names(ensemble_csv)[grepl('windspeed_', names(ensemble_csv))]
cols = c('time', windcols)

dt = ensemble_csv[ , ..cols]

names(dt) = sub(' \\(km/h\\)', '', names(dt))

mdt = melt(dt, id.vars = 'time')

mdt[ , time := as.POSIXct(time, format = "%Y-%m-%dT%H:%M", tz = "Asia/Hong_Kong")]

mdt[grepl('_icon_seamless', variable), model := 'DWD Icon']
mdt[grepl('_ecmwf_ifs04', variable), model := 'ECMWF IFS']
mdt[grepl('_gfs_seamless', variable), model := 'GFS']

mdt[ , forecast := ifelse(grepl('10m_icon_|10m_gfs_|10m_ecmwf_', variable), 
                          "Point", "Ensemble")]

p = ggplot(data = mdt, 
           mapping = aes(x = time, y = value, color = model)) + 
  geom_vline(xintercept = as.numeric(midnights), linetype = "solid", color = "grey88") + # Set to numeric because ggplotly fails to plot POSIXct objects
  geom_line(aes(group = variable, alpha = forecast, linewidth = forecast)) + 
  scale_alpha_manual(values = c(0.4,1)) + 
  scale_linewidth_manual(values = c(0.4,1.2)) +
  scale_x_datetime(labels = hourly_day_start, 
                   breaks = seq.POSIXt(from = midnights[1], 
                                       to = midnights[1] + 60*60*24*10, 
                                       by = 60*60*6), 
                   limits = c(midnights[1], midnights[1] + 60*60*24*7), 
                   expand = expansion(c(0.01,0.01),0)) + 
  scale_y_continuous(breaks=seq(0,500,by=10), 
                     limits = c(0,NA), expand = expansion(c(0,0.05),0)) + 
  theme_bw() + 
  theme(legend.position = 'top', 
        legend.key.width = unit(2, "lines"), 
        axis.text.x = element_text(hjust = 0), 
        panel.grid.major.y = element_line(color = "grey88"), 
        panel.grid.major.x = element_line(color = "grey92", linetype = "dashed")) + 
  guides(color = guide_legend(override.aes = list(linewidth = 1.2))) + 
  labs(y = 'Wind Speed 10m (km/h)', 
       x = NULL, 
       color = "Weather model", 
       alpha = "Forecast", linewidth = "Forecast", 
       title = 'Ensemble Model Wind Forecasts for Hong Kong', 
       subtitle = 'Wind Speed at 10m elevation in Stanley, Hong Kong (22.2204, 114.2127).\nFrom Open-Meteo API.') 

ggsave(filename = file.path('plots', 'ensemble', paste0('wind_10m_', format(curr_hour, '%Y%m%dT%H%M'), '.png')), 
       plot = p, 
       width = 12, 
       height = 6)