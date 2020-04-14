#' Daily Bike Sharing Data
#'
#' This dataset contains the daily count of
#'  rental bike transactions between years 2011 and 2012 in Capital bikeshare
#'  system with the corresponding weather and seasonal information.
#'
#' @format A tibble: 731 x 16
#' - instant: record index
#' - dteday : date
#' - season : season (1:winter, 2:spring, 3:summer, 4:fall)
#' - yr : year (0: 2011, 1:2012)
#' - mnth : month ( 1 to 12)
#' - hr : hour (0 to 23)
#' - holiday : weather day is holiday or not (extracted from [Web Link])
#' - weekday : day of the week
#' - workingday : if day is neither weekend nor holiday is 1, otherwise is 0.
#' - weathersit :
#'     - 1: Clear, Few clouds, Partly cloudy, Partly cloudy
#'     - 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
#'     - 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
#'     - 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
#' - temp : Normalized temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-8, t_max=+39 (only in hourly scale)
#' - atemp: Normalized feeling temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-16, t_max=+50 (only in hourly scale)
#' - hum: Normalized humidity. The values are divided to 100 (max)
#' - windspeed: Normalized wind speed. The values are divided to 67 (max)
#' - casual: count of casual users
#' - registered: count of registered users
#' - cnt: count of total rental bikes including both casual and registered
#'
#' @references Fanaee-T, Hadi, and Gama, Joao,
#'  'Event labeling combining ensemble detectors and background knowledge',
#'   Progress in Artificial Intelligence (2013): pp. 1-15, Springer Berlin Heidelberg,
#'   [Web Link](https://link.springer.com/article/10.1007/s13748-013-0040-3).
#'
#'
#' @examples
#' bike_sharing_daily
#'
"bike_sharing_daily"

