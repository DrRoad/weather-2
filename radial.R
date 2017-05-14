# Based on code by: jasonjb82
# Link: https://github.com/jasonjb82/R-visuals/blob/master/Code/Weather_Radial.R

# Libraries
library(dplyr)
library(lubridate)
library(weatherData)
library(ggplot2)
library(viridis)
library(scales)
library(grid)
library(gridExtra)
library(gtable)

# Theme for weather radial
theme_wr <- theme_minimal() +
  theme(text = element_text(family = "Arial Narrow", color = "#3A3F4A"),
        panel.grid = element_line(linetype = 'solid', color = 'black', size = 0.75),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 10, vjust =10),
        axis.text.y = element_blank(),
        legend.position = c(0.5, 0.47),
        legend.direction = 'horizontal',
        legend.key.height = unit(4, "pt"),
        legend.key.width = unit(20, "pt"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.title.align = 0.5,
        plot.background = element_rect(fill = "#EFF2F4",colour = NA),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))


# Produce temperature labels
lbls <- data.frame(x = rep(as.Date('2016-07-01'), 5), y = seq(40, 80, 10), label = seq(40, 80, 10))

# Download data from Weather Underground using the weatherData package and reformat data accordingly

# Seattle
sea16 <- getWeatherForDate("KBFI", "2016-01-01", end_date = "2016-12-31", opt_all_columns = TRUE)

sea16$date <- as.Date(sea16$Date)
sea16$PrecipitationIn <- as.numeric(sea16$PrecipitationIn)

# Plot using ggplot2
seattle <- 
  ggplot(sea16) +
  geom_hline(yintercept = 0, color = 'gray', size = 0.5) +
  geom_linerange(aes(x = date, ymin = Min_TemperatureF, ymax = Max_TemperatureF, 
                     color = Mean_TemperatureF), size = 1) +
  geom_text(aes(x = as.Date('2016-01-01'), y = 70, label = 'Seattle',
                family = "Arial Narrow"), size = 6, color = "#3A3F4A") +
  geom_text(aes(x = as.Date('2016-01-01'), y = 60, label = 'Weather in 2016 (source: Weather Underground)',
                family = "Arial Narrow"), size = 3, color = "#3A3F4A")+
  geom_point(data = sea16, aes(x = date, y = 90, size = PrecipitationIn),
             alpha = 0.25, color = "deepskyblue2") +
  scale_fill_gradient(guide = FALSE) +
  scale_size_area(max_size = 15, guide = guide_legend(title = "Precipitation in Inches")) +
  scale_color_viridis(option = 'plasma', end = 1) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = '1 month', 
               labels = c('January', 'February', 'March', 'April', 'May', 'June',
                          'July', 'August', 'September', 'October', 'November', 'December', '')) +
  coord_polar() +
  geom_text(data = lbls, aes(x = x, y = y, label = label, family = "Arial Narrow"), 
            size = 4, color = "#3A3F4A", hjust = 0.5) +
  guides(color = guide_colorbar(title = expression("Temperature in"*~degree*F), 
                                raster = F, title.position = "top"),
         size = guide_legend(title = "Precipitation in Inches", title.position = "top")) +
  theme_wr

# Boston
bos16 <- getWeatherForDate("KBOS", "2016-01-01", end_date = "2016-12-31", opt_all_columns = TRUE)

bos16$date <- as.Date(bos16$Date)
bos16$PrecipitationIn <- as.numeric(bos16$PrecipitationIn)

# Plot using ggplot2
bos <- 
  ggplot(bos16) +
  geom_hline(yintercept = 0, color = 'gray', size = 0.5) +
  geom_linerange(aes(x = date, ymin = Min_TemperatureF, ymax = Max_TemperatureF, 
                     color = Mean_TemperatureF), size = 1) +
  geom_text(aes(x = as.Date('2016-01-01'), y = 70, label = 'Boston',
                family = "Arial Narrow"), size = 6, color = "#3A3F4A") +
  geom_text(aes(x = as.Date('2016-01-01'), y = 60, label = 'Weather in 2016 (source: Weather Underground)',
                family = "Arial Narrow"), size = 3, color = "#3A3F4A")+
  geom_point(data = bos16, aes(x = date, y = 90, size = PrecipitationIn),
             alpha = 0.25, color = "deepskyblue2") +
  scale_fill_gradient(guide = FALSE) +
  scale_size_area(max_size = 15, guide = guide_legend(title = "Precipitation in Inches")) +
  scale_color_viridis(option = 'plasma', end = 1) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = '1 month', 
               labels = c('January', 'February', 'March', 'April', 'May', 'June',
                          'July', 'August', 'September', 'October', 'November', 'December', '')) +
  coord_polar() +
  geom_text(data = lbls, aes(x = x, y = y, label = label, family = "Arial Narrow"), 
            size = 4, color = "#3A3F4A", hjust = 0.5) +
  guides(color = guide_colorbar(title = expression("Temperature in"*~degree*F), 
                                raster = F, title.position = "top"),
         size = guide_legend(title = "Precipitation in Inches", title.position = "top")) +
  theme_wr

# Plot side by side
grid.arrange(seattle, bos, ncol = 2)
