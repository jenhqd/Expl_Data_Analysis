dl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if(!file.exists('data.zip')) {download.file(dl, destfile = 'data.zip', method = 'curl')}
unzip('data.zip')
list.files()

library(dplyr)
library(readr)
library(ggplot2)
library(ggprism)
library(stringr)

scc <- read_rds("Source_Classification_Code.rds")
pm25 <- read_rds("summarySCC_PM25.rds")
file.remove(c("data.zip","Source_Classification_Code.rds", "summarySCC_PM25.rds"))

head(scc)
sapply(scc, class)
scc <- scc %>% mutate_at(c(1,3,4), as.character)
head(pm25)
sapply(pm25, class)

vehicle <- scc %>% filter(grepl('vehicle', EI.Sector,ignore.case = TRUE)) %>% select(SCC, EI.Sector)
vehicle <- vehicle %>% rowwise() %>% mutate(EI.Sector= str_c(word(EI.Sector, c(4:7)), collapse = ' '))
baltimore_vehicle <- pm25 %>% filter((fips == '24510') & (SCC %in% vehicle[['SCC']]))
baltimore_vehicle <- left_join(baltimore_vehicle, vehicle, by = 'SCC')
baltimore_vehicle$EI.Sector <- as.factor(baltimore_vehicle$EI.Sector)

ggplot(baltimore_vehicle, aes(x = year, y = Emissions, col = EI.Sector, group = EI.Sector)) +
  stat_summary(fun = sum, geom = 'point', size = 3) +
  stat_summary(fun = sum, geom = 'line') +
  ggtitle(expression(~ PM[2.5]~ 'in Baltimore City from Motor Vehicles')) +
  labs(color = 'Vehicle Type:', x = 'Year', y = expression('Total' ~PM[2.5]~ 'Emission')) +
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.background=element_rect(fill = "transparent",color = NA),
        plot.title = element_text(hjust = 0.5), aspect.ratio = 1/1)
ggsave('plot5.png')