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
baltimore_la <- pm25 %>% filter((fips %in% c('24510', '06037')) & (SCC %in% vehicle[['SCC']]))

baltimore_la <- left_join(baltimore_la, vehicle, by = 'SCC')
baltimore_la <- baltimore_la  %>%
  mutate(City = case_when(fips == '24510' ~ 'Baltimore', fips == '06037' ~ 'LA County')) %>%
  mutate_at(c('year', 'EI.Sector', 'City'), as.factor)

ggplot(baltimore_la, aes(year, Emissions, col = City, group = City)) +
  stat_summary(fun = sum, geom = 'point', size = 3) +
  stat_summary(fun = sum, geom = 'line') +
  ggtitle(expression(~PM[2.5]~ 'in Baltimore City and LA County from Motor Vehicles')) +
  labs(color = 'Vehicle Type:', x = 'Year', y = expression('Total' ~PM[2.5]~ 'Emission')) +
  theme(legend.background=element_rect(fill = "transparent",color = NA),
        legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5), aspect.ratio = 1/1)
ggsave('plot6.png')



