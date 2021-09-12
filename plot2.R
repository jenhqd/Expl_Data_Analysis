dl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if(!file.exists('data.zip')) {download.file(dl, destfile = 'data.zip', method = 'curl')}
unzip('data.zip')
list.files()

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

scc <- read_rds("Source_Classification_Code.rds")
pm25 <- read_rds("summarySCC_PM25.rds")
file.remove(c("data.zip","Source_Classification_Code.rds", "summarySCC_PM25.rds"))

head(scc)
sapply(scc, class)
head(pm25)
sapply(pm25, class)

baltimore <- pm25 %>% filter(fips == '24510') %>%
  group_by(year) %>% summarize(Total_Emissions = sum(Emissions))

png(file = 'plot2.png', width = 480, height = 480)
with(baltimore, plot(x=year, y=Total_Emissions, pch = 19, col = 'steelblue',
                        xlab = 'Year', ylab = 'Total PM2.5 Emission (million)',
                        main = 'Total PM2.5 in Baltimore City by Year', xaxt = 'n'))
lines(baltimore$year, baltimore$Total_Emissions, col = 'steelblue')
axis(1, at = baltimore$year, labels = baltimore$year)
dev.off()






