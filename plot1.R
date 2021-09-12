dl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if(!file.exists('data.zip')) {download.file(dl, destfile = 'data.zip', method = 'curl')}
unzip('data.zip')
file.remove('data.zip')
list.files()

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

scc <- read_rds("Source_Classification_Code.rds")
pm25 <- read_rds("summarySCC_PM25.rds")
file.remove(c("Source_Classification_Code.rds", "summarySCC_PM25.rds" ))

head(scc)
sapply(scc, class)
head(pm25)
sapply(pm25, class)

pm25_year <- pm25 %>% group_by(year) %>% summarize(Total_Emissions = sum(Emissions))

png(file = 'plot1.png', width = 480, height = 480)
with(pm25_year, plot(y=Total_Emissions/10^6, x=year, xaxt = 'n',yaxt = 'n', pch = 19,
                        xlab = 'Year', ylab = 'Total PM2.5 Emission', main = 'Total PM2.5 by Year',
                        col = 'steelblue'))
lines(pm25_year$year, pm25_year$Total_Emissions/10^6, col = 'steelblue')
pretty_axs <- paste(axTicks(2), 'MM', sep = ' ')
axis(1, at = pm25_year$year)
axis(2, at = axTicks(2), labels = pretty_axs)
dev.off()
