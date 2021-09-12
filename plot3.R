dl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if(!file.exists('data.zip')) {download.file(dl, destfile = 'data.zip', method = 'curl')}
unzip('data.zip')
list.files()

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(ggprism)

scc <- read_rds("Source_Classification_Code.rds")
pm25 <- read_rds("summarySCC_PM25.rds")
file.remove(c("data.zip","Source_Classification_Code.rds", "summarySCC_PM25.rds"))

head(scc)
sapply(scc, class)
head(pm25)
sapply(pm25, class)

baltimore_1 <- pm25 %>% filter(fips == '24510') %>% group_by(year, type) %>%
  summarize(Emissions = sum(Emissions))

baltimore_1$type <- as.factor(baltimore_1$type)
baltimore_1$year <- as.factor(baltimore_1$year)

theme_set(theme_bw())

# ggplot(baltimore_1, aes(x=year, y = Emissions, fill = type)) + 
#   geom_boxplot(outlier.shape = NA) +
#   scale_y_continuous(breaks = seq(-5,70,10), limits = c(-5,70), expand = c(0,0),
#                      guide = 'prism_minor', minor_breaks = seq(-5,70,5)) +
#   labs(x = 'Year', y = 'PM2.5', fill = 'Collected site:')
# 
# ggplot(baltimore_1, aes(x = year, y = Emissions, fill = type)) + geom_boxplot(outlier.shape = NA) +
#   facet_wrap(~type) + scale_y_continuous(breaks = seq(-5,70,10), limits = c(-5,70), expand = c(0,0),
#                                          guide = 'prism_minor', minor_breaks = seq(-5,70,5)) +
#   labs(x = 'Year', y = 'PM2.5', fill = 'Collected site:')
# 
# ggplot(baltimore_1, aes(x = year, y = Emissions, fill = type)) + geom_bar(stat='identity') +
#   facet_wrap(~type) + labs(x = 'Year', y = 'PM2.5', fill = 'Collected site:')

ggplot(baltimore_1, aes(x = year, y = Emissions, color = type, group = type)) +
  geom_point() + geom_line() +
  labs(x = 'Year', y = 'PM2.5', color= 'Collected site:') +
  ggtitle(expression(~ PM[2.5]~ 'in Baltimore City by groups')) +
  theme(plot.title = element_text(hjust = 0.5), aspect.ratio = 1/1) + 
  scale_y_continuous(guide = 'prism_minor', minor_breaks = seq(0,2500,100)) +
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.background=element_rect(fill = "transparent",color = NA))
ggsave(file = 'plot3.png')



