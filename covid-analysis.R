source('styleguide.R')
library(stringr)
library(scales)

df_start <- read.csv("covid_start_survey_data.csv")

ggplot(data=df_start, aes(x=factor(location))) + 
  geom_bar(aes(fill = factor(location))) + 
  scale_fill_manual(values = c(primary[2], primary[1], primary[3], primary[5], primary[4])) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10), 
                   limits = c("Family home", "A friend's house", "On campus with exemption", "Personal apartment/housing", "Other (please specify)")) +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  ylim(c(0, 400)) + 
  xlab("Location") + 
  ylab("Count") + 
  labs(title="Locations of students", fill = "Location") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

df_start$within_three <- abs(df_start$timezone + 4) >= 3

ggplot(data=subset(df_start, !is.na(timezone)), aes(x=factor(within_three))) + 
  geom_bar(aes(fill = factor(within_three))) + 
  scale_fill_manual(values = c(primary[1], primary[4])) +
  scale_x_discrete(labels = c("Less than 3 hours of East Coast", "At least 3 hours from East Coast")) +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-0.5) + 
  xlab("Whether student was within 3 hours of East Coast time") + 
  ylab("Count") + 
  labs(title="Timezones of students") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))
