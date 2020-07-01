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

df_start$trimmed_expenses = df_start$expenses
for(i in 1:nrow(df_start)) {
  if (!is.na(df_start$trimmed_expenses[i]) && df_start$trimmed_expenses[i] > 1000) {
    df_start$trimmed_expenses[i] = 1000
  }
}
ggplot(data=subset(df_start, !is.na(trimmed_expenses)), aes(x=trimmed_expenses)) +
  geom_histogram(bins = 10, fill = primary[1]) + 
  theme_hodp() +
  labs(title="Out of pocket expenses from moving") +
  scale_x_continuous(breaks = (0:10) * 100, labels = c(paste("$", 0:9 * 100, sep = ""), "$1000+")) +
  ylab("Count") +
  xlab("Expenses")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

sum(df_start$expenses >= 1000, na.rm = T) / length(df_start$expenses[!is.na(df_start$expenses)])

ggplot(data=df_start, aes(x=factor(workspace))) + 
  geom_bar(aes(fill = factor(workspace))) + 
  scale_fill_manual(values = c(primary[4], primary[3], primary[1], primary[2])) + 
  scale_x_discrete(labels = str_wrap(c("Reliable internet access and quiet space to work",
                                       "Only reliable internet access",
                                       "Only quiet space to work",
                                       "Neither quiet space to work nor reliable internet"), width = 20), 
                   limits = c("I have reliable internet access and a quiet space to work", 
                              "I have reliable internet access but not a quiet space to work", 
                              "I have a quiet space to work but not reliable internet access", 
                              "I have neither a quiet space to work nor reliable internet access")) +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  ylim(c(0, 300)) + 
  xlab("Working conditions") + 
  ylab("Count") + 
  labs(title="Student working conditions") +
  theme_hodp() +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

ggplot(data=subset(df_start, enrollment_changes != ""), aes(x=factor(enrollment_changes))) + 
  geom_bar(aes(fill = factor(enrollment_changes))) + 
  scale_fill_manual(values = c(primary[3], primary[2], primary[4], primary[1])) + 
  scale_x_discrete(labels = str_wrap(c("No enrollment changes",
                                       "Changed class to pass/fail",
                                       "Dropped a class",
                                       "Changed to pass/fail and dropped classes"), width = 20), 
                   limits = c("I have not made grading or enrollment changes", 
                              "I have changed classes to pass/fail", 
                              "I have dropped classes", 
                              "Both")) +
  ylim(c(0, 350)) + 
  xlab("Changes") + 
  ylab("Count") + 
  labs(title="Course changes") +
  theme_hodp() +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

ggplot(data=subset(df_start, flu_symptoms != ""), aes(x=factor(flu_symptoms))) + 
  geom_bar(aes(fill = factor(flu_symptoms))) + 
  scale_fill_manual(values = c(primary[2], primary[1], primary[4])) + 
  scale_x_discrete(labels = str_wrap(c("No symptoms",
                                       "Mild symptoms",
                                       "Severe symptoms"), width = 20), 
                   limits = c("No", 
                              "Mild symptoms", 
                              "Severe symptoms")) +
  ylim(c(0, 350)) + 
  xlab("Experienced flu symptoms") + 
  ylab("Count") + 
  labs(title="Flu symptoms among students") +
  theme_hodp() +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

ggplot(data=subset(df_start, tested != ""), aes(x=factor(tested))) + 
  geom_bar(aes(fill = factor(tested))) + 
  scale_fill_manual(values = c(primary[1], primary[2])) + 
  scale_x_discrete(labels = str_wrap(c("No",
                                       "Yes"), width = 20), 
                   limits = c("No", 
                              "Yes")) +
  ylim(c(0, 450)) + 
  xlab("Tested for COVID-19") + 
  ylab("Count") + 
  labs(title="Students tested for COVID") +
  theme_hodp() +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))


ggplot(data=subset(df_start, self_quarantined != ""), aes(x=factor(self_quarantined))) + 
  geom_bar(aes(fill = factor(self_quarantined))) + 
  scale_fill_manual(values = c(primary[1], primary[2], primary[4])) + 
  scale_x_discrete(labels = str_wrap(c("No self-quarantine",
                                       "Self-quarantined from friends",
                                       "Self-quarantined from friends and family"), width = 20), 
                   limits = c("I have not self-quarantined after leaving campus", 
                              "I have self-quarantined from friends, but not from immediate family",
                              "I have self-quarantinined from family and friends")) +
  ylim(c(0, 350)) + 
  xlab("Self-quarantined") + 
  ylab("Count") + 
  labs(title="Students self-quarantining at home") +
  theme_hodp() +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))


# made a mistke in the survey, have to readjust harvard low from 0 to 1
df_start$harvard_handling_mod <- df_start$harvard_handling
for(i in 1:nrow(df_start)) {
  if(df_start$harvard_handling_mod[i] == 0 && !is.na(df_start$harvard_handling_mod[i])) {
    df_start$harvard_handling_mod[i] = 1
  }
}

ggplot(data=subset(df_start, !is.na(harvard_handling_mod)), aes(x=harvard_handling_mod)) +
  geom_histogram(bins = 10, fill = primary[1]) + 
  theme_hodp() +
  labs(title="Harvard Handling Rating") +
  scale_x_continuous(breaks = 1:10) +
  ylab("Count") +
  xlab("Rating")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

ggplot(data=subset(df_start, !is.na(govt_handling)), aes(x=govt_handling)) +
  geom_histogram(bins = 10, fill = primary[1]) + 
  theme_hodp() +
  labs(title="Government Handling Rating") +
  scale_x_continuous(breaks = 1:10) +
  ylab("Count") +
  xlab("Rating")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))

ggplot(data=subset(df_start, normal_estimate != ""), aes(x=factor(normal_estimate))) + 
  geom_bar(aes(fill = factor(normal_estimate))) + 
  scale_fill_manual(values = c(primary[1], primary[1], primary[1], primary[1], primary[1], primary[1])) + 
  scale_x_discrete(labels = str_wrap(c("<1 month", 
                                         "2-3 months",
                                         "3-6 months",
                                         "6-12 months",
                                         "12+ months",
                                         "Never"), width = 20), 
                   limits = c("Less than a month", 
                              "2-3 months",
                              "3-6 months",
                              "6-12 months",
                              "12+ months",
                              "Never")) +
  ylim(c(0, 250)) + 
  xlab("Estimate until back to normal") + 
  ylab("Count") + 
  labs(title="How long until back to normal") +
  theme_hodp() +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))


ggplot(data=subset(df_start, preferred_grading != ""), aes(x=factor(preferred_grading))) + 
  geom_bar(aes(fill = factor(preferred_grading))) + 
  scale_fill_manual(values = c(primary[4], primary[1], primary[3], primary[2])) + 
  scale_x_discrete(labels = str_wrap(c("Optional pass/fail",
                                       "Universal pass/fail",
                                       "Universal A/A-",
                                       "Other"), width = 20), 
                   limits = c("Optional pass/fail", 
                              "Universal pass/fail",
                              "Universal A/A-",
                              "None of the above")) +
  ylim(c(0, 350)) + 
  xlab("Grading System") + 
  ylab("Count") + 
  labs(title="Students preferred grading system in March") +
  theme_hodp() +
  geom_text(stat='count', aes(label=percent((..count..)/sum((..count..)))), vjust=-1) +
  theme(legend.position = "none")
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))
