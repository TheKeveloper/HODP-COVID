source('styleguide.R')
library(tidyverse)
library(stringr)
library(ggalluvial)

df_end <- read.csv("covid_end_survey_data.csv", na.strings = c("", "NA"))

# Generic function for making a frequency bar chart
freq_plot <- function(df, x_var, options, title, x_lab, x_ticks=NULL, same_color=FALSE) {
  if (is.null(x_ticks)) {
    x_ticks <- options
  }
  if (!same_color) {
    fill <- primary[1:length(options)]
  } else {
    fill <- primary[1]
  }
  plot <- ggplot(filter(df, !is.na({{x_var}})), aes(factor({{x_var}}, levels=options))) +
    geom_bar(fill = fill) +
    ggtitle(title) +
    xlab(x_lab) +
    ylab("Count") +
    geom_text(
      aes(label=scales::percent(..count.. / nrow(filter(df_end, !is.na({{x_var}}))), accuracy = 0.1), group=1),
      stat='count',
      vjust=-0.5) +
    scale_x_discrete(labels = str_wrap(x_ticks, width=20)) +
    theme_hodp()
  
  return(plot)
}

# End-of-semester satisfaction with SEM/UEM policy
sat_unsat_options <- c("Extremely satisfied",
                       "Somewhat satisfied",
                       "Neither satisfied nor dissatisfied",
                       "Somewhat dissatisfied",
                       "Extremely dissatisfied")

sat_unsat_plot <- freq_plot(df_end,
                            sat_unsat,
                            sat_unsat_options,
                            "End-of-semester satisfaction with emergency grading system",
                            "Satisfaction with SEM/UEM policy")

sat_unsat_plot

# End-of-semester ratings of Harvard's response
harvard_handling_plot <- ggplot(data=subset(df_end, !is.na(harvard_handling)), aes(x=harvard_handling)) +
  geom_histogram(bins = 10, fill = primary[1]) + 
  theme_hodp() +
  labs(title="Harvard handling rating at end of semester") +
  scale_x_continuous(breaks = 1:10) +
  ylab("Count") +
  xlab("Rating")

harvard_handling_plot

# Changes to summer plans
plans_options <- c("Internship/Industry Work",
                   "Research/Academia",
                   "Study abroad",
                   "Travel",
                   "Other",
                   "Uncertain")

alluvial <- ggplot(filter(df_end, !is.na(prior_plans)), aes(axis1 = factor(prior_plans, levels=plans_options), axis2 = factor(current_plans, levels=plans_options))) +
  geom_alluvium(aes(fill = prior_plans), width = 1/12, show.legend = FALSE) +
  geom_stratum(width = 1/8, fill = primary[5]) +
  geom_label(stat = "stratum", infer.label = TRUE) +
  scale_x_discrete(limits = c("Plans before COVID-19", "Current plans"), expand = c(0.07, 0.07)) +
  scale_fill_manual(values=c(primary[1:4], "#ad6df9", "#86bf71")) +
  ggtitle("How COVID-19 has changed students' summer/post-grad plans") +
  ylab("Count") +
  theme_hodp() +
  theme(axis.line = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(vjust = 5),
        plot.margin = unit(c(1, 1, 0.5, 1.2),"cm"))

alluvial

# How summer plans were affected
plans_affected_options <- c("Cancelled",
                            "Delayed and/or shortened",
                            "Moved to virtual",
                            "Recruiting/search process terminated",
                            "No change")

plans_affected_plot <- freq_plot(separate_rows(df_end, plans_affected, sep=","),
                                 plans_affected,
                                 plans_affected_options,
                                 "Effects on summer/post-grad plans",
                                 "Effect")
plans_affected_plot

# Taking time off in the fall
situation_labels <- c("No consequences for taking time off",
                      "Can't return until fall 2021",
                      "Can't return until indeterminate date",
                      "Not guaranteed housing upon return",
                      "Most friends taking semester off",
                      "Most friends NOT taking semester off")

situation_options <- c("fall_off_no_conseq",
                       "fall_off_one_year",
                       "fall_off_indeterminate",
                       "fall_off_no_housing",
                       "fall_off_most_friends",
                       "fall_off_without_friends")

fall_off_df <- data.frame("situation" = rep(situation_options, times=3), 
                          "response" = c(rep("Yes", times=length(situation_options)),
                                         rep("Unsure", times=length(situation_options)),
                                         rep("No", times=length(situation_options))),
                          "prop" = rep(0, times=3*length(situation_options)))

for (row in 1:nrow(fall_off_df)) {
  current_situation <- fall_off_df[row, "situation"]
  current_response <- fall_off_df[row, "response"]
  v <- df_end[current_situation]
  fall_off_df[row, "count"] <- table(v)[current_response]
  fall_off_df[row, "prop"] <- fall_off_df[row, "count"] / length(v[!is.na(v)])
}

fall_off_plot <- ggplot(fall_off_df, aes(x=factor(situation, levels=situation_options), y=count, fill=factor(response, levels=c("Yes", "Unsure", "No")))) +
  geom_bar(stat="identity", position='dodge') +
  xlab('Hypothetical situation') +
  ylab('Count') +
  ggtitle("If fall 2020 were virtual, would students take the semester off?") +
  scale_x_discrete(labels=str_wrap(situation_labels, width=20)) +
  scale_fill_manual(values=rev(primary[1:3])) +
  geom_text(aes(label=scales::percent(prop, accuracy=1)), position=position_dodge(width=0.9), vjust=-0.5) +
  theme_hodp() +
  theme(legend.title = element_blank())

fall_off_plot

# Preferences for fall 2020 policies
fall_policies_options <- c("Fully on-campus",
                           "Only allow some students on-campus",
                           "Fully virtual",
                           "Start virtual, migrate on campus midway through semester",
                           "Cancel/Delay to Spring",
                           "No preference",
                           "Other")

fall_policies_df <- data.frame("policy" = rep(fall_policies_options, times=2),
                               "pov" = c(rep("preferred_policy", times=length(fall_policies_options)),
                                         rep("policy_if_admin", times=length(fall_policies_options))),
                               "prop" = rep(0, times=2*length(fall_policies_options)))

for (row in 1:nrow(fall_policies_df)) {
  current_policy <- fall_policies_df[row, "policy"]
  current_pov <- fall_policies_df[row, "pov"]
  v <- df_end[current_pov]
  fall_policies_df[row, "count"] <- table(v)[current_policy]
  fall_policies_df[row, "prop"] <- fall_policies_df[row, "count"] / length(v[!is.na(v)])
}

fall_policies_plot <- ggplot(fall_policies_df, aes(x=factor(policy, levels=fall_policies_options), y=count, fill=factor(pov, levels=c("preferred_policy", "policy_if_admin")))) +
  geom_bar(stat="identity", position='dodge') +
  xlab('Preferred policy') +
  ylab('Count') +
  ggtitle("Preferred policies for fall 2020") +
  scale_x_discrete(labels=str_wrap(fall_policies_options, width=20)) +
  scale_fill_manual(values=primary, labels=c("Students' own preferences", 
                                             "Policies students would implement if they were administrators")) +
  geom_text(aes(label=scales::percent(prop, accuracy=0.1)), position=position_dodge(width=0.9), vjust=-0.5) +
  theme_hodp() +
  theme(legend.title = element_blank())

fall_policies_plot

# Likelihood of returning to campus in the fall
likelihood_options <- c("Very likely",
                        "Somewhat likely",
                        "Equally likely and unlikely",
                        "Somewhat unlikely",
                        "Very unlikely")

on_campus_fall_plot <- freq_plot(df_end,
                                 on_campus_fall,
                                 likelihood_options,
                                 "Predicted likelihood of being on campus in the fall",
                                 "Likelihood")
on_campus_fall_plot

# Comfort with returning to campus in the fall
return_campus_options <- c("Very comfortable",
                           "Somewhat comfortable",
                           "Neither comfortable nor uncomfortable",
                           "Somewhat uncomfortable",
                           "Very uncomfortable")

return_campus_plot <- freq_plot(df_end,
                                return_campus,
                                return_campus_options,
                                "Comfort returning to campus in the fall",
                                "Comfort level")

return_campus_plot

# Frequency of contact with friends from school
contact_options <- c("Every day",
                     "Every 2-3 days",
                     "Every week",
                     "Less frequently than every week")

contact_df <- data.frame("frequency" = rep(contact_options, times=2),
                         "medium" = c(rep("contact_friends", times=length(contact_options)),
                                      rep("contact_friends_not_text", times=length(contact_options))),
                         "prop" = rep(0, times=2*length(contact_options)))

for (row in 1:nrow(contact_df)) {
  current_freq <- contact_df[row, "frequency"]
  current_medium <- contact_df[row, "medium"]
  v <- df_end[current_medium]
  contact_df[row, "count"] <- table(v)[current_freq]
  contact_df[row, "prop"] <- contact_df[row, "count"] / length(v[!is.na(v)])
}

contact_plot <- ggplot(contact_df, aes(x=factor(frequency, levels=contact_options), y=count, fill=factor(medium, levels=c("contact_friends", "contact_friends_not_text")))) +
  geom_bar(stat="identity", position='dodge') +
  xlab('Frequency of contact') +
  ylab('Count') +
  ggtitle("How often students have contacted their friends from school") +
  scale_x_discrete(labels=str_wrap(contact_options, width=20)) +
  scale_fill_manual(values=primary, labels=c("Through any medium", 
                                             "Through a medium other than texting/messaging")) +
  geom_text(aes(label=scales::percent(prop, accuracy=0.1)), position=position_dodge(width=0.9), vjust=-0.5) +
  theme_hodp() +
  theme(legend.title = element_blank())

contact_plot

# How often students have met others in person
met_others_options <- c("Yes, I have done so regularly",
                        "Yes, I have done so infrequently",
                        "No, I have not met in person with anyone outside my household")

met_others_plot <- freq_plot(df_end,
                             met_others,
                             met_others_options,
                             "How often students have met in person with others",
                             "Frequency",
                             x_ticks = c("Regularly", "Infrequently", "Never"))
met_others_plot

# Time to go back to normal
normal_time_options <- c("1-2 months",
                         "3-6 months",
                         "6-12 months",
                         "12-24 months",
                         "24+ months",
                         "This is the new normal")

normal_time_plot <- freq_plot(df_end,
                              normal_time,
                              normal_time_options,
                              "End-of-semester predictions for time until back to normal",
                              "Estimate until back to normal",
                              same_color = TRUE)

normal_time_plot

# Add logo
grid::grid.raster(logo, x = 0.01, y = 0.01, just = c('left', 'bottom'), width = unit(1.5, 'cm'))
