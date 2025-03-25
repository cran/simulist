## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(simulist)
library(epiparameter)
library(tidyr)
library(dplyr)
library(ggplot2)
library(incidence2)

## ----read-delay-dists---------------------------------------------------------
contact_distribution <- epiparameter(
  disease = "COVID-19",
  epi_name = "contact distribution",
  prob_distribution = create_prob_distribution(
    prob_distribution = "pois",
    prob_distribution_params = c(mean = 2)
  )
)

infectious_period <- epiparameter(
  disease = "COVID-19",
  epi_name = "infectious period",
  prob_distribution = create_prob_distribution(
    prob_distribution = "gamma",
    prob_distribution_params = c(shape = 3, scale = 2)
  )
)

onset_to_hosp <- epiparameter(
  disease = "COVID-19",
  epi_name = "onset to hospitalisation",
  prob_distribution = create_prob_distribution(
    prob_distribution = "lnorm",
    prob_distribution_params = c(meanlog = 1, sdlog = 0.5)
  )
)

# get onset to death from {epiparameter} database
onset_to_death <- epiparameter_db(
  disease = "COVID-19",
  epi_name = "onset to death",
  single_epiparameter = TRUE
)

## ----set-seed-----------------------------------------------------------------
set.seed(123)

## ----sim-linelist-no-reporting-delay------------------------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death
)
identical(linelist$date_onset, linelist$date_reporting)

## ----sim-linelist-variable-reporting-delay------------------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  reporting_delay = function(x) rlnorm(n = x, meanlog = 1, sdlog = 1)
)
head(linelist)

## ----plot-linelist-events-----------------------------------------------------
tidy_linelist <- linelist %>%
  pivot_longer(
    cols = c("date_onset", "date_reporting", "date_admission", "date_outcome")
  ) %>%
  mutate(ordering_value = ifelse(name == "date_onset", value, NA)) %>% # nolint consecutive_mutate_linter
  mutate(case_name = reorder(case_name, ordering_value, min, na.rm = TRUE)) # nolint consecutive_mutate_linter

ggplot(data = tidy_linelist) +
  geom_line(
    mapping = aes(x = value, y = case_name),
    linewidth = 0.25
  ) +
  geom_point(
    mapping = aes(
      x = value,
      y = case_name,
      shape = name,
      col = name
    ), size = 2
  ) +
  scale_x_date(name = "Event date", date_breaks = "week") +
  scale_y_discrete(name = "Case name") +
  scale_color_brewer(
    palette = "Set1",
    name = "Event type",
    labels = c("Date Admission", "Date Onset", "Date Outcome", "Date Reporting")
  ) +
  scale_shape_manual(
    name = "Event type",
    labels = c(
      "Date Admission", "Date Onset", "Date Outcome", "Date Reporting"
    ),
    values = c(15, 16, 17, 18)
  ) +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 4))

## ----plot-variable-reporting-delay--------------------------------------------
ggplot(data = linelist) +
  geom_histogram(
    mapping = aes(x = as.numeric(date_reporting - date_onset)),
    binwidth = 2
  ) +
  scale_x_continuous(
    name = paste0(
      "Reporting delay (",
      attr(linelist$date_reporting - linelist$date_onset, "units"),
      ")"
    )
  ) +
  theme_bw() +
  theme(axis.title.y = element_blank())

## ----sim-linelist-fixed-reporting-delay---------------------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  reporting_delay = function(x) rep(5, times = x)
)
head(linelist)
linelist$date_reporting - linelist$date_onset

## ----sim-linelist-------------------------------------------------------------
# set seed to produce small line list
set.seed(3)
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  reporting_delay = function(x) rlnorm(n = x, meanlog = 2, sdlog = 0.5)
)

# first 6 rows of linelist
head(linelist)

## ----prep-linelist-events-trunc-----------------------------------------------
tidy_linelist <- linelist %>%
  pivot_longer(
    cols = c("date_onset", "date_reporting", "date_admission", "date_outcome")
  ) %>%
  mutate(ordering_value = ifelse(name == "date_onset", value, NA)) %>% # nolint consecutive_mutate_linter
  mutate(case_name = reorder(case_name, ordering_value, min, na.rm = TRUE)) # nolint consecutive_mutate_linter

truncation_day <- 14
trunc_date <- max(tidy_linelist$value, na.rm = TRUE) - truncation_day

## ----plot-linelist-events-trunc-----------------------------------------------
ggplot(data = tidy_linelist) +
  geom_line(
    mapping = aes(x = value, y = case_name),
    linewidth = 0.25
  ) +
  geom_point(
    mapping = aes(
      x = value,
      y = case_name,
      shape = name,
      col = name
    ),
    size = 2
  ) +
  geom_vline(xintercept = trunc_date, linetype = 2) +
  scale_x_date(name = "Event date", date_breaks = "2 week") +
  scale_y_discrete(name = "Case name") +
  scale_color_brewer(
    palette = "Set1",
    name = "Event type",
    labels = c("Date Admission", "Date Onset", "Date Outcome", "Date Reporting")
  ) +
  scale_shape_manual(
    name = "Event type",
    labels = c(
      "Date Admission", "Date Onset", "Date Outcome", "Date Reporting"
    ),
    values = c(15, 16, 17, 18)
  ) +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 4))

## ----truncate-ll--------------------------------------------------------------
linelist_trunc <- truncate_linelist(linelist = linelist)

## ----truncate-ll-forward------------------------------------------------------
linelist_trunc <- truncate_linelist(
  linelist = linelist,
  truncation_day = 3,
  unit = "months",
  direction = "forward"
)

## ----sim-linelist-incidence---------------------------------------------------
# set seed to produce single wave outbreak
set.seed(3)
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  outbreak_size = c(500, 5000)
)

# create incidence object
weekly_inci <- incidence(
  x = linelist,
  date_index = "date_onset",
  interval = "epiweek",
  complete_dates = TRUE
)

## ----sim-linelist-plot-incidence----------------------------------------------
plot(weekly_inci)

## ----trunc-stages-------------------------------------------------------------
linelist_early <- truncate_linelist(
  linelist = linelist,
  truncation_day = as.Date("2023-02-01")
)
inci_early <- incidence(
  x = linelist_early,
  date_index = "date_onset",
  interval = "epiweek",
  complete_dates = TRUE
)

linelist_mid <- truncate_linelist(
  linelist = linelist,
  truncation_day = as.Date("2023-03-15")
)
inci_mid <- incidence(
  x = linelist_mid,
  date_index = "date_onset",
  interval = "epiweek",
  complete_dates = TRUE
)

linelist_late <- truncate_linelist(
  linelist = linelist,
  truncation_day = as.Date("2023-05-01")
)
inci_late <- incidence(
  x = linelist_late,
  date_index = "date_onset",
  interval = "epiweek",
  complete_dates = TRUE
)

## ----sim-linelist-plot-incidence-reporting-delay------------------------------
# set seed to produce single wave outbreak
set.seed(3)
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  reporting_delay = function(x) rlnorm(n = x, meanlog = 2, sdlog = 0.5),
  outbreak_size = c(500, 5000)
)

## ----trunc-stages-reporting-delay, fig.show="hold", out.width="30%"-----------
linelist_early <- truncate_linelist(
  linelist = linelist,
  truncation_day = as.Date("2023-02-01")
)
inci_early <- incidence(
  x = linelist_early,
  date_index = "date_onset",
  interval = "epiweek",
  complete_dates = TRUE
)

linelist_mid <- truncate_linelist(
  linelist = linelist,
  truncation_day = as.Date("2023-03-15")
)
inci_mid <- incidence(
  x = linelist_mid,
  date_index = "date_onset",
  interval = "epiweek",
  complete_dates = TRUE
)

linelist_late <- truncate_linelist(
  linelist = linelist,
  truncation_day = as.Date("2023-05-01")
)
inci_late <- incidence(
  x = linelist_late,
  date_index = "date_onset",
  interval = "epiweek",
  complete_dates = TRUE
)

## ----plot-trunc-stages-reporting-delay, fig.show="hold", out.width="30%"------
plot(inci_early) +
  ggtitle("Early") +
  theme(plot.title = element_text(size = 25, hjust = 0.5))
plot(inci_mid) +
  ggtitle("Mid") +
  theme(plot.title = element_text(size = 25, hjust = 0.5))
plot(inci_late) +
  ggtitle("Late") +
  theme(plot.title = element_text(size = 25, hjust = 0.5))

