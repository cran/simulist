## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(simulist)
library(epiparameter)
library(tidyr)
library(dplyr)
library(incidence2)
library(ggplot2)

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
    prob_distribution_params = c(shape = 3, scale = 3)
  )
)

# get onset to hospital admission from {epiparameter} database
onset_to_hosp <- epiparameter_db(
  disease = "COVID-19",
  epi_name = "onset to hospitalisation",
  single_epiparameter = TRUE
)

# get onset to death from {epiparameter} database
onset_to_death <- epiparameter_db(
  disease = "COVID-19",
  epi_name = "onset to death",
  single_epiparameter = TRUE
)

## ----set-seed-----------------------------------------------------------------
set.seed(1)

## ----sim-linelist-------------------------------------------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  hosp_death_risk = 0.5,
  non_hosp_death_risk = 0.05,
  outbreak_size = c(500, 1000),
  config = create_config()
)

# first 6 rows of linelist
head(linelist)

## ----reshape-linelist---------------------------------------------------------
linelist <- linelist %>%
  pivot_wider(
    names_from = outcome,
    values_from = date_outcome
  ) %>%
  rename(
    date_death = died,
    date_recovery = recovered
  )

## ----plot-onset-hospitalisation, fig.cap="Daily incidence of cases from symptom onset and incidence of deaths. Case fatality risk for hospitalised individuals is 0.5 and the risk for non-hospitalised individuals is 0.05, and these risks are constant through time.", fig.width = 8, fig.height = 5----
daily <- incidence(
  linelist,
  date_index = c(
    onset = "date_onset",
    death = "date_death"
  ),
  interval = "daily"
)
daily <- complete_dates(daily)
plot(daily)

## ----sim-linelist-higher-death-risk-------------------------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  hosp_death_risk = 0.9,
  non_hosp_death_risk = 0.75,
  outbreak_size = c(500, 1000),
  config = create_config()
)

head(linelist)

## ----reshape-linelist-higher-death-risk---------------------------------------
linelist <- linelist %>%
  pivot_wider(
    names_from = outcome,
    values_from = date_outcome
  ) %>%
  rename(
    date_death = died,
    date_recovery = recovered
  )

## ----plot-onset-death-higher-risk, fig.cap="Daily incidence of cases from symptom onset and incidence of deaths. Case fatality risk for hospitalised individuals is 0.9 and the risk for non-hospitalised individuals is 0.75, and these risks are constant through time.", fig.width = 8, fig.height = 5----
daily <- incidence(
  linelist,
  date_index = c(
    onset = "date_onset",
    death = "date_death"
  ),
  interval = "daily"
)
daily <- complete_dates(daily)
plot(daily)

## ----setup-time-varying-cfr---------------------------------------------------
config <- create_config(
  time_varying_death_risk = function(risk, time) risk * exp(-0.05 * time)
)

## ----plot-exponential-dist, fig.cap="The time-varying hospitalised case fatality risk function (`config$time_varying_death_risk`) throughout the epidemic. In this case the hospitalised risks (`hosp_death_risk`) are at their maximum value at day 0 and decline through time, with risk approaching zero at around day 100.", fig.width = 8, fig.height = 5----
exp_df <- data.frame(
  time = 1:150,
  value = config$time_varying_death_risk(risk = 0.9, time = 1:150)
)
ggplot(exp_df) +
  geom_point(mapping = aes(x = time, y = value)) +
  scale_y_continuous(name = "Value") +
  scale_x_continuous(name = "Time (Days)") +
  theme_bw()

## ----sim-linelist-time-varying-cfr--------------------------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  hosp_death_risk = 0.9,
  non_hosp_death_risk = 0.75,
  outbreak_size = c(500, 1000),
  config = config
)

head(linelist)

## ----reshape-linelist-time-varying-cfr----------------------------------------
linelist <- linelist %>%
  pivot_wider(
    names_from = outcome,
    values_from = date_outcome
  ) %>%
  rename(
    date_death = died,
    date_recovery = recovered
  )

## ----plot-onset-death-time-varying-cfr, fig.cap="Daily incidence of cases from symptom onset and incidence of deaths. The baseline case fatality risk for hospitalised individuals is 0.9 and for non-hospitalised individuals is 0.75, and these decline exponentially through time.", fig.width = 8, fig.height = 5----
daily <- incidence(
  linelist,
  date_index = c(
    onset = "date_onset",
    death = "date_death"
  ),
  interval = "daily"
)
daily <- complete_dates(daily)
plot(daily)

## ----setup-time-varying-cfr-stepwise, echo=2----------------------------------
# nolint start redundant_ifelse_linter ifelse used for consistency with other examples
config <- create_config(
  time_varying_death_risk = function(risk, time) ifelse(test = time < 60, yes = risk, no = 0)
)
# nolint end

## ----plot-stepwise-dist, fig.cap="The time-varying case fatality risk function (`config$time_varying_death_risk`) for the hospitalised death risk (`hosp_death_risk`) and non-hospitalised death risk (`non_hosp_death_risk`) throughout the epidemic. In this case the risks are at their user-supplied values from day 0 to day 60, and then become 0 onwards.", fig.width = 8, fig.height = 5----
stepwise_df <- data.frame(
  time = 1:150,
  value = config$time_varying_death_risk(risk = 0.9, time = 1:150)
)
ggplot(stepwise_df) +
  geom_point(mapping = aes(x = time, y = value)) +
  scale_y_continuous(name = "Value") +
  scale_x_continuous(name = "Time (Days)") +
  theme_bw()

## ----sim-linelist-time-varying-cfr-stepwise-----------------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  hosp_death_risk = 0.9,
  non_hosp_death_risk = 0.75,
  outbreak_size = c(500, 1000),
  config = config
)

head(linelist)

## ----reshape-linelist-time-varying-cfr-stepwise-------------------------------
linelist <- linelist %>%
  pivot_wider(
    names_from = outcome,
    values_from = date_outcome
  ) %>%
  rename(
    date_death = died,
    date_recovery = recovered
  )

## ----plot-onset-death-time-varying-cfr-stepwise, fig.cap="Daily incidence of cases from symptom onset and incidence of deaths. The maximum case fatality risk for hospitalised individuals is 0.9 and for non-hospitalised individuals is 0.75, and these rates remain constant from days 0 to 60, and then go to 0 from day 60 onwards.", fig.width = 8, fig.height = 5----
daily <- incidence(
  linelist,
  date_index = c(
    onset = "date_onset",
    death = "date_death"
  ),
  interval = "daily"
)
daily <- complete_dates(daily)
plot(daily)

## ----setup-time-varying-cfr-stepwise-window-----------------------------------
config <- create_config(
  time_varying_death_risk = function(risk, time) {
    ifelse(test = time > 50 & time < 100, yes = risk * 0.5, no = risk)
  }
)

## ----plot-stepwise-dist-window, fig.cap="The time-varying case fatality risk function (`config$time_varying_death_risk`) which scales the hospitalised death risk (`hosp_death_risk`) and non-hospitalised death risk (`non_hosp_death_risk`) throughout the epidemic. In this case the risks are at their maximum, user-supplied, values from day 0 to day 50, and then half the risks from day 50 to day 100, and then return to their maximum value from day 100 onwards.", fig.width = 8, fig.height = 5----
stepwise_df <- data.frame(
  time = 1:150,
  value = config$time_varying_death_risk(risk = 0.9, time = 1:150)
)
ggplot(stepwise_df) +
  geom_point(mapping = aes(x = time, y = value)) +
  scale_y_continuous(name = "Value", limits = c(0, 1)) +
  scale_x_continuous(name = "Time (Days)") +
  theme_bw()

## ----sim-linelist-time-varying-cfr-stepwise-window----------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  hosp_death_risk = 0.9,
  non_hosp_death_risk = 0.75,
  outbreak_size = c(500, 1000),
  config = config
)

head(linelist)

## ----reshape-linelist-time-varying-cfr-stepwise-window------------------------
linelist <- linelist %>%
  pivot_wider(
    names_from = outcome,
    values_from = date_outcome
  ) %>%
  rename(
    date_death = died,
    date_recovery = recovered
  )

## ----plot-onset-death-time-varying-cfr-stepwise-window, fig.cap="Daily incidence of cases from symptom onset and incidence of deaths. The maximum case fatality risk for hospitalised individuals is 0.9 and for non-hospitalised individuals is 0.75, and these rates remain constant from days 0 to 50, and then from days 50 to 100 the case fatality risk is halved (i.e `hosp_death_risk` = 0.45 and `non_hosp_death_risk` = 0.375), before going back to their original risks from day 100 onwards.", fig.width = 8, fig.height = 5----
daily <- incidence(
  linelist,
  date_index = c(
    onset = "date_onset",
    death = "date_death"
  ),
  interval = "daily"
)
daily <- complete_dates(daily)
plot(daily)

