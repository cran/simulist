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

## ----plot-onset-hospitalisation-----------------------------------------------
daily <- incidence(
  linelist,
  date_index = c(
    onset = "date_onset",
    death = "date_death"
  ),
  interval = "daily",
  complete_dates = TRUE
)
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

## ----prep-onset-death-higher-risk---------------------------------------------
daily <- incidence(
  linelist,
  date_index = c(
    onset = "date_onset",
    death = "date_death"
  ),
  interval = "daily",
  complete_dates = TRUE
)

## ----plot-onset-death-higher-risk---------------------------------------------
plot(daily)

## ----setup-time-varying-cfr---------------------------------------------------
config <- create_config(
  time_varying_death_risk = function(risk, time) risk * exp(-0.05 * time)
)

## ----prep-exponential-dist----------------------------------------------------
exp_df <- data.frame(
  time = 1:150,
  value = config$time_varying_death_risk(risk = 0.9, time = 1:150)
)

## ----plot-exponential-dist----------------------------------------------------
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

## ----prep-onset-death-time-varying-cfr----------------------------------------
daily <- incidence(
  linelist,
  date_index = c(
    onset = "date_onset",
    death = "date_death"
  ),
  interval = "daily",
  complete_dates = TRUE
)

## ----plot-onset-death-time-varying-cfr----------------------------------------
plot(daily)

## ----setup-time-varying-cfr-stepwise, echo=2----------------------------------
# nolint start redundant_ifelse_linter ifelse used for consistency with other examples
config <- create_config(
  time_varying_death_risk = function(risk, time) ifelse(test = time < 60, yes = risk, no = 0)
)
# nolint end

## ----prep-stepwise-dist-------------------------------------------------------
stepwise_df <- data.frame(
  time = 1:150,
  value = config$time_varying_death_risk(risk = 0.9, time = 1:150)
)

## ----plot-stepwise-dist-------------------------------------------------------
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

## ----prep-onset-death-time-varying-cfr-stepwise-------------------------------
daily <- incidence(
  linelist,
  date_index = c(
    onset = "date_onset",
    death = "date_death"
  ),
  interval = "daily",
  complete_dates = TRUE
)

## ----plot-onset-death-time-varying-cfr-stepwise-------------------------------
plot(daily)

## ----setup-time-varying-cfr-stepwise-window-----------------------------------
config <- create_config(
  time_varying_death_risk = function(risk, time) {
    ifelse(test = time > 50 & time < 100, yes = risk * 0.5, no = risk)
  }
)

## ----prep-stepwise-dist-window------------------------------------------------
stepwise_df <- data.frame(
  time = 1:150,
  value = config$time_varying_death_risk(risk = 0.9, time = 1:150)
)

## ----plot-stepwise-dist-window------------------------------------------------
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

## ----prep-onset-death-time-varying-cfr-stepwise-window------------------------
daily <- incidence(
  linelist,
  date_index = c(
    onset = "date_onset",
    death = "date_death"
  ),
  interval = "daily",
  complete_dates = TRUE
)

## ----plot-onset-death-time-varying-cfr-stepwise-window------------------------
plot(daily)

