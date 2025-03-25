## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(simulist)
library(epiparameter)

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
    prob_distribution_params = c(shape = 1, scale = 1)
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
set.seed(1)

## ----sim-linelist-------------------------------------------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death
)

# first 6 rows of linelist
head(linelist)

## ----sim-linelist-diff-risks--------------------------------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  hosp_risk = 0.4,
  hosp_death_risk = 0.2,
  non_hosp_death_risk = 0.01
)

head(linelist)

## ----make-hosp-risk-df--------------------------------------------------------
age_dep_hosp_risk <- data.frame(
  age_limit = c(1, 5, 80),
  risk = c(0.1, 0.05, 0.2)
)
age_dep_hosp_risk

## ----sim-age-strat-linelist---------------------------------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  hosp_risk = age_dep_hosp_risk
)

head(linelist)

## ----sim-age_strat_linelist-error, error=TRUE---------------------------------
try({
age_dep_hosp_risk <- data.frame(
  age_limit = c(1, 5, 95),
  risk = c(0.1, 0.05, 0.2)
)
age_dep_hosp_risk

linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  hosp_risk = age_dep_hosp_risk
)
})

## ----sim-age_strat-linelist-diff-age-range------------------------------------
age_dep_hosp_risk <- data.frame(
  age_limit = c(1, 5, 95),
  risk = c(0.1, 0.05, 0.2)
)

linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  hosp_risk = age_dep_hosp_risk,
  population_age = c(1, 100)
)

head(linelist)

## ----sim-age_strat_linelist-hosp-death-risk-----------------------------------
age_dep_hosp_death_risk <- data.frame(
  age_limit = c(1, 5, 80),
  risk = c(0.3, 0.1, 0.6)
)
age_dep_hosp_death_risk

linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  hosp_death_risk = age_dep_hosp_death_risk
)

## ----sim-age_strat_linelist-non-hosp-death-risk-------------------------------
age_dep_non_hosp_death_risk <- data.frame(
  age_limit = c(1, 5, 80),
  risk = c(0.1, 0.05, 0.1)
)
age_dep_non_hosp_death_risk

linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  non_hosp_death_risk = age_dep_non_hosp_death_risk
)

## ----sim-age-strat-linelist-all-----------------------------------------------
age_dep_hosp_risk <- data.frame(
  age_limit = c(1, 5, 80),
  risk = c(0.1, 0.05, 0.2)
)
age_dep_hosp_death_risk <- data.frame(
  age_limit = c(1, 5, 80),
  risk = c(0.3, 0.1, 0.6)
)
age_dep_non_hosp_death_risk <- data.frame(
  age_limit = c(1, 5, 80),
  risk = c(0.1, 0.05, 0.1)
)

linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  hosp_risk = age_dep_hosp_risk,
  hosp_death_risk = age_dep_hosp_death_risk,
  non_hosp_death_risk = age_dep_non_hosp_death_risk
)

head(linelist)

