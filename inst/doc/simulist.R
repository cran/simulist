## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(simulist)
library(epiparameter)

## ----read-epidist-------------------------------------------------------------
# create contact distribution (not available from {epiparameter} database)
contact_distribution <- epiparameter(
  disease = "COVID-19",
  epi_name = "contact distribution",
  prob_distribution = create_prob_distribution(
    prob_distribution = "pois",
    prob_distribution_params = c(mean = 2)
  )
)

# create infectious period (not available from {epiparameter} database)
infectious_period <- epiparameter(
  disease = "COVID-19",
  epi_name = "infectious period",
  prob_distribution = create_prob_distribution(
    prob_distribution = "gamma",
    prob_distribution_params = c(shape = 1, scale = 1)
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
set.seed(123)

## ----sim-linelist-------------------------------------------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death
)
head(linelist)

## ----sim-large-linelist-------------------------------------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  outbreak_size = c(250, 1e4)
)
head(linelist)

## ----sim-linelist-default-case-type-------------------------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death
)
head(linelist)

## ----sim-linelist-mod-case-type-----------------------------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  case_type_probs = c(suspected = 0.05, probable = 0.05, confirmed = 0.9)
)
head(linelist)

## ----sim-anon-linelist--------------------------------------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  anonymise = TRUE
)
head(linelist)

## ----sim-contacts-------------------------------------------------------------
contacts <- sim_contacts(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5
)
head(contacts)

## ----sim-outbreak-------------------------------------------------------------
outbreak <- sim_outbreak(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death
)
head(outbreak$linelist)
head(outbreak$contacts)

## ----sim-outbreak-func--------------------------------------------------------
contact_distribution <- function(x) dpois(x = x, lambda = 2)
infectious_period <- function(x) rgamma(n = x, shape = 2, scale = 2)
onset_to_hosp <- function(x) rlnorm(n = x, meanlog = 1.5, sdlog = 0.5)
onset_to_death <- function(x) rweibull(n = x, shape = 0.5, scale = 0.2)

outbreak <- sim_outbreak(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death
)
head(outbreak$linelist)
head(outbreak$contacts)

## ----sim-outbreak-anon-func---------------------------------------------------
outbreak <- sim_outbreak(
  contact_distribution = function(x) dpois(x = x, lambda = 2),
  infectious_period = function(x) rgamma(n = x, shape = 2, scale = 2),
  prob_infection = 0.5,
  onset_to_hosp = function(x) rlnorm(n = x, meanlog = 1.5, sdlog = 0.5),
  onset_to_death = function(x) rweibull(n = x, shape = 0.5, scale = 0.2)
)
head(outbreak$linelist)
head(outbreak$contacts)

## ----sim-linelist-no-hosp-death-----------------------------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = NULL,
  onset_to_death = NULL,
  hosp_risk = NULL,
  hosp_death_risk = NULL,
  non_hosp_death_risk = NULL
)
head(linelist)

