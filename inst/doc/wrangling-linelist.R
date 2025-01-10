## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(simulist)
library(epiparameter)
library(dplyr)

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

## ----sim-outbreak-------------------------------------------------------------
outbreak <- sim_outbreak(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death
)
linelist <- outbreak$linelist
contacts <- outbreak$contacts

## ----rm-ct-col-tidyverse------------------------------------------------------
# remove column by name
linelist %>%
  select(!ct_value)

## ----rm-ct-col-base-----------------------------------------------------------
# remove column by numeric column indexing
# ct_value is column 12 (the last column)
linelist[, -12]

# remove column by column name
linelist[, colnames(linelist) != "ct_value"]

# remove column by assigning it to NULL
linelist$ct_value <- NULL
linelist

