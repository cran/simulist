## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(simulist)
library(epiparameter)
library(incidence2)
library(epicontacts)

## ----read-delay-dists---------------------------------------------------------
contact_distribution <- epiparameter(
  disease = "COVID-19",
  epi_name = "contact distribution",
  prob_distribution = create_prob_distribution(
    prob_distribution = "pois",
    prob_distribution_params = c(mean = 3)
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
  prob_infection = 0.33,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  outbreak_size = c(500, 1e4)
)

## ----create-incidence---------------------------------------------------------
# create incidence object
daily <- incidence(x = linelist, date_index = "date_onset", interval = "daily")

## ----complete-dates, fig.cap="Daily incidence of cases from symptom onset including days with zero cases.", fig.width = 8, fig.height = 5----
# impute for days without cases
daily <- complete_dates(daily)
plot(daily)

## ----plot-weekly, fig.cap="Weekly incidence of cases from symptom onset", fig.width = 8, fig.height = 5----
weekly <- incidence(linelist, date_index = "date_onset", interval = "isoweek")
plot(weekly)

## ----group-by-sex, fig.cap="Weekly incidence of cases from symptom onset facetted by sex", fig.width = 8, fig.height = 5----
weekly <- incidence(
  linelist,
  date_index = "date_onset",
  interval = "isoweek",
  groups = "sex"
)
plot(weekly)

## ----reshape-linelist-base-r, eval=FALSE--------------------------------------
#  # this can also be achieved with the reshape() function but the user interface
#  # for that function is complicated so here we just create the columns manually
#  linelist$date_death <- linelist$date_outcome
#  linelist$date_death[linelist$outcome == "recovered"] <- NA
#  linelist$date_recovery <- linelist$date_outcome
#  linelist$date_recovery[linelist$outcome == "died"] <- NA

## ----reshape-linelist-tidyverse, message=FALSE--------------------------------
library(tidyr)
library(dplyr)
linelist <- linelist %>%
  tidyr::pivot_wider(
    names_from = outcome,
    values_from = date_outcome
  )
linelist <- linelist %>%
  dplyr::rename(
    date_death = died,
    date_recovery = recovered
  )

## ----plot-onset-hospitalisation, fig.cap="Daily incidence of cases from symptom onset and incidence of hospitalisations and deaths.", fig.width = 8, fig.height = 5----
daily <- incidence(
  linelist,
  date_index = c(
    onset = "date_onset",
    hospitalisation = "date_admission",
    death = "date_death"
  ),
  interval = "daily",
  groups = "sex"
)
daily <- complete_dates(daily)
plot(daily)

## ----contact-distribution-----------------------------------------------------
contact_distribution <- epiparameter(
  disease = "COVID-19",
  epi_name = "contact distribution",
  prob_distribution = create_prob_distribution(
    prob_distribution = "pois",
    prob_distribution_params = c(mean = 2)
  )
)

## ----sim-outbreak-------------------------------------------------------------
set.seed(1)
outbreak <- sim_outbreak(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.5,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death
)
head(outbreak$linelist)
head(outbreak$contacts)

## ----create-epicontacts-------------------------------------------------------
epicontacts <- make_epicontacts(
  linelist = outbreak$linelist,
  contacts = outbreak$contacts,
  id = "case_name",
  from = "from",
  to = "to",
  directed = TRUE
)

## ----print-epicontacts--------------------------------------------------------
epicontacts

## ----plot-epicontacts, fig.cap="Contact network from infectious disease outbreak. This includes all contacts, i.e. individuals that were infected and not infected"----
plot(epicontacts)

## ----subset-linelist-base-r---------------------------------------------------
outbreak$contacts <- outbreak$contacts[outbreak$contacts$was_case == "Y", ]

## ----subset-linelist-tidyverse------------------------------------------------
library(dplyr)
outbreak$contacts <- outbreak$contacts %>%
  dplyr::filter(was_case == "Y")

## ----inspect-data-------------------------------------------------------------
head(outbreak$linelist)
head(outbreak$contacts)

## ----create-cases-epicontacts-------------------------------------------------
epicontacts <- make_epicontacts(
  linelist = outbreak$linelist,
  contacts = outbreak$contacts,
  id = "case_name",
  from = "from",
  to = "to",
  directed = TRUE
)

## ----print-cases-epicontacts--------------------------------------------------
epicontacts

## ----plot-cases-epicontacts, fig.cap="Transmission chain from infectious disease outbreak. This includes only individuals that were infected, including confirmed, probable and suspected cases."----
plot(epicontacts)

