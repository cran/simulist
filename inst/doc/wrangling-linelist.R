## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(simulist)
library(epiparameter)
library(dplyr)
library(epicontacts)

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

## -----------------------------------------------------------------------------
linelist$date_onset
unclass(linelist$date_onset)

## -----------------------------------------------------------------------------
daily_cens_linelist <- censor_linelist(linelist, interval = "daily")
head(daily_cens_linelist)

weekly_cens_linelist <- censor_linelist(linelist, interval = "weekly")
head(weekly_cens_linelist)

## -----------------------------------------------------------------------------
linelist$date_onset
round(linelist$date_onset)
daily_cens_linelist$date_onset

## -----------------------------------------------------------------------------
linelist %>%
  filter(as.logical(rbinom(n(), size = 1, prob = 0.5)))

## -----------------------------------------------------------------------------
idx <- as.logical(rbinom(n = nrow(linelist), size = 1, prob = 0.5))
linelist[idx, ]

## -----------------------------------------------------------------------------
linelist %>%
  dplyr::slice_sample(prop = 0.5) %>%
  dplyr::arrange(id)

## -----------------------------------------------------------------------------
epicontacts <- make_epicontacts(
  linelist = linelist,
  contacts = contacts,
  id = "case_name",
  from = "from",
  to = "to",
  directed = TRUE
)
plot(epicontacts)

## -----------------------------------------------------------------------------
all_contacts <- unique(c(contacts$from, contacts$to))
not_reported <- sample(x = all_contacts, size = 0.5 * length(all_contacts))
not_reported

## -----------------------------------------------------------------------------
# make copy of contact tracing data for under-reporting
contacts_ur <- contacts
for (person in not_reported) {
  contacts_ur <- contacts_ur[contacts_ur$to != person, ]
  contacts_ur[contacts_ur$from %in% person, "from"] <- NA
}
head(contacts_ur)

## -----------------------------------------------------------------------------
linelist_ur <- linelist[!linelist$case_name %in% not_reported, ]
epicontacts <- make_epicontacts(
  linelist = linelist_ur,
  contacts = contacts_ur,
  id = "case_name",
  from = "from",
  to = "to",
  directed = TRUE
)
plot(epicontacts)

## -----------------------------------------------------------------------------
all_contacts <- unique(c(contacts$from, contacts$to))
not_reported <- sample(x = all_contacts, size = 1)
not_reported

## -----------------------------------------------------------------------------
# make copy of contact tracing data for under-reporting
contacts_ur <- contacts
while (length(not_reported) > 0) {
  contacts_ur <- contacts_ur[!contacts_ur$to %in% not_reported, ]
  not_reported_ <- contacts_ur$to[contacts_ur$from %in% not_reported]
  contacts_ur <- contacts_ur[!contacts_ur$from %in% not_reported, ]
  not_reported <- not_reported_
}
head(contacts_ur)

## -----------------------------------------------------------------------------
# subset line list to match under-reporting in contact tracing data
linelist_ur <- linelist[linelist$case_name %in% unique(contacts$from), ]

epicontacts <- make_epicontacts(
  linelist = linelist_ur,
  contacts = contacts_ur,
  id = "case_name",
  from = "from",
  to = "to",
  directed = TRUE
)
plot(epicontacts)

## ----rm-ct-col-tidyverse------------------------------------------------------
# remove column by name
linelist %>% # nolint one_call_pipe_linter
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

