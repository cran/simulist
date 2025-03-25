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
set.seed(1)

## ----sim-linelist-age-range---------------------------------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.45,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  population_age = c(5, 75),
  outbreak_size = c(100, 1e4)
)
head(linelist)

## ----plot-age-range-----------------------------------------------------------
ggplot(linelist[, c("sex", "age")]) +
  geom_histogram(
    mapping = aes(x = age),
    fill = "#10BED2",
    colour = "black",
    binwidth = 5
  ) +
  scale_y_continuous(name = "Number of Individuals") +
  scale_x_continuous(name = "Age", breaks = seq(0, 75, 5)) +
  theme_bw()

## ----make-age-struct-df-------------------------------------------------------
age_struct <- data.frame(
  age_limit = c(1, 20, 60, 90),
  proportion = c(0.3, 0.4, 0.3, 0)
)
age_struct

## ----sim-age-struct-linelist--------------------------------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.45,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  population_age = age_struct,
  outbreak_size = c(100, 1e4)
)

head(linelist)

## ----plot-age-struct----------------------------------------------------------
ggplot(linelist[, c("sex", "age")]) +
  geom_histogram(
    mapping = aes(x = age),
    fill = "#10BED2",
    colour = "black",
    binwidth = 5
  ) +
  scale_y_continuous(name = "Number of Individuals") +
  scale_x_continuous(name = "Age", breaks = seq(0, 90, 5)) +
  theme_bw() +
  facet_wrap(vars(sex))

## ----make-age-struct-df-young-------------------------------------------------
age_struct <- data.frame(
  age_limit = c(1, 10, 30, 60, 75),
  proportion = c(0.4, 0.3, 0.2, 0.1, 0)
)
age_struct

## ----sim-age-struct-linelist-young--------------------------------------------
linelist <- sim_linelist(
  contact_distribution = contact_distribution,
  infectious_period = infectious_period,
  prob_infection = 0.45,
  onset_to_hosp = onset_to_hosp,
  onset_to_death = onset_to_death,
  population_age = age_struct,
  outbreak_size = c(100, 1e4)
)

head(linelist)

## ----prep-age-struct-young----------------------------------------------------
linelist_m <- subset(linelist, subset = sex == "m")
age_cats_m <- as.data.frame(table(floor(linelist_m$age / 5) * 5))
colnames(age_cats_m) <- c("AgeCat", "Population")
age_cats_m <- cbind(age_cats_m, sex = "m")
linelist_f <- subset(linelist, subset = sex == "f")
age_cats_f <- as.data.frame(table(floor(linelist_f$age / 5) * 5))
colnames(age_cats_f) <- c("AgeCat", "Population")
age_cats_f$Population <- -age_cats_f$Population
age_cats_f <- cbind(age_cats_f, sex = "f")
age_cats <- rbind(age_cats_m, age_cats_f)

breaks <- pretty(range(age_cats$Population), n = 10)
labels <- abs(breaks)

## ----plot-age-struct-young----------------------------------------------------
ggplot(age_cats) +
  geom_col(mapping = aes(x = Population, y = factor(AgeCat), fill = sex)) +
  scale_y_discrete(name = "Lower bound of Age Category") +
  scale_x_continuous(name = "Population", breaks = breaks, labels = labels) +
  scale_fill_manual(values = c("#F04A4C", "#106BA0")) +
  theme_bw()

