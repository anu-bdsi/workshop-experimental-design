library(tidyverse)

pop <- read_csv(here::here("data/data-raw/Australia_ age structure in 2022.csv"), skip = 1, n_max = 100) |> 
  mutate(age_group = case_when(Age <= 14 ~ "0-14",
                               Age <= 29 ~ "15-29",
                               Age <= 34 ~ "30-34",
                               Age <= 39 ~ "35-39",
                               Age <= 44 ~ "40-44",
                               Age <= 49 ~ "45-49",
                               Age <= 54 ~ "50-54",
                               Age <= 59 ~ "55-59",
                               Age <= 64 ~ "60-64",
                               Age <= 69 ~ "65-69",
                               Age <= 74 ~ "70-74",
                               Age <= 79 ~ "75-79",
                               Age <= 84 ~ "80-84",
                               Age > 84 ~ "85+")) |> 
  pivot_longer(cols = c(Male, Female), names_to = "sex", values_to = "population") |> 
  summarise(population = sum(population), .by = c(sex, age_group)) |> 
  mutate(sex = tolower(sex))


death <- readxl::read_excel(here::here("data/data-raw/AIHW-PHE-229-report-supplementary-tables.xlsx"), sheet = "Table S4.1", skip = 3, n_max = 14) |> 
  select(age_group = `Age group`,
         male_deaths = Number...2,
         male_deathrate = `Age-specific rate (per 100,000)...3`,
         female_deaths = Number...5,
         female_deathrate = `Age-specific rate (per 100,000)...6`
  ) |> 
  pivot_longer(cols = -age_group) |> 
  separate(name, "_", into = c("sex", "name")) |>
  pivot_wider(names_from = name, values_from = value) |> 
  mutate(age_group = str_replace(age_group, "–", "-")) |> 
  left_join(pop, by = c("sex", "age_group"))

saveRDS(death, file = here::here("data/data-input/covid19.rds"))
death
