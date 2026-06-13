############################################################
# Homework 2 — Olta Recica and Olsa Berani (DSDM)
############################################################

# Run these once to install, then you can comment them out
install.packages("vars",   repos = "https://cloud.r-project.org")
install.packages("readxl", repos = "https://cloud.r-project.org")
install.packages("dplyr",  repos = "https://cloud.r-project.org")

rm(list = ls())

library(vars)
library(readxl)
library(dplyr)

# ── Load raw data ──────────────────────────────────────────

epu_raw    <- read_excel("DATA/US_Policy_Uncertainty_Data.xlsx")
payems_raw <- read.csv("DATA/PAYEMS.csv")
indpro_raw <- read.csv("DATA/INDPRO.csv")
pcepi_raw  <- read.csv("DATA/PCEPI.csv")

# ── Clean and prepare EPU ──────────────────────────────────

epu <- epu_raw %>%
  rename(year = Year, month = Month, epu = News_Based_Policy_Uncert_Index) %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(year)) %>%
  filter(year > 1985 | (year == 1985 & month >= 1)) %>%
  filter(year < 2023  | (year == 2023  & month <= 3)) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  arrange(date) %>%
  select(date, epu)

# ── Clean PAYEMS, INDPRO, PCEPI and compute mom growth rates ──

payems <- payems_raw %>%
  mutate(date = as.Date(observation_date)) %>%
  filter(date >= as.Date("1985-01-01") & date <= as.Date("2023-03-01")) %>%
  arrange(date) %>%
  mutate(payems_g = 100 * (PAYEMS / lag(PAYEMS) - 1)) %>%
  select(date, payems_g)

indpro <- indpro_raw %>%
  mutate(date = as.Date(observation_date)) %>%
  filter(date >= as.Date("1985-01-01") & date <= as.Date("2023-03-01")) %>%
  arrange(date) %>%
  mutate(indpro_g = 100 * (INDPRO / lag(INDPRO) - 1)) %>%
  select(date, indpro_g)

pcepi <- pcepi_raw %>%
  mutate(date = as.Date(observation_date)) %>%
  filter(date >= as.Date("1985-01-01") & date <= as.Date("2023-03-01")) %>%
  arrange(date) %>%
  mutate(pcepi_g = 100 * (PCEPI / lag(PCEPI) - 1)) %>%
  select(date, pcepi_g)

# ── Merge all into one dataset ─────────────────────────────

my_data <- epu %>%
  inner_join(payems, by = "date") %>%
  inner_join(indpro, by = "date") %>%
  inner_join(pcepi,  by = "date") %>%
  filter(!is.na(payems_g) & !is.na(indpro_g) & !is.na(pcepi_g))

print(head(my_data))
print(tail(my_data))
cat("Rows:", nrow(my_data), "\n")
