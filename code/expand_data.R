# ---------------------------------------------------------
# Prelims
# ---------------------------------------------------------
library(tidyverse)
library(haven)
library(lubridate)
library(countrycode)
library(here)

getwd()

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
here::here()

match_df <- readr::read_csv(here("data",
                                 "results.csv"))

match_df <- match_df %>% 
  mutate(match_id = row_number())


# ---------------------------------------------------------
# Expand data structure
# ---------------------------------------------------------
df <- match_df %>% 
  select(c(match_id,
           starts_with("home_"),
           starts_with("away_")))

df <- df %>% 
  bind_rows(., .) %>% 
  arrange(match_id)

df <- df %>% 
  group_by(match_id) %>% 
  mutate(seq_ = row_number()) %>% 
  ungroup()

df

df <- df %>% 
  mutate(cntry = ifelse(seq_ == 1,
                        home_team,
                        away_team)) %>% 
  mutate(opponent = ifelse(seq_ == 1,
                           away_team,
                           home_team)) %>% 
  mutate(goals_for = ifelse(seq_ == 1,
                            home_score,
                            away_score)) %>% 
  mutate(goals_against = ifelse(seq_ == 1,
                                away_score,
                                home_score)) %>% 
  mutate(home = ifelse(cntry == home_team,
                       1,
                       0))


df <- df %>% 
  select(-c(starts_with("home_"),
            starts_with("away_")))

match_df <- match_df %>% 
  select(-c(starts_with("home_"),
            starts_with("away_")))

df <- df %>% 
  tidylog::left_join(.,
                     match_df,
                     by = "match_id")

df <- df %>% 
  mutate(year = as.numeric(substr(date, start = 1, stop = 4)))

df <- df %>% 
  mutate(date = ymd(date))

df <- df %>% 
  mutate(home_ground = ifelse(cntry == country,
                              1,
                              0))

df <- df %>% 
  select(-c(seq_))


# ---------------------------------------------------------
# Create country codes
# ---------------------------------------------------------

df <- df %>% 
  mutate(cc_iso3c = countrycode::countrycode(cntry,
                                             origin = "country.name.en",
                                             destination = "iso3c")) %>% 
  mutate(cc_cow = countrycode::countrycode(cntry,
                                           origin = "country.name.en",
                                           destination = "cowc")) %>% 
  mutate(cc_wvs = countrycode::countrycode(cntry,
                                           origin = "country.name.en",
                                           destination = "wvs")) %>% 
  mutate(cc_wb = countrycode::countrycode(cntry,
                                          origin = "country.name.en",
                                          destination = "wb")) %>% 
  mutate(cc_ess = case_when(cntry == "Albania",
                            cntry == "Austria",
                            cntry == "Belgium",
                            cntry == "Bulgaria",
                            cntry == "Austria",
                            cntry == "Switzerland",
                            cntry == "Cyprus",
                            cntry == "Czech Republic",
                            cntry == "Germany",
                            cntry == "Denmark",
                            cntry == "Estonia",
                            cntry == "Spain",
                            cntry == "Finland",
                            cntry == "France",
                            cntry == "United Kingdom", # no national teams
                            cntry == "Greece",
                            cntry == "Croatia",
                            cntry == "Hungary",
                            cntry == "Ireland",
                            cntry == "Israel",
                            cntry == "Iceland",
                            cntry == "Italy",
                            cntry == "Lithuania",
                            cntry == "Luxembourg",
                            cntry == "Latvia",
                            cntry == "Montenegro",
                            cntry == "Netherlands",
                            cntry == "Norway",
                            cntry == "Poland",
                            cntry == "Portugal",
                            cntry == "Romania",
                            cntry == "Serbia",
                            cntry == "Sweden",
                            cntry == "Slovenia",
                            cntry == "Slovakia",
                            cntry == "Turkey",
                            cntry == "Ukraine",
                            cntry == "Romania",
                            cntry == "Russia",
                            cntry == "Kosovo"))


"AL"
"AT"
"BE"
"BG"
"AT"
"CH"
"CY"
"CZ"
"DE"
"DK"
"EE"
"ES"
"FI"
"FR"
"GB"
"GR"
"HR"
"HU"
"IE"
"IL"
"IS"
"IT"
"LT"
"LU"
"LV"
"ME"
"NL"
"NO"
"PL"
"PT"
"RO"
"RS"
"SE"
"SI"
"SK"
"TR"
"UA"
"RO"
"RU"
"XK"

test <- df %>% 
  filter(!is.na(cc_wb))

rm(test)


ess_full <- ess_full %>% 
  rename(cntry_ess = cntry) %>% 
  mutate(cntry = case_when()) 

# ---------------------------------------------------------
# Export data
# ---------------------------------------------------------

# ----- R
saveRDS(df,
        file = "data/IntMatches1872_2021.rds")


rm(df)


