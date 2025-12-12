
# 7COM1079 Final Report – R Code (RStudio)
# RQ: Is there a difference in the mean CWUR (Center of World University Rankings) Score between universities in Asia and Europe?


# Load required packages
library(readr)
library(dplyr)


# 1. Load data

cwur <- read_csv("cwurData.csv")


# 2. Define region groupings

asia_countries <- c(
  "Japan", "Israel", "South Korea", "Singapore", "China",
  "Taiwan", "Hong Kong", "Thailand", "Malaysia", "India",
  "Turkey", "Saudi Arabia", "Iran", "Lebanon", "United Arab Emirates"
)

europe_countries <- c(
  "United Kingdom", "Switzerland", "France", "Sweden", "Italy",
  "Germany", "Netherlands", "Finland", "Norway", "Denmark",
  "Belgium", "Spain", "Ireland", "Austria", "Portugal",
  "Czech Republic", "Greece", "Hungary", "Poland", "Iceland",
  "Slovenia", "Estonia", "Croatia", "Slovak Republic",
  "Bulgaria", "Lithuania", "Romania", "Cyprus", "Serbia"
)

# 3. Create Region variable and filter to Asia/Europe with valid scores
# ---------------------------
asia_europe <- cwur %>%
  mutate(
    Region = case_when(
      country %in% asia_countries   ~ "Asia",
      country %in% europe_countries ~ "Europe",
      TRUE                          ~ NA_character_
    )
  ) %>%
  filter(
    Region %in% c("Asia", "Europe"),
    !is.na(score)
  )

# Make Region an ordered factor (Asia, then Europe)
asia_europe$Region <- factor(asia_europe$Region,
                             levels = c("Asia", "Europe"))

# Quick check of sample sizes
print(table(asia_europe$Region))

# ---------------------------
# 4. Create outputs folder (if it does not exist)
# ---------------------------
if (!dir.exists("outputs")) dir.create("outputs")