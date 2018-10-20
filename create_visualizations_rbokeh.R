# load packages
library(jsonlite)
library(dplyr)
library(stringr)
library(readxl)
library(quanteda)
library(ggplot2)
library(texreg)
library(tidytext)
library(here)

# load data 
reviews <- fromJSON(here("data", "winemag-data-130k-v2.json"))

# check out preview
head(reviews, 10)

# make reviews a tibble
reviews <- as_tibble(reviews)

# how many reviews of french wines
reviews %>% 
  filter(country == "France") %>% 
  summarise(n_french = n())

# create subset of reviews of french wines
reviews_fr <- reviews %>% 
  filter(country == "France")

# unique french regions in reviews NOT SURE IF NEED TO MAKE OBJECT
regions_fr <- reviews_fr %>%
  filter(!is.na(region_1)) %>% 
  distinct(region_1) %>% 
  arrange(region_1) 

# look at first ten
regions_fr

# import aoc regions
aoc <- read_excel(here("data", "vins_AOC.xlsx"))

# number of aoc regions
aoc %>% 
  distinct(appellations) %>% 
  summarise(n()) 

# import igp regions
igp <- read_excel(here("data", "vins_IGP.xlsx"))

#  number of igp regions
igp %>% 
  distinct(igp) %>% 
  summarise(n()) 

# convert all region names to lower
reviews_fr$region_1 <- char_tolower(reviews_fr$region_1)
reviews_fr$region_2 <- char_tolower(reviews_fr$region_2)
reviews_fr$province <- char_tolower(reviews_fr$province)
aoc$appellations <- char_tolower(aoc$appellations)
aoc$`régions viticoles` <- char_tolower(aoc$`régions viticoles`)
aoc$subdivisions <- char_tolower(aoc$subdivisions)
igp$region <- char_tolower(igp$region)
igp$igp <- char_tolower(igp$igp)
igp$unite_plus_petit <- char_tolower(igp$unite_plus_petit)

# remove "-" character from names
reviews_fr$region_1 <- str_replace_all(reviews_fr$region_1, "-", " ")
reviews_fr$region_2 <- str_replace_all(reviews_fr$region_2, "-", " ")
reviews_fr$province <- str_replace_all(reviews_fr$province, "-", " ")
aoc$appellations <- str_replace_all(aoc$appellations, "-", " ")
aoc$`régions viticoles` <- str_replace_all(aoc$`régions viticoles`, "-", " ")
aoc$subdivisions <- str_replace_all(aoc$subdivisions, "-", " ")
igp$region <- str_replace_all(igp$region, "-", " ")
igp$igp <- str_replace_all(igp$igp, "-", " ")
igp$unite_plus_petit <- str_replace_all(igp$unite_plus_petit, "-", " ")


# ensure accents are correct
reviews_fr$region_1 <- iconv(reviews_fr$region_1, "UTF-8", "ASCII", sub="byte")
reviews_fr$region_1 <- str_replace_all(reviews_fr$region_1, "<c3><a2>", "â")
reviews_fr$region_1 <- str_replace_all(reviews_fr$region_1, "<c3><a9>", "é")
reviews_fr$region_1 <- str_replace_all(reviews_fr$region_1, "<c3><a0>", "à")
reviews_fr$region_1 <- str_replace_all(reviews_fr$region_1, "<c3><b4>", "ô")
reviews_fr$region_1 <- str_replace_all(reviews_fr$region_1, "<c3><a8>", "è")
reviews_fr$region_1 <- str_replace_all(reviews_fr$region_1, "<c3><a7>", "ç")
reviews_fr$region_1 <- str_replace_all(reviews_fr$region_1, "<c3><ae>", "ae")
reviews_fr$region_1 <- str_replace_all(reviews_fr$region_1, "<c3><89>", "é")
# last one is technically upper case (but I want them lower)

# identify unique combination of characters to replace
replace <- reviews_fr %>% 
  mutate(punc = str_detect(reviews_fr$region_1, "<")) %>% 
  filter(punc == TRUE) %>% 
  select(region_1) %>% 
  unique()


# indicator variable for region type (aoc/igp/NA)
reviews_fr <- reviews_fr %>% 
  mutate(region_type = case_when(
    (region_1 %in% aoc$appellations |
       region_1 %in% aoc$`régions viticoles` |
       region_1 %in% aoc$subdivisions) &
      (!region_1 %in% igp$region |
         !region_1 %in% igp$igp |
         ! region_1 %in% igp$unite_plus_petit) ~ "aoc",
    (region_1 %in% igp$region |
       region_1 %in% igp$igp |
       region_1 %in% igp$unite_plus_petit) &
      (!region_1 %in% aoc$appellations |
         !region_1 %in% aoc$`régions viticoles` |
         !region_1 %in% aoc$subdivisions) ~ "igp"
    
  ))

# number of each value
reviews_fr %>% 
  group_by(region_type) %>% 
  summarise(n()) # 16144
