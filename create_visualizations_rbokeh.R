#### load packages
library(jsonlite)
library(dplyr)
library(stringr)
library(readxl)
library(quanteda)
library(ggplot2)
library(texreg)
library(tidytext)
library(here)
library(rbokeh)

### get data in order

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

###
### work on names
###

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

# number of unique values
reviews_fr %>% 
  summarise(n_distinct(region_type))

# number of each value
reviews_fr %>% 
  group_by(region_type) %>% 
  summarise(n()) # 16144


## note: the following two commands will remove the ability to 
## distinguish white v red v rosé (i think)

# remove "blanc" from labels (e.g., bordeaux blanc)
reviews_fr$region_1 <- str_replace_all(reviews_fr$region_1,
                                       "bordeaux blanc", "bordeaux")

# remove "rosé" from "bordeaux rosé"
reviews_fr$region_1 <- str_replace_all(reviews_fr$region_1,
                                       "bordeaux rosé", "bordeaux")

# remove "vin de" from "vin de pays d'oc"
reviews_fr$region_1 <- str_replace_all(reviews_fr$region_1,
                                       "vin de pays d'oc",
                                       "pays d'oc")

# remove "vin de pays de " from "vin de pays des côtes de gascogne"
reviews_fr$region_1 <- str_replace_all(reviews_fr$region_1,
                                       "vin de pays des côtes de gascogne",
                                       "côtes de gascogne")

# fix contraction in nuits saints georges
reviews_fr$region_1 <- str_replace_all(reviews_fr$region_1,
                                       "nuits st. georges",
                                       "nuits saint georges")
# could use more expansive replacement re: st., but not sure
# that wouldn't cause more problems (e.g., some might use
# st rather than saint)

# remove "rosé" to fix ones like beaujolais
reviews_fr$region_1 <- str_replace_all(reviews_fr$region_1, 
                                       "rosé", "")

# rerun after changes
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

# now make manual changes

# change castillon/other regions cotes de bordeaux to cotes de bordeaux
# see https://fr.wikipedia.org/wiki/C%C3%B4tes-de-castillon
# no IGP containing bordeaux, so just detect based upon that
reviews_fr <- reviews_fr %>% 
  mutate(cdbord = if_else(str_detect(region_1, 
                                     "bordeaux"), 
                          TRUE, FALSE))

# recode all cotes de bordeaux
reviews_fr$region_type[reviews_fr$cdbord==TRUE] <- "aoc"

# all coteaux de languedoc versions to aoc
reviews_fr <- reviews_fr %>% 
  mutate(lang = case_when(
    str_detect(reviews_fr$region_1, "lang") == TRUE ~ TRUE,
    str_detect(reviews_fr$region_1, "lang") == FALSE ~ FALSE))
reviews_fr$region_type[reviews_fr$lang==TRUE] <- "aoc"

# mâcon villages, etc
reviews_fr <- reviews_fr %>% 
  mutate(macon = case_when(
    str_detect(reviews_fr$region_1, "mâcon") == TRUE ~ TRUE,
    str_detect(reviews_fr$region_1, "mâcon") == FALSE ~ FALSE))
reviews_fr$region_type[reviews_fr$macon==TRUE] <- "aoc"

# contains vin de pays to igp
reviews_fr <- reviews_fr %>% 
  mutate(vdp = case_when(
    str_detect(reviews_fr$region_1, "vin de pays") == TRUE ~ TRUE,
    str_detect(reviews_fr$region_1, "vin de pays") == FALSE ~ FALSE))
reviews_fr$region_type[reviews_fr$vdp==TRUE] <- "igp"

# containing costieres to igp
reviews_fr <- reviews_fr %>% 
  mutate(cost = case_when(
    str_detect(reviews_fr$region_1, "costières") == TRUE ~ TRUE,
    str_detect(reviews_fr$region_1, "costières") == FALSE ~ FALSE))
reviews_fr$region_type[reviews_fr$cost==TRUE] <- "aoc"

# mediterranean to igp
reviews_fr <- reviews_fr %>% 
  mutate(med = case_when(
    str_detect(reviews_fr$region_1, "mediterranée") == TRUE ~ TRUE,
    str_detect(reviews_fr$region_1, "mediterranée") == FALSE ~ FALSE))
reviews_fr$region_type[reviews_fr$med==TRUE] <- "igp"

# médoc variants to aoc
reviews_fr <- reviews_fr %>% 
  mutate(medoc = case_when(
    str_detect(reviews_fr$region_1, "médoc") == TRUE ~ TRUE,
    str_detect(reviews_fr$region_1, "médoc") == FALSE ~ FALSE))
reviews_fr$region_type[reviews_fr$medoc==TRUE] <- "aoc"

# vin de liquer to NA
reviews_fr <- reviews_fr %>% 
  mutate(vdl = case_when(
    str_detect(reviews_fr$region_1, "vin de liqueur") == TRUE ~ TRUE,
    str_detect(reviews_fr$region_1, "vin de liqueur") == FALSE ~ FALSE))
reviews_fr$region_type[reviews_fr$vdl==TRUE] <- NA

# bergerac sec (and other similar ones) to aoc
reviews_fr <- reviews_fr %>% 
  mutate(berg = case_when(
    str_detect(reviews_fr$region_1, "bergerac sec") == TRUE ~ TRUE,
    str_detect(reviews_fr$region_1, "bergerac sec") == FALSE ~ FALSE))
reviews_fr$region_type[reviews_fr$berg==TRUE] <- "aoc"

# maury variants to aoc
reviews_fr <- reviews_fr %>% 
  mutate(maury = case_when(
    str_detect(reviews_fr$region_1, "maury") == TRUE ~ TRUE,
    str_detect(reviews_fr$region_1, "maury") == FALSE ~ FALSE))
reviews_fr$region_type[reviews_fr$maury==TRUE] <- "aoc"

# trim whitespace after changes
#reviews_fr$region_1 <- str_trim(reviews_fr$region_1, side = "both")











# number of each value
reviews_fr %>% 
  group_by(region_type) %>% 
  summarise(n()) # 16144


# create vintage/year variable
reviews_fr <- reviews_fr %>% 
  mutate(vintage = str_extract(title, "[[:digit:]]{4}"))

# rename missing values
reviews_fr$vintage[reviews_fr$vintage == "character(0)"] <- NA

# view if not four digits
reviews_fr %>% 
  filter(nchar(vintage) != 4) %>% 
  View
# check year class
class(reviews_fr$vintage)

# convert to numeric
reviews_fr$vintage <- as.numeric(reviews_fr$vintage)

# check min and max
reviews_fr %>% 
  filter(!is.na(vintage)) %>% 
  summarise(earliest = min(vintage),
            latest = max(vintage)) # prob: min == 1752

# look at distribution of years
reviews_fr %>% 
  filter(!is.na(vintage)) %>% 
  ggplot(aes(vintage)) + 
  geom_density(show.legend = FALSE)
# most are quite recent

# count unique values
reviews_fr %>% 
  filter(!is.na(vintage)) %>% 
  count(vintage)
# three weird cases: 1752, 1904, 1945

# view records for these three
reviews_fr %>% 
  filter(vintage <= 1945) %>% 
  View()
# 1904 is a NV (recode to NA)
# 1945 is correct
# 1752 should be 2016

# recode the above values
reviews_fr$vintage[reviews_fr$vintage==1752] <- 2016
reviews_fr$vintage[reviews_fr$vintage==1904] <- NA

# remove before 1985 due to rarity 
reviews_fr <- reviews_fr %>% 
  filter(vintage >= 1985)

# make points numeric
reviews_fr$points <- as.numeric(reviews_fr$points)




### start visualizing

# points versus vintage
figure() %>% 
  ly_points(x = vintage,
            y = points,
            data = reviews_fr,
            hover = c("@vintage, @points"))

# customize hover
figure() %>% 
  ly_points(x = vintage,
            y = points,
            data = reviews_fr,
            hover = "<b>@vintage</b><br><b>Points</b>: @points")

#ramp <- colorRampPalette(c("red", "blue"))(nrow(reviews_fr))
#figure() %>% 
#  ly_points(x = vintage,
#            y = points,
#            data = reviews_fr,
#            hover = "<b>@vintage</b><br><b>Points</b>: @points",
#            color = ramp)

# adjust size and akpha
figure() %>% 
  ly_points(x = vintage,
            y = points,
            data = reviews_fr,
            hover = "<b>@vintage</b><br><b>Points</b>: @points",
            size = 7,
            alpha = 0.3)

# switch to price
figure() %>% 
  ly_points(x = price,
            y = points,
            data = reviews_fr,
            hover = "<b>Price</b>: @price<br><b>Points</b>: @points")


# adjust size and alpha
figure() %>% 
  ly_points(x = price,
            y = points,
            data = reviews_fr,
            hover = "<b>Price</b>: @price<br><b>Points</b>: @points",
            size = 6,
            alpha = 0.3)

###
### grids 
###

### region type
# split by region type
split_region_type <- split(reviews_fr, reviews_fr$region_type)

# create plotting function
plot_scatter <- function(x){
  figure() %>% 
    ly_points(x = price,
              y = points,
              data = x,
              hover = "<b>Price</b>: @price<br><b>Points</b>: @points",
              size = 6,
              alpha = 0.3)
}

# create list of figures
scatter_list_regtype <- lapply(split_region_type, plot_scatter)

# now plot together
grid_plot(list(AOC = scatter_list[[1]],
               IGP = scatter_list[[2]]),
          nrow = 1,
          same_axes = TRUE,
          xlim = c(0, 1000))

### by specified regions/province

# split by region
split_province <- split(reviews_fr, reviews_fr$province)

# create plotting function
# use from previous sub-section

# create list of figures
scatter_list_province <- lapply(split_province, plot_scatter)

# now plot together
grid_plot(list(Bordeaux = scatter_list_province[["bordeaux"]],
               Burgundy = scatter_list_province[["burgundy"]],
               Champagne = scatter_list_province[["champagne"]],
               Alsace = scatter_list_province[["alsace"]]),
          nrow = 2,
          same_axes = TRUE,
          xlim = c(0, 1000))

# for variety color, need to reduce number of levels


