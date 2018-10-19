#####
#### wine reviews
####

# the .csv files have formatting issues with accented
# characters, so need to use json file

# load packages
library(jsonlite)
library(dplyr)
library(stringr)
library(readxl)
library(quanteda)
library(ggplot2)
library(texreg)
library(tidytext)

# load data 
reviews <- fromJSON("C:/Users/Brandon/Google Drive/blog/wine reviews/wine-reviews/winemag-data-130k-v2.json", simplifyDataFrame = TRUE)

# check out
head(reviews, 10)

# make reviews object a tbl
reviews <- as_tibble(reviews)

# how many reviews of french wines
reviews %>% 
  filter(str_detect(country, "France")) %>% 
  summarise(distinct_fr_names = n_distinct(country),
          n_fr_cases = n())

# create subset of reviews of french wines
reviews_fr <- reviews %>% 
  filter(str_detect(country, "France"))

# unique french regions in reviews
regions_fr <- reviews %>% 
  filter(str_detect(country, "France")) %>%
  distinct(region_1) %>% 
  arrange(region_1) # NA present (last row)

# import aoc regions
aoc <- read_excel("vins_AOC.xlsx")

# number of aoc regions
aoc %>% 
  distinct(appellations) %>% 
  summarise(n()) # 302

# import igp regions
igp <- read_excel("vins_IGP.xlsx")

#  number of igp regions
igp %>% 
  distinct(igp) %>% 
  summarise(n()) # 76

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
reviews_fr$region_1 <- str_trim(reviews_fr$region_1, side = "both")


# number of each value
reviews_fr %>% 
  group_by(region_type) %>% 
  summarise(n()) 
  
# view NAs
reviews_fr %>% 
  filter(is.na(region_type)) %>% 
  View()

# count unique problematic remaining regions to see
# if any are worth changing
reviews_fr %>% 
  filter(is.na(region_type)) %>% 
  count(region_1) %>% 
  arrange(desc(n))

# counts of unique regions
reviews_fr %>% 
  count(province) %>% 
  arrange(desc(n)) %>% 
  mutate(province = reorder(province, n)) %>% 
  ggplot(aes(province, n, fill = province)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip()

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

###
### analyses
###

### analyzing text of reviews

# what is unique about french versus non-french wines?
# tf-idf of all countries

# unnest reviews/descriptions 
data("stop_words")
unnest_reviews <- reviews %>%
  filter(!is.na(description)) %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words) 

# get tf-idf
count_wc <- unnest_reviews %>%
  select(word, country) %>%
  count(word, country, sort = TRUE)
total_words <- count_wc %>% 
  group_by(country) %>% 
  summarise(total = sum(n))
country_words <- left_join(count_wc,
                           total_words)
country_words <- country_words %>%
  bind_tf_idf(word, country, n)
head(country_words, 20)

# isolate france
france_words <- country_words %>% 
  filter(country == "France") 

# check out 
france_words %>% 
  arrange(desc(tf_idf)) %>% 
  head(20)

# visualize top ten
france_words %>% 
  arrange(desc(tf_idf)) %>% 
  top_n(10) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, tf_idf, fill = word)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip()
  
# what is unique about each french region ("provinces")
# tf-idf by province on reviews_fr 

# unnest reviews/descriptions 
data("stop_words")
unnest_reviews_fr <- reviews_fr %>%
  filter(!is.na(description)) %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words) 

# get tf-idf
count_wp <- unnest_reviews_fr %>%
  select(word, province) %>%
  count(word, province, sort = TRUE)
total_words_fr <- count_wp %>% 
  group_by(province) %>% 
  summarise(total = sum(n))
province_words <- left_join(count_wp,
                           total_words_fr)
province_words <- province_words %>%
  bind_tf_idf(word, province, n)

# check out 
province_words %>% 
  arrange(desc(tf_idf)) %>% 
  head(20)

# visualize top ten for each region
province_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(province) %>% 
  top_n(10) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = province)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~province, ncol = 2, scales = "free") +
  coord_flip()

### predicting points

# make points numeric
reviews_fr$points <- as.numeric(reviews_fr$points)

# overall ratings distribution
reviews_fr %>% 
  ggplot(aes(points)) +
  geom_histogram(show.legend = FALSE, bins = 20)
reviews_fr %>% 
  ggplot(aes(points)) +
  geom_density(show.legend = FALSE)

# visualization ratings distribution by region_type
reviews_fr %>% 
  filter(!is.na(region_type)) %>% 
  ggplot(aes(points, color = region_type, fill = region_type)) +
  geom_density(alpha = .5)
  # igp much more concentrated and around mid-80s, whereas 
  # aoc is more evenly distributed and has more mass at 
  # higher ratings
  # could signify two types of consumers, also tougher
  # expectations with aoc, perhaps the higher price of 
  # aoc wines could impact rating (lower rating for same quality)

  # can include all NAs in region_type in both categories to see 
  # distributions change

# check review min and max
reviews_fr %>% 
  filter(!is.na(region_type)) %>% 
  summarise(min = min(points),
            max = max(points))

# points my major reviewers
reviews_fr %>% 
  #filter(!is.na(taster_name)) %>% 
  group_by(taster_name) %>% 
  summarise(mean = mean(points),
            min = min(points),
            max = max(points),
            range = max - min,
            sd = sd(points))
  # NA, though unknown how many people and much larger in
  # number of cases than other names, appears to behave 
  # in a similar manner

  # RIDGELINE PLOT

# descriptives on points by year
reviews_fr %>% 
  filter(!is.na(vintage)) %>%
  filter(vintage > 1982) %>% 
  ggplot(aes(x = vintage, y = points, color = vintage)) +
  geom_point() +
  geom_abline()

reviews_fr %>% 
  filter(!is.na(vintage) & !is.na(region_type)) %>% 
  filter(vintage > 1982) %>% # 1945 and 1982 (only one avis)
  ggplot(aes(x = as.factor(vintage), y = points)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  facet_wrap(~ region_type)


# basic ols with year as predictor
lm.0 <- lm(points ~ vintage, data = reviews_fr)
summary(lm.0)
lm.1 <- lm(points ~ vintage + I(vintage^2), data = reviews_fr)
summary(lm.1)
lm.2 <- lm(points ~ vintage + I(vintage^2) + I(vintage^3), 
           data = reviews_fr)
summary(lm.2)
lm.3 <- lm(points ~ vintage + as.factor(region_type),
           data = reviews_fr)
summary(lm.3)
lm.4 <- lm(points ~ vintage + as.factor(region_type) + 
             as.factor(province),
            data = reviews_fr)
summary(lm.4)
lm.5 <- lm(points ~ vintage + as.factor(region_type) + 
             as.factor(province) + price,
           data = reviews_fr)
summary(lm.5)

# create table of output
screenreg(list(lm.0, lm.3, lm.4, lm.5))



# create separate data objects for aoc and igp
reviews_aoc <- reviews_fr %>% 
  filter(region_type == "aoc")

reviews_igp <- reviews_fr %>% 
  filter(region_type == "igp")

# run separate base models
lm.aoc.0 <- lm(points ~ vintage, data = reviews_aoc)
lm.igp.0 <- lm(points ~ vintage, data = reviews_igp)
summary(lm.aoc.0)
summary(lm.igp.0)

# vintage and price
lm.aoc.1 <- lm(points ~ vintage + price,
               data = reviews_aoc)
lm.igp.1 <- lm(points ~ vintage + price,
               data = reviews_igp)
summary(lm.aoc.1)
summary(lm.igp.1)



###
### tree-based models
###

# prepare data
tree_data <- reviews_fr %>% 
  select(points, taster_name, province, vintage, region_type, price)
class(tree_data$points) # numeric
class(tree_data$taster_name) # character, need to make factor
unique(tree_data$taster_name) # seven 
tree_data$taster_name <- as.factor(tree_data$taster_name)
class(tree_data$province)
length(unique(tree_data$province)) # 11 levels
tree_data$province <- as.factor(tree_data$province)
class(tree_data$region_type)
length(unique(tree_data$region_type)) # 3 levels
tree_data$region_type <- as.factor(tree_data$region_type)
class(tree_data$price) # integer

# number of cases with missing data
complete.cases(tree_data)
sum(complete.cases(tree_data)) # 19582 complete cases

# remove cases with missing data
tree_data <- tree_data %>% 
  filter(complete.cases(tree_data))

# create test-train partition (see caret vignette)
library(caret)
set.seed(123)
in_train <- createDataPartition(
  y = tree_data$points,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

# check format of results
glimpse(in_train)

# create train and test objects
tree_train <- tree_data[in_train,]
tree_test <- tree_data[-in_train,]

# estimate random forest with randomForest
library(randomForest)
rf.rf <- randomForest(points ~ .,
                     data = tree_train,
                     importance = TRUE,
                     ntree = 1000)
varImpPlot(rf.rf)
importance(rf.rf)
plot(rf.rf)
print(rf.rf)

pred <- predict(object = rf.rf, 
                newdata = tree_test,
                type = "response")

library(Metrics)
rmse(actual = tree_test$points,
     predicted = pred)
caret::RMSE(pred, tree_test$points)

# use caret packager
ctrl <- trainControl(method = "cv", 
                     number = 10, 
                     returnResamp = "all")
rf.caret <- train(points ~ .,
                  data = tree_train,
                  method = "rf", 
                  trControl = ctrl,
                  ntree = 500,
                  importance = TRUE)

save(rf.caret, file = "rfcaret.rda")

# check out importance
varImp(rf.caret)
plot(varImp(rf.caret))

# plot tree
plot(rf.caret$finalModel)




### reg with k fold cross validation
lm.caret <- train(points ~ .,
                  data = tree_train,
                  method = "lm",
                  trControl = trainControl(
                    method = "cv",
                    number = 10
                  ))

tree_test$pred <- predict(lm.caret, newdata = tree_test)

tree_test %>% 
  ggplot(aes(x = points, y = pred)) +
  geom_point() +
  geom_smooth()
  # handfull of predictions above 100 but could easily 
  # code down to 100

# residual versus fitted plot
tree_test %>% 
  mutate(resid = points - pred) %>% 
  ggplot(aes(x = pred, y = resid)) +
  geom_point()

tree_test %>% 
  mutate(resid = points - pred) %>% 
  ggplot(aes(x = pred, y = resid)) +
  geom_point() +
  xlim(80, 100) +
  ylim(-10, 10)
  # largest differences around transition from fine to good
  # wines (around 90)

# plot by region_type
library(ggExtra)
g <- tree_test %>% 
  mutate(resid = points - pred) %>% 
  ggplot(aes(x = pred, y = resid, color = region_type)) +
  geom_point() +
  xlim(80, 100) +
  ylim(-10, 10) 
  ggMarginal(g, type = "density", 
             groupColour = TRUE,
             groupFill = TRUE)
  # no igp seems to be predicted above 85ish
  # both seem to be normally distributed around the y axis
  # although aoc has more variation
  # aoc definitely not normally distributed along the x axis
  # igp _could_ be norally distributed along x, need to check



# rating distribution by country
  # good place for ridge plot
# rating distribution by reviewer
# tf-idf: country and description
#library(WikipediR)
#aoc_fr <- page_content(language = "fr",
#                       project = "wikipedia",
#                       page_name = "liste des vins AOC français")
#
#test <- rvest::html_nodes(aoc_fr, "table")
  # was quicker to paste text into excel than to deal with 
  # html parsing

# do ratings change when multiple people who have reviewed a lot of wines review the same one?
