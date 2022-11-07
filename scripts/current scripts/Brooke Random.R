# Brooke random
# ignore this for now, toying with branches
# hello

# jflksdjflkdsj

# fdksjldfkjs

# dlkfjsldkfj

library(readxl)
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)
library(gitcreds)
library(dplyr)
library(tidyr)
library(stringi)
library(RColorBrewer)   
library(scales)
library(gridExtra)


ad <- read.csv("CP Github/data/adultdata.csv")

cd <- cp

View(cd)

adsum <- ad %>%
  group_by(injury) %>%
  summarise(
    n = n(),
    mean= mean(rating),
    sd = sd(rating),
    median = median(rating)
  ) %>%
  arrange(mean)

cdsum <- cd %>%
  group_by(injury) %>%
  summarise(
    n = n(),
    mean= mean(rating),
    sd = sd(rating),
    median = median(rating)
  ) %>%
  arrange(mean)


View(cdsum)
View(adsum)













