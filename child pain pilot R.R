# child pain pilot R
##first made this with our n of 6, pilot stuff before pre-reg
## now, with n=15 so far

#I just made two different excel docs... 
    ## 'pain kids R.xlsx' is without the control questions (w/ their ratings)
    ## 'pain kids R with control ratings.xlsx' is with control question ratings
##updates to this R file made 4.19.2022, 15 kids. all before pre reg done.

##notes to self...
  ## been a long time since my stat classes/ really should look back at those notes/files, AND get Hixon help? or someone else.
  ## still a handful of things I'm doing modeled after kt pilot data. R, I need to know a lot more than what was done in there
  ## may be wrong in some models, about where to call injury a random effect...
  ## when why what type of ANOVAS/ what values (chi-square, fully forgot where that value is/signif. threshold)
  ## haven't looked at variance
  ## need childrends demographic info (have age, not sex/race)


library(readxl)
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)
library(gitcreds)

dwc <- read.csv("data/pain kids R with control ratings.csv")

d <- read.csv("data/pain kids csv.csv")

## participant info notated cp here, not p, as in 'child pain/ child participant'
## took out things I don't yet / on hand have with me, like child race even/ accurate age w months not just year

d <- d %>%
  mutate(subject_ID = as.factor(subject_ID),
         race = as.factor(race), # race of char in photo
         age = as.factor(age), # age of char in photo
         sex = as.factor(sex), # sex of char in photo
         condition = as.factor(condition), # codes which injury is paired with each photo 
         injury = as.factor(injury), # injury in vignette; excludes the control items
         cp_age = as.factor(cp_age), #participant age
         character = as.factor(character)) %>% # name of character; we don't expect this to matter; might simplify things to have it tied to the photo?
  unite("dem", sex, race, age, remove=F) %>% # creates a new variable coding the combination of 3 demographic variables
  mutate(dem = as.factor(dem)) 

  
  d$sex = recode_factor(d$sex,
                           'f' = 'female',
                           'm' = 'male')

  
  d$age = recode_factor(d$age,
                        'a'='adult',
                        'c' = 'child')
  
  d$race = recode_factor(d$race,
                         'b'= 'black',
                         'w' = 'white')
  
  
  # make level names more transparent. recoding.
  d$sex = recode_factor(d$sex,
                           'f' = 'female',
                           'm' = 'male')
  
  d$age = recode_factor(d$age,
                        'a'='adult',
                        'c' = 'child')
  
  d$race = recode_factor(d$race,
                         'b'= 'black',
                         'w' = 'white')
  
  
  # mean ratings across subjects for all combinations of factors 
  d.sc<- d %>%
    group_by(injury, sex, race, age) %>%
    summarize(n=n(),   # n's are uneven; less than they used to be but is it still an issue?
              mean.rating  = mean(rating), 
              sd.rating = sd(rating))
  
  
  # collapsing across injury
  d.sc2 <- d %>%
    group_by(sex, race, age) %>%
    summarize(n = n(),
              mean.rating  = mean(rating), # note these look quite similar
              sd.rating = sd(rating))
  
  
  #### no way this pans out w n=6 but just doing stuff
  
  
  # just male vs female
  sex.sc <- d %>%
    group_by(sex) %>%
    summarize(n = n(),
              mean.sex = mean(rating),
              median.sex = median(rating)) 
  
  # just white vs black
  race.sc <- d %>%
    group_by(race) %>%
    summarize(n = n(),
              mean.race = mean(rating),
              median.race = median(rating))
  
  
  # just adult vs child
  age.cs <- d %>%
    group_by(age) %>%
    summarize(n = n(),
              mean.age = mean(rating),
              median.age = median(rating))
  
  
  # effect of injury 
  injury.cs <- d %>%
    group_by(injury) %>%
    summarize(n=n(),
              mean.inj = mean(rating), 
              median.inj = median(rating))
  
  #now w dems together
  dem.cs <- d %>%
    group_by(dem) %>%
    summarize(n = n(),
              mean.dem = mean(rating),
              median.dem = median(rating))

  
  ### some actual linear mixed effects models now #### (begun on 4.19.22)
  
  # treating subject ID as random effect
  lm1 <- lmer(rating ~ injury + sex + race + age + (1|subject_ID), data=d)
  summary(lm1)
  Anova(lm1, Type=3)
  ## injury has significant p value, somewhat close p values for sex and age? .1's though
  ## but sex= males higher ratings than females right now i think
  
  #treating injury as a random effect
  lm2 <- lmer(rating ~ sex + race + age + (1|injury) + (1|subject_ID), data=d)
  summary(lm2)
  Anova(lm2)
  # none significant here
  
  # doing an interaction of age/ injury
  lm3 <- lmer(rating ~ injury * age + race + sex + (1|subject_ID), data=d)
  summary(lm3)
  Anova(lm3, Type=3)
  # injury/age interaction NOT signif. (kids not reflecting systematic rating difference for child vs adult characters [yet... need more n?])
  
  ## start looking at participant characteristics... but don't have it coded what sex/race participants are
  ## I do have age  can compare age of 4s vs 5s?
  ## reminder to ask Lily if they have/ if we can get demographic info about the kids? esp. race
  
  
  # child ages, some 4s/5s
  cp_age.cs <- d %>%
    group_by(cp_age) %>%
    summarize(n = n(),
              mean.age = mean(rating),
              median.age = median(rating))
  #somewhat overall lower ratings from 5s than 4s. doesn't mean a whole lot yet though/idk the nature of that yet
  
  #still has injury/subject ID as random effects, adding in child age to the model
  lm4 <- lmer(rating ~ sex + race + age + cp_age + (1|injury) + (1|subject_ID), data=d)
  summary(lm4)
  Anova(lm4)
  ## child's age not significant
  # I wonder about interaction  injury/cp age though?
  
  #model accounting for all character demographics, and looking for interaction of injury/child age
  lm5 <- lmer(rating ~ injury * cp_age + sex + race + age + (1|subject_ID), data=d)
  summary(lm4)
  Anova(lm4, type=3)
  #not sure if this is looking unfamiliar bc been a long time or did it wrong...
      # also honestly, don't know why anova sometimes, other times include 'type=3'
      # not seeing a place where it tells me about the interaction
  
  
  #simple plots
  g <- ggplot(d, aes(dem, rating)) +
    facet_wrap(~ injury) +
    geom_boxplot(aes(fill=factor(dem))) + 
    labs(x="demographic category (sex-race-age)",
         y="pain rating")
  g
  
  # we ALL over the place
  # need neater graphs too
  
  g2 <- ggplot(d, aes(age, rating)) +
    facet_wrap(~ race + sex) +
    geom_boxplot(aes(fill=factor(age))) + 
    labs(x="age",
         y="pain rating")
  g2
  # return to this once I have greater n. shows if kids are rating kids or adults, per sex/race group, as higher or lower than each other.
  # right now, adults rated lower than kids in all but white male??
  
  
  #.... starting pieces for trying to graph adult / child participant data in non gg plot forms
  
  # Simple Bar Plot
  # counts <- table(mtcars$gear)
  # barplot(counts, main="Car Distribution",
          # xlab="Number of Gears")
  
  
  # Grouped Bar Plot
 #  counts <- table(mtcars$vs, mtcars$gear)
 #  barplot(counts, main="Car Distribution by Gears and VS",
         #  xlab="Number of Gears", col=c("darkblue","red"),
         #  legend = rownames(counts), beside=TRUE)
  
  
  
  
  
  
  