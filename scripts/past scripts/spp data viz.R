#SPP data viz

#adult data analysis/data visualization
# n=300 version

library(readxl)
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)
library(gitcreds)

d <- read_excel("data/300 for R 3.22.22.xlsx") #data path, put in whatever yours is


d <- d %>%
  mutate(subject_ID = as.factor(subject_ID),
         race = as.factor(race), # race of char in photo
         age = as.factor(age), # age of char in photo
         c_child = ifelse(age =='child', 1,0),
         gender = as.factor(gender), # sex of char in photo
         condition = as.factor(condition), # codes which injury is paired with each photo 
         injury = as.factor(injury), # injury in vignette; excludes the control items
         p_age = as.factor(p_age), #participant age
         p_gender = as.factor(p_gender), #participant gender (not sex rn, includes nonbinary)
         p_race = as.factor(p_race), #participant race
         p_white = ifelse(p_race =='white', 1,0),
         p_black = ifelse(p_race == 'black_africanamerican', 1,0),
         p_hispanic = ifelse(p_race == 'hispanic_latino', 1,0),
         character = as.factor(character)) %>% # name of character; we don't expect this to matter; might simplify things to have it tied to the photo?
  unite("dem", gender, race, age, remove=F) %>% # creates a new variable coding the combination of 3 demographic variables
  mutate(dem = as.factor(dem)) 


# make level names more transparent
d$gender = recode_factor(d$gender,
                         'f' = 'female',
                         'm' = 'male')

d$age = recode_factor(d$age,
                      'a'='adult',
                      'c' = 'child')

d$race = recode_factor(d$race,
                       'b'= 'black',
                       'w' = 'white')


# mean ratings across subjects for all combinations of factors 
d.s<- d %>%
  group_by(injury, gender, race, age) %>%
  summarize(n=n(),   # n's are uneven; less than they used to be but is it still an issue?
            mean.rating  = mean(rating), 
            sd.rating = sd(rating))


# collapsing across injury
d.s2 <- d %>%
  group_by(gender, race, age) %>%
  summarize(n = n(),
            mean.rating  = mean(rating), # note these look quite similar
            sd.rating = sd(rating))


# just male vs female
gender.s <- d %>%
  group_by(gender) %>%
  summarize(n = n(),
            mean.gender = mean(rating),
            median.gender = median(rating)) 


# just white vs black
race.s <- d %>%
  group_by(race) %>%
  summarize(n = n(),
            mean.race = mean(rating),
            median.race = median(rating))


# just adult vs child
age.s <- d %>%
  group_by(age) %>%
  summarize(n = n(),
            mean.age = mean(rating),
            median.age = median(rating))


# effect of injury 
injury.s <- d %>%
  group_by(injury) %>%
  summarize(n=n(),
            mean.inj = mean(rating),
            sd.rating = sd(rating),
            median.inj = median(rating))

d.char <- d %>%
  group_by(character) %>%
  summarize(n = n(),
            mean.character = mean(rating),
            median.character = median(rating))

#now similar, but comines characters of same demographics
d.dem <- d %>%
  group_by(dem) %>%
  summarize(n = n(),
            mean.dem = mean(rating),
            median.dem = median(rating))

# make level names more transparent
d$p_parent = recode_factor(d$p_parent,
                           'no' = 'no',
                           'yes, a biological parent' = 'biological',
                           'yes, a step-parent' = 'step')


parent.s <- d %>%
  group_by(p_parent) %>%
  summarize(n=n(),
            mean.inj = mean(rating), 
            median.inj = median(rating))

#...
# just male vs female participant characteristic (not character characteristic)
d.pg <- d %>%
  group_by(p_gender) %>%
  summarize(n = n(),
            mean.p_gender = mean(rating),
            median.p_gender = median(rating))
#female participants did overall give higher pain ratings than male participants (about 0.5 of a point)
# maybe I got non significant gender b/c nonbinary is in here still?

# all participant races separated (doesn't control for selecting multiple)
d.pr <- d %>%
  group_by(p_race) %>%
  summarize(n = n(),
            mean.p_race = mean(rating),
            median.p_race = median(rating))

#just white vs nonwhite
d.pwhite <- d %>%
  group_by(p_white) %>%
  summarize(n = n(),
            mean.p_race = mean(rating),
            median.p_race = median(rating)) 

#...

#visualization time yehaw 
  # a couple example plots

# broken up by injury, each demographic combo represented
g <- ggplot(d, aes(dem, rating)) +
  facet_wrap(~ injury) +
  geom_boxplot(aes(fill=factor(dem))) + 
  labs(x="demographic category (gender-race-age)",
       y="pain rating")
g

#plots broken up by both race/sex, showing adult and child within each of those as box plots
g2 <- ggplot(d, aes(age, rating)) +
  facet_wrap(~ race + gender) +
  geom_boxplot(aes(fill=factor(age))) + 
  labs(x="age",
       y="pain rating")
g2


# parents / non parents ratings of adults vs kids, and gender seperated too
g3 <- ggplot(d, aes(age, rating)) +
  facet_wrap(~p_parent) +
  geom_boxplot(aes(fill=factor(gender))) +
  labs(x="parent status",
       y="pain rating")
g3


################################################################################
################################################################################

View(d)

whitep <- d %>%
  filter(p_white == 1)

pocp <- d %>%
  filter(p_white == 0)

whitepsum <- whitep %>%
  group_by(injury) %>%
  summarise(
    n = n(),
    mean = mean(rating),
    sd = sd(rating)) %>%
  mutate(ses = sd/sqrt(n)) %>%
  mutate(cis = 1.96*ses) 

pocpsum <- pocp %>%
  group_by(injury) %>%
  summarise(
    n = n(),
    mean = mean(rating),
    sd = sd(rating)) %>%
  mutate(ses = sd/sqrt(n)) %>%
  mutate(cis = 1.96*ses) 


whitepplot <- ggplot(whitepsum, aes(x=reorder(injury, mean), y=mean)) +
  geom_col(aes(fill=factor(injury))) +
  geom_errorbar(aes(ymin = mean - cis, ymax = mean + cis), width = 0.3) +
  ggtitle("White") +
  ylim(0,7) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

pocpplot <- ggplot(pocpsum, aes(x=reorder(injury, mean), y=mean)) +
  geom_col(aes(fill=factor(injury))) +
  geom_errorbar(aes(ymin = mean - cis, ymax = mean + cis), width = 0.3) +
  ggtitle("POC") +
  ylim(0,7) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

grid.arrange(whitepplot, pocpplot, ncol=2, nrow=1)

count(whitep)
count(pocp)



