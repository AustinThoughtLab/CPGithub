
################################################################################
####                          Data Visualization                            ####
################################################################################
# Source the libraries 

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

#for dev data/other stuff
#now just add category 'dem' (combo age/sex/race) vvv MAYBE DON'T NEED??

cp <- cp %>%
  unite("dem", sex, race, age, remove=F) %>% # creates a new variable coding the combination of 3 demographic variables
  mutate(dem = as.factor(dem)) %>%
  mutate(cp_4age = ifelse(cp_age =='4', 1,0),
         cp_5age = ifelse(cp_age =='5', 1,0),
         cp_6age = ifelse(cp_age == '6',1,0)) %>%
  unite("456_ages", cp_4age, cp_5age, cp_6age, remove =F)

# mean ratings across subjects for all combinations of factors 
injxdem.means<- cp %>%
  group_by(injury, sex, race, age) %>%
  summarize(n=n(),   # n's are uneven; less than they used to be but is it still an issue?
            mean.rating  = mean(rating), 
            sd.rating = sd(rating))

# collapsing across injury
demo.means<- cp %>%
  group_by(dem) %>%
  summarize(n=n(),   
            mean.rating  = mean(rating), 
            sd.rating = sd(rating))

# just male vs female
sex.means <- cp %>% #opposite of expected: male>female, by 0.22 points (greatest amount w/in dems)
  group_by(sex) %>%
  summarize(n = n(),
            mean.sex = mean(rating),
            median.sex = median(rating)) 

# just white vs black
race.means <- cp %>% #white<black?? but only by .02
  group_by(race) %>%
  summarize(n = n(),
            mean.race = mean(rating),
            median.race = median(rating))

# just adult vs child
age.means <- cp %>% #still child>adult, but only by 0.17 points avg?
  group_by(age) %>%
  summarize(n = n(),
            mean.age = mean(rating),
            median.age = median(rating))

# effect of injury 
injury.means <- cp %>%
  group_by(injury) %>%
  summarize(n=n(),
            mean.inj = mean(rating),
            sd.rating = sd(rating),
            median.inj = median(rating))

# each age group....
kid_ages.means <- cp %>%
  group_by(cp_age) %>%
  summarize(n = n(),
            mean.rating.age = mean(rating),
            sd.rating.age = sd(rating),
            median.rating.age = median(rating))

# now linear models
#unlike 'w katie data june 15' I have current exclusions here

#"pretending rating is continuous, not ordinal, which isn't true"
lm1 <- lmer(rating ~ injury + sex + race + age + (1|cp_number), data=cp)
summary(lm1)
Anova(lm1, Type=3)
#nothing significant.... oddly sex and race more signif than age....

#controlling for injury (basically same as lm1)
lm2 <- lmer(rating ~ sex + race + age + (1|injury) + (1|cp_number), data=cp)
summary(lm2)
Anova(lm2)
#no main effects significant

#injury interaction checks:
# doing an interaction of age:injury
lm3 <- lmer(rating ~ injury * age + race + sex + (1|cp_number), data=cp)
summary(lm3)
Anova(lm3, Type=3)
# interaction of injury and age, p=0.067.... ALMOST significant
# how do i graph this?

#check race:injury...
lm4 <- lmer(rating ~ injury * race + age + sex + (1|cp_number), data=cp)
summary(lm4)
Anova(lm4, Type=3)
#nope... 0.2

#sex:injury
lm5 <- lmer(rating ~ injury * sex + race + age + (1|cp_number), data=cp)
summary(lm5)
Anova(lm5, Type=3)
#def not... 0.8

#holding injury as a constant/ demographic interaction?
lm6 <- lmer(rating ~ race * age * sex + (1|cp_number) + (1|injury), data=cp)
summary(lm6)
Anova(lm6, Type=3)
# no 2 or 3 way interactions
#closest to significant are sex main effect (0.21) and interaction of race/age (0.20)

#check child's age effects....

lm7 <- lmer(rating ~ sex + race + age + cp_age + (1|injury) + (1|cp_number), data=cp)
summary(lm7)
Anova(lm7) #cp age = p=0.432

#child age interactions?

lm8 <- lmer(rating ~ cp_age * sex * race * age + (1|injury) + (1|cp_number), data=cp)
summary(lm8)
Anova(lm8) #ok so many interactions analyzed for... one foudn
# significant is cp_age:sex:age... so sex/age interaction significant for some child ages not others..

#each age seperately

#4 y/os main effects
lm_fours1  <- lmer(rating ~ cp_4age + sex + race + age + (1|injury) + (1|cp_number), data=cp)
summary(lm_fours1)
Anova(lm_fours1)
#include injury/rating
lm_fours2  <- lmer(rating ~ cp_4age + sex + race + age + injury + (1|cp_number), data=cp)
summary(lm_fours2)
Anova(lm_fours2) #injury has significant effect, at least

# 5 y/os main effects
lm_fives1  <- lmer(rating ~ cp_5age + sex + race + age + (1|injury) + (1|cp_number), data=cp)
summary(lm_fives1)
Anova(lm_fives1)
#include injury/rating
lm_fives2  <- lmer(rating ~ cp_5age + sex + race + age + injury + (1|cp_number), data=cp)
summary(lm_fives2)
Anova(lm_fives2) #injury significant for 4s and 5s... nothing else


#### can't yet do interactions of child sex/character sex
####same w participant race (can we get this w lab school kids?)
# return to this...... but now viz

#data viz
library(tidyverse)
library(patchwork)

#using old code first:
gc <- ggplot(cp, aes(dem, rating)) + #here see 5's rate same avg across character demographics...
  facet_wrap(~ cp_age) +
  geom_boxplot(aes(fill=factor(dem))) + 
  labs(x="demographic category (sex-race-age)",
       y="pain rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(gc)

gc2 <- ggplot(cp, aes(injury, rating)) + #5's show higher broken arm/skinned knees...also lower stomach ache/burned tongue
  facet_wrap(~ cp_age) +
  geom_boxplot(aes(fill=factor(injury))) + 
  labs(x="demographic category (sex-race-age)",
       y="pain rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(gc2)

# which is the bar plot of all injuries?

################################################################################
# Copying plots and data analysis from SPP adult data script + fitting to cp


## broken up by injury, each demographic combo represented
g <- ggplot(cp, aes(dem, rating)) +
  facet_wrap(~ injury) +
  geom_boxplot(aes(fill=factor(dem))) + 
  labs(x="demographic category (gender-race-age)",
       y="pain rating")
print(g)

## plots broken up by both race/sex, showing adult and child within each of those as box plots
g2 <- ggplot(cp, aes(age, rating)) +
  facet_wrap(~ race + sex) +
  geom_boxplot(aes(fill=factor(age))) + 
  labs(x="age",
       y="pain rating")
print(g2)


## injury sum, then geom_col w error bars

cinjurysum <- cp %>%
  group_by(injury) %>%
  summarise(
    counts = n(),
    means = mean(rating, digits = 5),
    sds = sd(rating)) %>%
  mutate(ses = sds/sqrt(counts)) %>%
  mutate(cis = 1.96*ses) 


cinjplot <- cinjurysum %>%
  mutate(injury = fct_relevel(cinjurysum$injury, "stomach.ache", "bruised.leg", "burned.tongue",
                              "paper.cut", "skinned.knees", "splinter", "bee.sting", 
                              "broken.arm")) %>% #manually ordered by ascending mean; 
  #can just do ggplot(aes(x=reorder(injury, means), y=means) but not in rainbow order
  ggplot(aes(x=injury, y=means)) +
  geom_col(aes(fill= factor(injury))) +
  geom_errorbar(aes(ymin = means - cis, ymax = means + cis), width = 0.3) +
  labs(x="Injury", y="Mean Pain Rating (0-7)") +
  ggtitle("Plot of Mean Pain Rating by Injury with 95% Confidence Intervals") +
  theme_bw()

print(cinjplot)


# for ordering means if there are two identical means when rounded to 2 digits
rmeans <- round(x = cinjurysum$means, digits = 5)


## same as above but for demographics

cdemsum <- cp %>%
  group_by(dem) %>%
  summarise(
    counts = n(),
    means = mean(rating, digits = 5),
    sds = sd(rating)) %>%
  mutate(ses = sds/sqrt(counts)) %>%
  mutate(cis = 1.96*ses) 


dem.plot <- cdemsum %>%
  ggplot(aes(x = reorder(dem, means), y = means)) +
  geom_col(aes(fill=factor(dem))) +
  geom_errorbar(aes(ymin = means - cis, ymax = means + cis), width = 0.3) +
  labs(x="Demographic", y="Mean Pain Rating") +
  ggtitle("Plot of Mean Pain Rating by Demographic") +
  theme_bw() 

print(dem.plot)


## dot plot with CIs   
cagesum <- cp %>%
  group_by(age) %>%
  summarise(
    counts = n(),
    means = mean(rating),
    sds = sd(rating)) %>%
  mutate(ses = sds/sqrt(counts)) %>%
  mutate(cis = 1.96*ses)

cagemeans <- ggplot(cagesum, aes(x=age, y=means)) +
  geom_point(size=1) +
  ylim(3,4.5) +
  geom_errorbar(aes(ymin = means - cis, ymax = means + cis), width = 0.6) +
  labs(x="Age", y= "Mean Pain Rating (0-7)") +
  ggtitle("Mean Pain Rating for Adult vs Child Vignettes") +
  theme_bw() 

print(cagemeans)

csexsum <- cp %>%
  group_by(sex) %>%
  summarise(
    counts = n(),
    means = mean(rating),
    sds = sd(rating)) %>%
  mutate(ses = sds/sqrt(counts)) %>%
  mutate(cis = 1.96*ses)

csexmeans <- ggplot(csexsum, aes(x=sex, y=means)) +
  geom_point(size=1) +
  ylim(3,4.5) +
  geom_errorbar(aes(ymin = means - cis, ymax = means + cis), width = 0.6) +
  labs(x="Sex", y= "Mean Pain Rating (0-7)") +
  ggtitle("Mean Pain Rating for Female vs Male Vignettes") +
  theme_bw() 

print(csexmeans)

cracesum <- cp %>%
  group_by(race) %>%
  summarise(
    counts = n(),
    means = mean(rating),
    sds = sd(rating)) %>%
  mutate(ses = sds/sqrt(counts)) %>%
  mutate(cis = 1.96*ses)

cracemeans <- ggplot(cracesum, aes(x=race, y=means)) +
  geom_point(size=1) +
  ylim(3,4.5) +
  geom_errorbar(aes(ymin = means - cis, ymax = means + cis), width = 0.6) +
  labs(x="Race", y= "Mean Pain Rating (0-7)") +
  ggtitle("Mean Pain Rating for Black vs White Vignettes") +
  theme_bw() 

print(cracemeans)

## race ba


baracesum <- cp %>%
  group_by(race) %>%
  summarise(
    counts = n(),
    means = mean(rating),
    sds = sd(rating)) %>%
  mutate(ses = sds/sqrt(counts)) %>%
  mutate(cis = 1.96*ses)

ba <- cp %>%
  filter(injury == "broken.arm") 

ggplot(ba, aes(x=cp_exact_age, y=rating)) +
  geom_point()

print(barace)

# plot of variance of ratings for each injury... 

varplotage <- ggplot(cp, aes(x=injury, y=rating)) +
  facet_wrap(~cp$cp_age) +
  geom_violin() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
  

# same plot, color = age

varplotage <- ggplot(cp, aes(x=injury, y=rating)) +
  geom_violin(aes(fill=cp_age)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))



### what pains are kids saying no to?

nopain <- cp %>%
  mutate(yn= case_when(rating >=1 ~ "y",
                       rating == 0 ~ "n"))

nopainplot <- ggplot(nopain, aes(x=injury)) +
  geom_bar(position = "dodge", aes(fill= factor(yn))) +
  theme_bw()

# y/n tibble by injury

nopainsum <- nopain %>%
  group_by(injury) %>%
  count(yn) 

names(nopainsum)[3] <- 'n.yn'

nopainsum$decimal = (nopainsum$n.yn/58)

nopainsum$percentage = (percent(nopainsum$decimal, accuracy = 0.1))

View(nopainsum)

# plot of y/n by injury facet_wrapped by age

yn.injury.age <- nopain %>% ggplot(aes(x=injury)) +
  facet_wrap(~nopain$cp_age) +
  geom_bar(position = "dodge", aes(fill= factor(yn))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# y/n tibble by injury AND age: 4 year olds

nopainsum4 <- nopain %>%
  filter(nopain$cp_age == 4) %>%
  group_by(injury) %>%
  count(yn) 

names(nopainsum4)[3] <- 'n.yn'

nopainsum4$decimal = (nopainsum4$n.yn/27) ## change depending on n!! 

nopainsum4$percentage = (percent(nopainsum4$decimal, accuracy = 0.1))

View(nopainsum4)


# y/n tibble by injury AND age: 5 year olds

nopainsum5 <- nopain %>%
  filter(nopain$cp_age == 5) %>%
  group_by(injury) %>%
  count(yn) 

names(nopainsum5)[3] <- 'n.yn'

nopainsum5$decimal = (nopainsum5$n.yn/25) ## change depending on n!! 

nopainsum5$percentage = (percent(nopainsum5$decimal, accuracy = 0.1))

View(nopainsum5)


# y/n tibble by injury AND age: 6 year olds

nopainsum6 <- nopain %>%
  filter(nopain$cp_age == 6) %>%
  group_by(injury) %>%
  count(yn) 

names(nopainsum6)[3] <- 'n.yn'

nopainsum6$decimal = (nopainsum6$n.yn/6) ## change depending on n!! 

nopainsum6$percentage = (percent(nopainsum6$decimal, accuracy = 0.1))

View(nopainsum6)


# 4 vs 4.5?

fours <- cp %>%
  filter(cp$cp_age == 4)

View(fours)


fours <- fours %>%
  mutate(bday= case_when(cp_number == "4.1.1" ~ "4.5",
                             cp_number == "4.2.1" ~ "4", # there is no 4.3.1
                             cp_number == "4.4.1" ~ "4.5",
                             cp_number == "4.5.1" ~ "4.5",
                             cp_number == "4.6.1" ~ "4",
                             cp_number == "4.7.1" ~ "4.5",
                             cp_number == "4.8.1" ~ "4.5",
                             cp_number == "4.1.2" ~ "4.5",
                             cp_number == "4.2.2" ~ "4",
                             cp_number == "4.3.2" ~ "4",
                             cp_number == "4.4.2" ~ "4.5",
                             cp_number == "4.5.2" ~ "4.5",
                             cp_number == "4.6.2" ~ "4.5",
                             cp_number == "4.7.2" ~ "4.5",
                             cp_number == "4.8.2" ~ "4.5",
                             cp_number == "4.1.3" ~ "4.5",
                             cp_number == "4.2.3" ~ "4",
                             cp_number == "4.3.3" ~ "4.5",
                             cp_number == "4.4.3" ~ "4.5",
                             cp_number == "4.5.3" ~ "4",
                             cp_number == "4.6.3" ~ "4.5",
                             cp_number == "4.7.3" ~ "4",
                             cp_number == "4.8.3" ~ "4.5",
                             cp_number == "4.1.4" ~ "4.5",
                             cp_number == "4.2.4" ~ "4",
                             cp_number == "4.3.4" ~ "4",
                             cp_number == "4.4.4" ~ "4"))
                             

foursum <- fours %>%
  filter(bday == "4") %>%
  group_by(injury) %>%
  summarise(
    n = n(),
    mean = mean(rating),
    sd = sd(rating)
  )

fourfivesum <-fours %>%
  filter(bday == "4.5") %>%
  group_by(injury) %>%
  summarise(
    n = n(),
    mean = mean(rating),
    sd = sd(rating)
  )

View(foursum)
View(fourfivesum)

# useless plots vvv

fourboxplot <- ggplot(fours, aes(x=injury, y=rating)) + 
  facet_wrap(~bday) +
  geom_boxplot(aes(fill=factor(injury)))

fourcolplot <- ggplot(fours, aes(x=reorder(injury, rating), y=rating)) + 
  facet_wrap(~bday) +
  geom_col(aes(fill=factor(injury)))


# more useful plots vv

foursplot <- ggplot(foursum, aes(x=reorder(injury, mean), y=mean)) +
  geom_col(aes(fill=factor(injury))) +
  ggtitle("4 year old mean ratings by injury (n=10)") +
  ylim(0,7) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


four.fiveplot <- ggplot(fourfivesum, aes(x=reorder(injury, mean), y=mean)) +
  geom_col(aes(fill=factor(injury)))+
  ggtitle("4.5 year old mean ratings by injury (n=17)") +
  ylim(0,7) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


grid.arrange(foursplot, four.fiveplot, ncol=2)

# comparing 4 v 4.5 v 5

fivesum <- cp %>%
  filter(cp_age == 5) %>%
  group_by(injury) %>%
  summarise(
    n = n(),
    mean = mean(rating),
    sd = sd(rating)
  )

fiveplot <- ggplot(fivesum, aes(x=reorder(injury, mean), y=mean)) +
  geom_col(aes(fill=factor(injury)))+
  ggtitle("5 year old mean ratings by injury (n=25)") +
  ylim(0,7) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))



grid.arrange(foursplot, four.fiveplot, fiveplot, ncol=2, nrow=2)

grid.arrange(foursplot, four.fiveplot, fiveplot, ncol=1, nrow=3)



# exclusions yes to which control bar graph -> alphabetical so read book and
#   went to sleep keep swaping positions

# Qctrl1: drew pic
# Qctrl2: went to sleep
# Qctrl3: threw a ball
# Qctrl4: read a book

exyn <- exclusions %>%
  select(Qctrl1, Qctrl2, Qctrl3, Qctrl4)
  
vars <- c("Drew Picture", "Went to Sleep", "Threw Ball", "Read Book") 
  
colnames(exyn) <- vars

exyn <- exyn %>%
  pivot_longer(cols = 1:4, names_to = "question")
  
exynplot <- exyn %>%
  ggplot(aes(x=question)) +
  geom_bar(aes(fill=factor(value))) +
  theme_bw()


# exclusions ratings of pain


cinjurysum <- exclusions %>%
  group_by(injury) %>%
  summarise(
    counts = n(),
    means = mean(rating, digits = 5),
    sds = sd(rating)) %>%
  mutate(ses = sds/sqrt(counts)) %>%
  mutate(cis = 1.96*ses) 


cinjplot <- cinjurysum %>%
  mutate(injury = fct_relevel(cinjurysum$injury, "stomach.ache", "bruised.leg", "burned.tongue",
                              "paper.cut", "skinned.knees", "splinter", "bee.sting", 
                              "broken.arm")) %>% #manually ordered by ascending mean; 
  #can just do ggplot(aes(x=reorder(injury, means), y=means) but not in rainbow order
  ggplot(aes(x=injury, y=means)) +
  geom_col(aes(fill= factor(injury))) +
  geom_errorbar(aes(ymin = means - cis, ymax = means + cis), width = 0.3) +
  labs(x="Injury", y="Mean Pain Rating (0-7)") +
  ggtitle("Plot of Mean Pain Rating by Injury with 95% Confidence Intervals") +
  theme_bw()

print(cinjplot)
  










# assumption checks

cp %>%
  filter(injury == "broken.arm") %>%
  ggplot(aes(x=cp_exact_age, y=rating)) +
  geom_point() +
  ylim(0,7)

cp %>%
  filter(injury == "broken.arm") %>%
  ggplot(aes(x=cp_exact_age, y=rating, fill = cp_sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(0,7)

agelm <- lm(data=cp, rating ~ cp_exact_age)

summary(agelm)












