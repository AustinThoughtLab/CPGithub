##### my script playing w data

library(readxl)
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)
library(gitcreds)
library(dplyr)
install.packages("gghighlight")
library(gghighlight)


# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
lib = c("tidyverse", "nloptr", "lme4", "ggplot2", "reshape2", "ggpubr", "dplyr", "rstatix", "car", "stats", "coin", "scales", "psych", "ggthemes", "brms")
ipak(lib)



d <- read.csv("data/300 for R 3.22.22.csv")

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






View(d)

# manip dataset

dclean <- d %>%
  group_by(injury) %>%
  summarise(
    counts = n(rating),
    means = mean(rating),
    sds = sd(rating)) %>%
  mutate(ses = sds/sqrt(counts)) %>%
  mutate(cis = 1.96*ses)

show(f1dsum)



# plot bee sting data by dem (can remove fill=factor(dem) from geom_boxplot(),
#       I just thought the colors were nice lol)

beesting <- d %>%
  filter(injury == "bee sting") 

ggplot(beesting, aes(x= dem, y= rating)) +
  geom_boxplot(aes(fill=factor(dem))) + 
  labs(x="Demographics (sex.race.age)",
       y="Pain Rating") +
  ggtitle("My Plot!!!!!!") +
  theme_bw() 


# plot: all

ggplot(d, aes(x=rating, color = dem)) +
  geom_freqpoly(binwidth = 1) 
  

g <- ggplot(d, aes(dem, rating)) +
  facet_wrap(~ injury) +
  geom_boxplot(aes(fill=factor(dem))) + 
  labs(x="demographic category (sex-race-age)",
       y="pain rating")
g



ghist <- d%>%
  ggplot(aes(x = rating)) +
  geom_histogram(aes(fill=factor(rating)), binwidth = .5) +
  facet_wrap(~injury)+
  labs(x="Pain Rating (0-7)",
       y="Count") 

ghist


# plot broken arm vs paper cut

ba <- subset(d, injury == "broken arm")

pc <- subset(d, injury == "paper cut")

bvp <- rbind(ba, pc)

# broken arm vs paper cut (can reorder if you actually want to use it)

bvphist <- bvp%>%
  ggplot(aes(x = rating)) +
  geom_histogram(aes(fill=factor(rating)), binwidth = .5) +
  facet_wrap(~injury) +
  labs(x="Pain Rating (0-7)",
       y="Count") 





View(bpsum)
# plot: adults vs children

ggplot(d, aes(x=race, y=rating)) +
  geom_boxplot() 

# plot: white vs Black

ggplot(d, aes(x=age, y=rating)) +
  geom_boxplot()

# plot: male vs female

ggplot(d, aes(x=sex, y=rating)) +
  geom_boxplot()

head(d)


### plot SPP vvv

g4 <- ggplot(d, aes(age, rating)) +
  geom_boxplot(aes(fill=factor(age))) +
  labs(x="age", y="pain rating") +
  geom_errorbar(mysum, aes(ymin = means - cis, ymax = means + cis), width = 0.3)

                    
                    
agesum <- d %>%
  group_by(age) %>%
  summarise(
    counts = n(),
    means = mean(rating),
    sds = sd(rating)) %>%
  mutate(ses = sds/sqrt(counts)) %>%
  mutate(cis = 1.96*ses)
                  
# plot means with CIs
      
# dot plot with CIs            
agemeans <- ggplot(mysum, aes(x=age, y=means)) +
  geom_point(size=1) +
  ylim(3,4.5) +
  geom_errorbar(aes(ymin = means - cis, ymax = means + cis), width = 0.6) +
  labs(x="Age", y= "Mean Pain Rating (0-7)") +
  ggtitle("Mean Pain Rating for Adult vs Child Vignettes") +
  theme_bw() 
                  
agemeans

sexsum <- d %>%
  group_by(sex) %>%
  summarise(
    counts = n(),
    means = mean(rating),
    sds = sd(rating)) %>%
  mutate(ses = sds/sqrt(counts)) %>%
  mutate(cis = 1.96*ses)

sexmeans <- ggplot(sexsum, aes(x=sex, y=means)) +
  geom_point(size=1) +
  ylim(3,4.5) +
  geom_errorbar(aes(ymin = means - cis, ymax = means + cis), width = 0.6) +
  labs(x="Sex", y= "Mean Pain Rating (0-7)") +
  ggtitle("Mean Pain Rating for Female vs Male Vignettes") +
  theme_bw() 

sexmeans

racesum <- d %>%
  group_by(race) %>%
  summarise(
    counts = n(),
    means = mean(rating),
    sds = sd(rating)) %>%
  mutate(ses = sds/sqrt(counts)) %>%
  mutate(cis = 1.96*ses)

racemeans <- ggplot(racesum, aes(x=race, y=means)) +
  geom_point(size=1) +
  ylim(3,4.5) +
  geom_errorbar(aes(ymin = means - cis, ymax = means + cis), width = 0.6) +
  labs(x="Race", y= "Mean Pain Rating (0-7)") +
  ggtitle("Mean Pain Rating for Black vs White Vignettes") +
  theme_bw() 

racemeans


# boxplot with CIs
bp <- ggplot(d, aes(x=age, y=rating)) +
  geom_boxplot() +
  geom_errorbar(aes(ymin = mysum$means - mysum$cis, ymax = mysum$means + mysum$cis), 
                width = 0.3)
  

#looks right, not right

ggplot(d, aes(age, rating))+
  stat_boxplot(aes(age, rating),geom='errorbar', linetype=1, width=0.5) +  
  geom_boxplot(aes(age, rating),outlier.shape=1) 
  

# correct plot
  
geom_errorbar(aes(ymin = mysum$means - mysum$cis, ymax = mysum$means + mysum$cis)
                , width = 0.3)



# dot plot + boxplot




# the other plot



g2.0 <- ggplot(d, aes(age, means)) +
  facet_wrap(~ race + gender) +
  geom_boxplot(aes(fill=factor(age))) +
  geom_errorbar(aes(ymin = means - cis, ymax = means + cis), width = 0.3) +
  labs(x="age",
       y="pain rating")

g2.0

# injuries bar graph

injurysum <- d %>%
  group_by(injury) %>%
  summarise(
    counts = n(),
    means = mean(rating, digits = 5),
    sds = sd(rating)) %>%
  mutate(ses = sds/sqrt(counts)) %>%
  mutate(cis = 1.96*ses) %>%
  mutate(means = rmeans)


injplot <- injurysum %>%
  mutate(injury = fct_relevel(injurysum$injury, "paper cut", "splinter", "bruised leg", 
                              "soup", "stomach ache", 
                              "bee sting", "skinned knees", "broken arm")) %>%
  ggplot(aes(x=injury, y=means)) +
  geom_col(aes(fill= factor(injury))) +
  geom_errorbar(aes(ymin = means - cis, ymax = means + cis), width = 0.3) +
  labs(x="Injury", y="Mean Pain Rating (0-7)") +
  ggtitle("Plot of Mean Pain Rating by Injury with 95% Confidence Intervals") +
  theme_bw()

rmeans <- round(x = injurysum$means, digits = 5)
    



# broken arm mean pain for each demographic factor
library(stringi)

demsum <- d %>%
  group_by(dem) %>%
  summarise(
    counts = n(),
    means = mean(rating, digits = 5),
    sds = sd(rating)) %>%
  mutate(ses = sds/sqrt(counts)) %>%
  mutate(cis = 1.96*ses) 


demsum %>%
  mutate(dem = fct_relevel(dem, "m_w_a", "m_b_a", "f_w_a", 
                              "f_b_a", "m_b_c", 
                              "f_b_c", "f_w_c", "m_w_c")) %>%
  ggplot(aes(x=dem, y=means)) +
  geom_col(aes(fill= factor(dem))) +
  geom_errorbar(aes(ymin = means - cis, ymax = means + cis), width = 0.3) +
  labs(x="Demographic", y="Mean Pain Rating") +
  ggtitle("Plot of Mean Pain Rating by Demographic") +
  theme_bw()

rmeansdem <- round(x = demsum$means, digits = 5)

# messing around, ignore

ggplot(d, aes(x=dem, y=rating)) + 
  geom_boxplot(fill= "light blue", color = "blue") +
  theme_bw() +
  labs(x="Demographics (sex.race.age", y="Rating (0-7)") +
  ggtitle("Pain Ratings by Vignette Demographic")




################################################################################
#                                 Nicoles script                               #

#help from katie on plots, July 8th
# participant race: p_white

# need to rerun d and whatever manipulations Nicole made to it

d <- read.csv("data/300 for R 3.22.22.csv")

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

d<-d%>% 
  mutate(race_label= dplyr::recode(p_white,
                                   "1"="white",
                                   "0"="POC"))

################################### mutated dataset ############################

g7 <- ggplot(d, aes(race_label, rating)) +
  geom_boxplot(aes(fill=factor(race_label))) + 
  labs(x="participant race",
       y="pain rating")+
  theme(plot.title = element_text(family = "Times", color = "black", size = 16, hjust = 0.5),
        axis.title.y = element_text(family = "Times", color = "black", size = 16),
        axis.line.y = element_line(color = "black"), axis.text.y = element_text(color = "black", family = "Times", size = 16),
        axis.title.x = element_text(family = "Times", color = "black", size = 16),
        axis.text.x = element_text(color = "black", family = "Times", size = 16)) +
  theme(legend.position="none") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

g7


#similar but different representation of participant race
g7.5 <- ggplot(d, aes(race_label, rating)) +
  stat_boxplot(aes(race_label, rating),
               geom='errorbar', linetype =1, width = 0.5) + # add whiskers
  geom_boxplot(outlier.shape = 1) + 
  stat_summary(fun=mean, geom="point", size = 1) + # add dot for the mean
  #stat_summary(fun.data = mean_se, geom="errorbar", width = 0.70)+ # add error bar to each plot based on the variation within that factor variable
  labs(x="participant race",
       y="pain rating")+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.title=element_blank()) +
  theme(plot.title=element_text(family = "Times", color = "black", size = 20, hjust = 0.5),
        axis.title.y = element_text(family = "Times", color = "black", size = 20),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(family = "Times", color = "black", size = 20),
        axis.title.x = element_text(family = "Times", color = "black", size = 20),
        axis.text.x = element_text(family = "Times", color = "black", size = 20),
        legend.text=element_text(family = "Times", color = "black", size=16))
g7.5

g7.5 <- ggplot(d, aes(race_label, rating)) +
  geom_violin() +
  coord_flip() +
  geom_boxplot(outlier.shape = 1)
g7.5

race_summary <- d %>%
  group_by(p_white) %>%
  summarize(n = n(),
            rating.mean = (mean(rating, na.rm = T)),
            rating.sd = (sd(rating, na.rm = T)), # standard deviation
            rating.se = rating.sd/sqrt(n), # calculate standard error
            CILL = rating.mean - rating.se, # calculate lower 95% CI
            CIUL = rating.mean + rating.se) %>% # calculate upper 95% CI
  mutate(race_label= dplyr::recode(p_white,
                                   "1"="white",
                                   "0"="POC"))
d_race <- d %>%
  mutate(rating.mean = ifelse(p_white == 1, 3.31250, 3.91844),
         CILL = ifelse(p_white == 1, 3.275358, 3.877064),
         CIUL = ifelse(p_white == 1, 3.349642, 3.959815),
         race_label = ifelse(p_white == 1, 'white', 'POC'))


prace_dotplot <- ggplot(d_race, aes(x= race_label, y = rating, color = race_label)) +
  geom_jitter(position=position_jitter(0.3), size = 1, shape = 1) +
  geom_errorbar(aes(ymax=CIUL, ymin=CILL), colour = "black", width = 0.10) +
  geom_pointrange(aes(x=race_label, y=rating.mean, ymin=rating.mean, ymax=rating.mean), size = 1, shape=95) +
  ylim(-0, 7) +
  labs(x="participant race", y="pain rating") +
  ggtitle("participant race effect") +
  theme_minimal(base_size=14)+
  theme(plot.title = element_text(size = 14, hjust = 0.5), axis.title.y = element_text(family = "Times", color = "black", size = 14), axis.line.y = element_line(color = "black"), axis.text.y = element_text(color = "black"), axis.line.x = element_line(color = "black"), axis.text.x = element_text(family = "Times", color = "black", size = 14)) +
  theme(legend.position="none", legend.title=element_blank()) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
prace_dotplot

#export higher quality image
ggsave("prace_dotplot.png", prace_dotplot, dpi = 300, width = 7, height= 5)


# by country of origin

View(d)

south.africa <- d %>%
  filter(p_country == "South Africa", p_country == "Aouth Africa",  p_country == "South africa")
  
sa <- d %>% filter(p_country == "South Africa")
sb <- d %>% filter(p_country == "Aouth Africa") 
sc <- d %>% filter(p_country == "South africa") 
south.africa <- rbind.data.frame(sa, sb, sc)

s1 <- subset(d, p_country == "South Africa")
s2 <- subset(d, p_country == "Aouth Africa")
s3 <- subset(d, p_country == "South africa")
south.africa <- rbind(s1, s2, s3)

## recode; when %in% c()

unique(d$p_country)

View(country.names)

South.Africa <- recode(d$p_country, case_when %in% c("South Africa ", "Aouth Africa ", "South Sfrica", "South africa",
                           "South African ", "South africa ", "South Africa", "south africa",
                           "south Africa", "SOUTH AFRICA", "Soth Africa", "Republic of South Africa"))

View(d)

View(south.africa)

# need to be combined



d <- d %>% 
  mutate(p_country = case_when(p_country %in% c("South Africa ", "Aouth Africa ", "South Sfrica", "South africa",
                                                "South African ", "South africa ", "South Africa", "south africa",
                                                "south Africa", "SOUTH AFRICA", "Soth Africa", "Republic of South Africa") ~ "South Africa",
                               p_country %in% c("Poland", "Polska") ~ "Poland",
                               p_country %in% c("Mexico", "México", "mexico", "México ", "Mexico ",
                                                "MEXICO") ~ "Mexico",
                               p_country %in% c("greece", "Greece") ~ "Greece",
                               p_country %in% c("portugal", "Portugal ") ~ "Portugal",
                               p_country %in% c("Belgium", "Belgium ") ~ "Belgium",
                               p_country %in% c("Czech Republic ", "Czech Republic") ~ "Czech Republic",
                               p_country %in% c("UK", "United Kingdom") ~ "United Kingdom",
                               p_country %in% c("Italy", "Italia", "italy") ~ "Italy",
                               p_country %in% c("netherlands", "the Netherlands", "Netherlands") ~ "Netherlands",
                               p_country %in% c("") ~ "Not Listed",
                               TRUE ~ p_country))
View(d)

countrysum <- d %>%
  group_by(p_country) %>%
  summarise(
    counts = n()/16, # divide by 16 bc in long format so each participant has 16 rows
    means = mean(rating),
    sds = sd(rating)) %>%
  mutate(ses = sds/sqrt(counts)) %>%
  mutate(cis = 1.96*ses) 

View(countrysum)

## the highest n's are:
##   South Africa: n=72
##   Mexico: n=62
##   Portugal: n=47
##   Poland: n=30
##   Italy: n=19
##   Everything Else: n<10

countryplot <- ggplot(countrysum, aes(x=reorder(p_country, counts), y=means)) +
  geom_point() +
  geom_vline(xintercept =  "Spain", color = "red") +
  ylim(1.5,4.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

countryba <- d %>%
  filter(injury == "broken arm") %>%
  group_by(p_country) %>%
  summarise(
    count = n(),
    mean = mean(rating),
    sd = sd(rating))

    
  
  
country.ba.plot <- ggplot(countryba, aes(x= reorder (p_country,mean), y=mean)) +
  geom_col()


ba5 <- ba %>%
  filter(p_country == c("South Africa", "Mexico", "Portugal", "Poland", "Italy"))

View(ba5)

ba5sum <- ba5 %>%
  group_by(p_country) %>%
  summarise(
    count = n(),
    mean = mean(rating),
    sd = sd(rating)) %>%
  mutate(ses = sd/sqrt(count)) %>%
  mutate(cis = 1.96*ses)



ba5plot <- ggplot(ba5sum, aes(x=reorder(p_country, mean), y=mean))+
  geom_col() +
  geom_errorbar(aes(ymin = mean - cis, ymax = mean + cis), width = 0.6) +
  theme_bw()

# all injuries w dem

allinjplot <- ggplot(d, aes(dem, rating)) +
  facet_wrap(~ injury) +
  geom_boxplot(aes(fill=factor(dem))) + 
  labs(x="demographic category (gender-race-age)",
       y="pain rating")

# all injuries w country

allcountplot <-  ggplot(d, aes(injury, rating)) +
  facet_wrap(~ p_country) +
  geom_boxplot(fill = "grey")

# highlight on top 5 n's -> italy is diff idk why; need to fix

top5 <- d %>%
    filter(p_country == c("South Africa", "Mexico", "Portugal", "Poland", "Italy"))

highlightplot <- allcountplot %>%  #doesn't work; asks for mapping w aes
  ggplot(top5, aes(injury, rating)) +
  facet_wrap(~p_country) +
  geom_boxplot(aes(fill=factor(injury)))







  
combhighlight <- ggplot() +
  geom_boxplot(top5, aes(x=injury, y=rating, fill = factor(injury))) +
  geom_boxplot(d, aes(x=injury, y=rating, fill = "grey"))+
  facet_wrap(~p_country) 







