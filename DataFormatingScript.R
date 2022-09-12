
# Manipulating dataset from qualtrics to analyzable data!!!

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
install.packages("eeptools")        
library("eeptools")                 

# read in the new .csv file from qualtrics
#   (mine is in a folder titled "data", and I renamed it "painkids52"
#   so therefore: read.csv("data/painkids52corrected.csv")

d <- read.csv("data/8 pain kids_September 7, 2022_10.55.csv")

################################################################################
####                   Run lines 30 - 524 with new data                     ####
####  - if any new exclusions outside of normal criteria, make edits to     ####
####      exclusions section (around line 74)                               ####
################################################################################

d<- d %>% 
  select(-Duration..in.seconds., -StartDate, -IPAddress, -Finished, 
         -RecipientLastName, -RecipientFirstName, -RecipientEmail, 
         -ExternalReference, -LocationLatitude, -LocationLongitude, 
         -DistributionChannel, -UserLanguage, -mturkcode, -StartDate,
         -Status, -Progress, -RecordedDate, -ResponseId, -Q.d1,) 

# rename some variables 
vars = c('dot', 'condition','cp_number','cp_sex','Qpractice1','Qpractice1.5',
         'Qpractice2','Qpractice2.5','Qpractice3','Qpractice3.5','Q1', 'Q1.5', 'Q2', 'Q2.5', 
         'Q3','Q3.5', 'Q4', 'Q4.5', 'Q5', 'Q5.5', 'Q6', 'Q6.5', 'Q7', 'Q7.5', 'Q8', 'Q8.5',
         'Qctrl1', 'Qctrl1.5', 'Qctrl2', 'Qctrl2.5','Qctrl3', 'Qctrl3.5', 'Qctrl4', 'Qctrl4.5',
         'Q9', 'Q9.5', 'Q10', 'Q10.5','Q11','Q11.5', 'Q12', 'Q12.5', 'Q13', 'Q13.5', 'Q14',
         'Q14.5', 'Q15', 'Q15.5', 'Q16', 'Q16.5',
         'Q17', 'Q17.5', 'Q18', 'Q18.5','Q19','Q19.5', 'Q20', 'Q20.5', 'Q21', 'Q21.5', 'Q22',
         'Q22.5', 'Q23', 'Q23.5', 'Q24', 'Q24.5',
         'Q25', 'Q25.5', 'Q26', 'Q26.5','Q27','Q27.5', 'Q28', 'Q28.5', 'Q29', 'Q29.5', 'Q30',
         'Q30.5', 'Q31', 'Q31.5', 'Q32', 'Q32.5',
         'Q33', 'Q33.5', 'Q34', 'Q34.5','Q35','Q35.5', 'Q36', 'Q36.5', 'Q37', 'Q37.5', 'Q38',
         'Q38.5', 'Q39', 'Q39.5', 'Q40', 'Q40.5',
         'Q41', 'Q41.5', 'Q42', 'Q42.5','Q43','Q43.5', 'Q44', 'Q44.5', 'Q45', 'Q45.5', 'Q46',
         'Q46.5', 'Q47', 'Q47.5', 'Q48', 'Q48.5',
         'Q49', 'Q49.5', 'Q50', 'Q50.5','Q51','Q51.5', 'Q52', 'Q52.5', 'Q53', 'Q53.5', 'Q54',
         'Q54.5', 'Q55', 'Q55.5', 'Q56', 'Q56.5',
         'Q57', 'Q57.5', 'Q58', 'Q58.5','Q59','Q59.5', 'Q60', 'Q60.5', 'Q61', 'Q61.5', 'Q62',
         'Q62.5', 'Q63', 'Q63.5', 'Q64', 'Q64.5', 
         'should_this_be_excluded','should_text1','should_text2','cp_pains_experienced', 'na1','na2','na3','na4', 'na5', 'na6','na7', 'na8')
colnames(d) = vars 


# add col for cp_age (make sure to library(stringi))

cp_age <- stri_extract_first(d$cp_number,regex="\\d")

d <- d %>%
  mutate(cp_age = cp_age)

View(d)



#exclusions for control questions, no to broken arm, specific cases, and 7 yos
##      whenever a special case comes up, needs to be added here
##      {ex. quit half way through? add: "cp_number == "x.x.x" ~ 1,"}
#exclusions will be 1, non excluded will be 0

d2 <- d %>% 
  mutate(exclude= case_when(Qctrl1=="yes"~1,
                            Qctrl2=="yes"~1,	
                            Qctrl3=="yes"~1,	
                            Qctrl4=="yes"~1,
                            Q2=="no"~1,
                            Q15=="no"~1,
                            Q22=="no"~1,
                            Q27=="no"~1,
                            Q40=="no"~1,
                            Q45=="no"~1,
                            Q49=="no"~1,
                            Q60=="no"~1,
                            cp_number == "4.1.3 [excluded; already tested]" ~ 1,
                            cp_number == "4.8.1 [excluded]"~1,
                            cp_number == "4.3.2e [excluded]" ~1,
                            cp_age == "7" ~1,
                            TRUE~0))


  
# filter out exclusions

d3 <- d2 %>%
  filter(exclude == 0)


#count amount of valid kids, after exclusions/rename
# IGNORE cp_age 3; there are no 3 year olds, no idea why it does that
subs<-d3 %>%
  select(cp_number, cp_age)%>%
  group_by(cp_age) %>%
  distinct

count_subs<-subs%>%
  group_by(cp_age) %>%
  summarize(n())

print(count_subs)

#subset by condition (for all 8 conditions) 


d_1<-d3 %>% 
  filter(condition=="1") %>% 
  select(dot, condition, cp_number, cp_age, cp_sex, Qctrl1,	
         Qctrl1.5,	Qctrl2,	Qctrl2.5,	Qctrl3,	Qctrl3.5,	Qctrl4,	Qctrl4.5,
         Q1,	Q1.5,	Q2,	Q2.5,	Q3,	Q3.5, Q4,	Q4.5,	Q5,	Q5.5,	Q6,	Q6.5,	Q7,	Q7.5,	Q8,	Q8.5)%>%
  mutate(burned.tongue_WAF=case_when(Q1=="yes"~Q1.5,
                                     Q1=="no"~'0'),
         broken.arm_WCF=case_when(Q2=="yes"~Q2.5, 
                                  Q2=="no"~'0'),
         stomach.ache_WAM=case_when(Q3=="yes"~Q3.5,
                                    Q3=="no"~'0'),
         paper.cut_WCM=case_when(Q4=="yes"~Q4.5,
                                 Q4=="no"~'0'),
         skinned.knees_BAF=case_when(Q5=="yes"~Q5.5,
                                     Q5=="no"~'0'),
         bruised.leg_BCF=case_when(Q6=="yes"~Q6.5,
                                   Q6=="no"~'0'),
         splinter_BAM=case_when(Q7=="yes"~Q7.5,
                                Q7=="no"~'0'),
         bee.sting_BCM=case_when(Q8=="yes"~Q8.5,
                                 Q8=="no"~'0'))



d_2<- d3 %>%
  filter(condition=="2") %>% 
  select(dot, condition, cp_number, cp_age, cp_sex, Qctrl1,	
         Qctrl1.5,	Qctrl2,	Qctrl2.5,	Qctrl3,	Qctrl3.5,	Qctrl4,	Qctrl4.5,
         Q9,	Q9.5,	Q10,	Q10.5,	Q11,	Q11.5,	Q12,	Q12.5,	Q13,	Q13.5,
         Q14,	Q14.5,	Q15,	Q15.5,	Q16,	Q16.5)%>%
  mutate(paper.cut_WAF=case_when(Q9=="yes"~Q9.5,
                                 Q9=="no"~'0'),
         splinter_WCF=case_when(Q10=="yes"~Q10.5,
                                Q10=="no"~'0'),
         bruised.leg_WAM=case_when(Q11=="yes"~Q11.5,
                                   Q11=="no"~'0'),
         burned.tongue_WCM=case_when(Q12=="yes"~Q12.5,
                                     Q12=="no"~'0'),
         stomach.ache_BAF=case_when(Q13=="yes"~Q13.5,
                                    Q13=="no"~'0'),
         bee.sting_BCF=case_when(Q14=="yes"~Q14.5,
                                 Q14=="no"~'0'),
         broken.arm_BAM=case_when(Q15=="yes"~Q15.5, 
                                  Q15=="no"~'0'),
         skinned.knees_BCM=case_when(Q16=="yes"~Q16.5,
                                     Q16=="no"~'0'))


d_3<-d3 %>% 
  filter(condition=="3") %>%
  select(dot, condition, cp_number, cp_age, cp_sex, Qctrl1,	
         Qctrl1.5,	Qctrl2,	Qctrl2.5,	Qctrl3,	Qctrl3.5,	Qctrl4,	Qctrl4.5,
         Q17, Q17.5, Q18, Q18.5, Q19, Q19.5, Q20, Q20.5, Q21, Q21.5, Q22, Q22.5, Q23, Q23.5, Q24, Q24.5) %>%
  mutate(skinned.knees_WAF=case_when(Q17=="yes"~Q17.5,
                                     Q17=="no"~'0'),
         bruised.leg_WCF=case_when(Q18=="yes"~Q18.5,
                                   Q18=="no"~'0'),
         bee.sting_WAM=case_when(Q19=="yes"~Q19.5,
                                 Q19=="no"~'0'),
         splinter_WCM=case_when(Q20=="yes"~Q20.5,
                                Q20=="no"~'0'),
         paper.cut_BAF=case_when(Q21=="yes"~Q21.5,
                                 Q21=="no"~'0'),
         broken.arm_BCF=case_when(Q22=="yes"~Q22.5, 
                                  Q22=="no"~'0'),
         burned.tongue_BAM=case_when(Q23=="yes"~Q23.5,
                                     Q23=="no"~'0'),
         stomach.ache_BCM=case_when(Q24=="yes"~Q24.5,
                                    Q24=="no"~'0'))

d_4<-d3 %>% 
  filter(condition=="4") %>%
  select(dot, condition, cp_number, cp_age, cp_sex, Qctrl1,	
         Qctrl1.5,	Qctrl2,	Qctrl2.5,	Qctrl3,	Qctrl3.5,	Qctrl4,	Qctrl4.5,
         Q25, Q25.5, Q26, Q26.5, Q27, Q27.5, Q28, Q28.5, Q29, Q29.5, Q30, Q30.5, Q31, Q31.5, Q32, Q32.5) %>%
  mutate(bee.sting_WAF=case_when(Q25=="yes"~Q25.5,
                                 Q25=="no"~'0'),
         stomach.ache_WCF=case_when(Q26=="yes"~Q26.5,
                                    Q26=="no"~'0'),
         broken.arm_WAM=case_when(Q27=="yes"~Q27.5, 
                                  Q27=="no"~'0'),
         skinned.knees_WCM=case_when(Q28=="yes"~Q28.5,
                                     Q28=="no"~'0'),
         splinter_BAF=case_when(Q29=="yes"~Q29.5,
                                Q29=="no"~'0'),
         burned.tongue_BCF=case_when(Q30=="yes"~Q29.5,
                                     Q30=="no"~'0'),
         bruised.leg_BAM=case_when(Q31=="yes"~Q30.5,
                                   Q31=="no"~'0'),
         paper.cut_BCM=case_when(Q32=="yes"~Q32.5,
                                 Q32=="no"~'0'))


d_5<-d3 %>% 
  filter(condition=="5") %>%
  select(dot, condition, cp_number, cp_age, cp_sex, Qctrl1,	
         Qctrl1.5,	Qctrl2,	Qctrl2.5,	Qctrl3,	Qctrl3.5,	Qctrl4,	Qctrl4.5,
         Q33, Q33.5, Q34, Q34.5, Q35, Q35.5, Q36, Q36.5, Q37, Q37.5, Q38, Q38.5, Q39, Q39.5, Q40, Q40.5) %>%
  mutate(stomach.ache_WAF=case_when(Q33=="yes"~Q33.5,
                                    Q33=="no"~'0'),
         burned.tongue_WCF=case_when(Q34=="yes"~Q34.5,
                                     Q34=="no"~'0'),
         splinter_WAM=case_when(Q35=="yes"~Q35.5,
                                Q35=="no"~'0'),
         bee.sting_WCM=case_when(Q36=="yes"~Q36.5,
                                 Q36=="no"~'0'),
         bruised.leg_BAF=case_when(Q37=="yes"~Q37.5,
                                   Q37=="no"~'0'),
         skinned.knees_BCF=case_when(Q38=="yes"~Q38.5,
                                     Q38=="no"~'0'),
         paper.cut_BAM=case_when(Q39=="yes"~Q39.5,
                                 Q39=="no"~'0'),
         broken.arm_BCM=case_when(Q40=="yes"~Q40.5, 
                                  Q40=="no"~'0'))

d_6<-d3 %>% 
  filter(condition=="6") %>%
  select(dot, condition, cp_number, cp_age, cp_sex, Qctrl1,	
         Qctrl1.5,	Qctrl2,	Qctrl2.5,	Qctrl3,	Qctrl3.5,	Qctrl4,	Qctrl4.5,
         Q41, Q41.5, Q42, Q42.5, Q43, Q43.5, Q44, Q44.5, Q45, Q45.5, Q46, Q46.5, Q47, Q47.5, Q48, Q48.5) %>%
  mutate(bruised.leg_WAF=case_when(Q41=="yes"~Q41.5,
                                   Q41=="no"~'0'),
         skinned.knees_WCF=case_when(Q42=="yes"~Q42.5,
                                     Q42=="no"~'0'),
         burned.tongue_WAM=case_when(Q43=="yes"~Q43.5,
                                     Q43=="no"~'0'),
         stomach.ache_WCM=case_when(Q44=="yes"~Q44.5,
                                    Q44=="no"~'0'),
         broken.arm_BAF=case_when(Q45=="yes"~Q45.5, 
                                  Q45=="no"~'0'),
         paper.cut_BCF=case_when(Q46=="yes"~Q46.5,
                                 Q46=="no"~'0'),
         bee.sting_BAM=case_when(Q47=="yes"~Q47.5,
                                 Q47=="no"~'0'),
         splinter_BCM=case_when(Q48=="yes"~Q48.5,
                                Q48=="no"~'0'))

d_7<-d3 %>% 
  filter(condition=="7") %>%
  select(dot, condition, cp_number, cp_age, cp_sex, Qctrl1,	
         Qctrl1.5,	Qctrl2,	Qctrl2.5,	Qctrl3,	Qctrl3.5,	Qctrl4,	Qctrl4.5,
         Q49, Q49.5, Q50, Q50.5, Q51, Q51.5, Q52, Q52.5, Q53, Q53.5, Q54, Q54.5, Q55, Q55.5, Q56, Q56.5) %>%
  mutate(broken.arm_WAF=case_when(Q49=="yes"~Q49.5, # broken arm, exclude 0
                                  Q49=="no"~'0'),
         paper.cut_WCF=case_when(Q50=="yes"~Q50.5,
                                 Q50=="no"~'0'),
         skinned.knees_WAM=case_when(Q51=="yes"~Q51.5,
                                     Q51=="no"~'0'),
         bruised.leg_WCM=case_when(Q52=="yes"~Q52.5,
                                   Q52=="no"~'0'),
         bee.sting_BAF=case_when(Q53=="yes"~Q53.5,
                                 Q53=="no"~'0'),
         splinter_BCF=case_when(Q54=="yes"~Q54.5,
                                Q54=="no"~'0'),
         stomach.ache_BAM=case_when(Q55=="yes"~Q55.5,
                                    Q55=="no"~'0'),
         burned.tongue_BCM=case_when(Q56=="yes"~Q56.5,
                                     Q56=="no"~'0'))

d_8<-d3 %>% 
  filter(condition=="8") %>%
  select(dot, condition, cp_number, cp_age, cp_sex, Qctrl1,	
         Qctrl1.5,	Qctrl2,	Qctrl2.5,	Qctrl3,	Qctrl3.5,	Qctrl4,	Qctrl4.5,
         Q57, Q57.5, Q58, Q58.5, Q59, Q59.5, Q60, Q60.5, Q61, Q61.5, Q62, Q62.5, Q63, Q63.5, Q64, Q64.5) %>%
  mutate(splinter_WAF=case_when(Q57=="yes"~Q57.5, 
                                Q57=="no"~'0'),
         bee.sting_WCF=case_when(Q58=="yes"~Q58.5,
                                 Q58=="no"~'0'),
         paper.cut_WAM=case_when(Q59=="yes"~Q59.5,
                                 Q59=="no"~'0'),
         broken.arm_WCM=case_when(Q60=="yes"~Q60.5, # broken arm, exclude 0
                                  Q60=="no"~'0'),
         burned.tongue_BAF=case_when(Q61=="yes"~Q61.5,
                                     Q61=="no"~'0'),
         stomach.ache_BCF=case_when(Q62=="yes"~Q62.5,
                                    Q62=="no"~'0'),
         skinned.knees_BAM=case_when(Q63=="yes"~Q63.5,
                                     Q63=="no"~'0'),
         bruised.leg_BCM=case_when(Q64=="yes"~Q64.5,
                                   Q64=="no"~'0'))


### wide -> long data

#make column injury in long1 and make demographic categories in long1b
#1
long1 <- d_1 %>% 
  gather(injury, rating, -c(dot, condition, cp_number, cp_age, cp_sex,
                            Qctrl1, Qctrl1.5, Qctrl2,	Qctrl2.5,	Qctrl3,	Qctrl3.5,	Qctrl4,	Qctrl4.5,
                            Q1, Q1.5,	Q2,	Q2.5,	Q3,	Q3.5, Q4,	Q4.5,	Q5,	Q5.5,	Q6,	Q6.5,	Q7,	Q7.5,	Q8,	Q8.5))

long1b <- long1 %>% mutate(race=case_when(injury %in% c('burned.tongue_WAF', 'broken.arm_WCF', 'stomach.ache_WAM', 'paper.cut_WCM') ~ "white",
                                          injury %in% c('skinned.knees_BAF', 'bruised.leg_BCF', 'splinter_BAM', 'bee.sting_BCM') ~ "black"),
                           sex = case_when(injury %in% c('burned.tongue_WAF', 'broken.arm_WCF','skinned.knees_BAF', 'bruised.leg_BCF') ~ "female",
                                           injury %in% c('stomach.ache_WAM', 'paper.cut_WCM','splinter_BAM', 'bee.sting_BCM') ~ "male"), 
                           age = case_when(injury %in% c('burned.tongue_WAF','stomach.ache_WAM','skinned.knees_BAF','splinter_BAM') ~"adult",
                                           injury %in% c('broken.arm_WCF','paper.cut_WCM','bruised.leg_BCF','bee.sting_BCM')~"child"))


#2
long2 <- d_2 %>%
  gather(injury, rating, -c(dot, condition, cp_number, cp_age, cp_sex,
                            Qctrl1, Qctrl1.5, Qctrl2,	Qctrl2.5,	Qctrl3,	Qctrl3.5,	Qctrl4,	Qctrl4.5,
                            Q9,	Q9.5,	Q10,	Q10.5,	Q11,	Q11.5,	Q12,	Q12.5,	Q13,	Q13.5, Q14,	Q14.5,	Q15,	Q15.5,	Q16,	Q16.5))

long2b <- long2 %>% mutate(race=case_when(injury %in% c('paper.cut_WAF','splinter_WCF','bruised.leg_WAM','burned.tongue_WCM') ~ "white",
                                          injury %in% c('stomach.ache_BAF', 'bee.sting_BCF', 'broken.arm_BAM', 'skinned.knees_BCM') ~ "black"),
                           sex = case_when(injury %in% c('paper.cut_WAF','splinter_WCF','stomach.ache_BAF','bee.sting_BCF') ~ "female",
                                           injury %in% c('bruised.leg_WAM','burned.tongue_WCM','broken.arm_BAM', 'skinned.knees_BCM') ~ "male"), 
                           age = case_when(injury %in% c('paper.cut_WAF','bruised.leg_WAM','stomach.ache_BAF','broken.arm_BAM') ~"adult",
                                           injury %in% c('splinter_WCF','burned.tongue_WCM','bee.sting_BCF','skinned.knees_BCM')~"child"))

#3
long3 <- d_3 %>% 
  gather(injury, rating, -c(dot, condition, cp_number, cp_age, cp_sex,
                            Qctrl1, Qctrl1.5, Qctrl2,	Qctrl2.5,	Qctrl3,	Qctrl3.5,	Qctrl4,	Qctrl4.5,
                            Q17, Q17.5, Q18, Q18.5, Q19, Q19.5, Q20, Q20.5, Q21, Q21.5, Q22, Q22.5, Q23, Q23.5, Q24, Q24.5))

long3b <- long3 %>% mutate(race=case_when(injury %in% c('skinned.knees_WAF','bruised.leg_WCF','bee.sting_WAM','splinter_WCM') ~ "white",
                                          injury %in% c('paper.cut_BAF','broken.arm_BCF','burned.tongue_BAM','stomach.ache_BCM') ~ "black"),
                           sex = case_when(injury %in% c('skinned.knees_WAF','bruised.leg_WCF','paper.cut_BAF','broken.arm_BCF') ~ "female",
                                           injury %in% c('bee.sting_WAM','splinter_WCM','burned.tongue_BAM','stomach.ache_BCM') ~ "male"), 
                           age = case_when(injury %in% c('skinned.knees_WAF','bee.sting_WAM','paper.cut_BAF','burned.tongue_BAM') ~"adult",
                                           injury %in% c('bruised.leg_WCF','splinter_WCM','broken.arm_BCF','stomach.ache_BCM')~"child"))
#4
long4 <- d_4 %>% 
  gather(injury, rating, -c(dot, condition, cp_number, cp_age, cp_sex,
                            Qctrl1, Qctrl1.5, Qctrl2,	Qctrl2.5,	Qctrl3,	Qctrl3.5,	Qctrl4,	Qctrl4.5,
                            Q25, Q25.5, Q26, Q26.5, Q27, Q27.5, Q28, Q28.5, Q29, Q29.5, Q30, Q30.5, Q31, Q31.5, Q32, Q32.5))

long4b <- long4 %>% mutate(race=case_when(injury %in% c('bee.sting_WAF','stomach.ache_WCF','broken.arm_WAM','skinned.knees_WCM') ~ "white",
                                          injury %in% c('splinter_BAF','burned.tongue_BCF','bruised.leg_BAM','paper.cut_BCM') ~ "black"),
                           sex = case_when(injury %in% c('bee.sting_WAF','stomach.ache_WCF','splinter_BAF','burned.tongue_BCF') ~ "female",
                                           injury %in% c('broken.arm_WAM','skinned.knees_WCM','bruised.leg_BAM','paper.cut_BCM') ~ "male"), 
                           age = case_when(injury %in% c('bee.sting_WAF','broken.arm_WAM','splinter_BAF','bruised.leg_BAM') ~"adult",
                                           injury %in% c('stomach.ache_WCF','skinned.knees_WCM','burned.tongue_BCF','paper.cut_BCM')~"child"))
# theres two splinter_BAF's that have a 'NA' and not a 0?

#5
long5 <- d_5 %>% 
  gather(injury, rating, -c(dot, condition, cp_number, cp_age, cp_sex,
                            Qctrl1, Qctrl1.5, Qctrl2,	Qctrl2.5,	Qctrl3,	Qctrl3.5,	Qctrl4,	Qctrl4.5,
                            Q33, Q33.5, Q34, Q34.5, Q35, Q35.5, Q36, Q36.5, Q37, Q37.5, Q38, Q38.5, Q39, Q39.5, Q40, Q40.5))

long5b <- long5 %>% mutate(race=case_when(injury %in% c('stomach.ache_WAF','burned.tongue_WCF','splinter_WAM','bee.sting_WCM') ~ "white",
                                          injury %in% c('bruised.leg_BAF', 'skinned.knees_BCF','paper.cut_BAM','broken.arm_BCM') ~ "black"),
                           sex = case_when(injury %in% c('stomach.ache_WAF','burned.tongue_WCF','bruised.leg_BAF', 'skinned.knees_BCF') ~ "female",
                                           injury %in% c('splinter_WAM','bee.sting_WCM','paper.cut_BAM','broken.arm_BCM') ~ "male"), 
                           age = case_when(injury %in% c('stomach.ache_WAF','splinter_WAM','bruised.leg_BAF','paper.cut_BAM') ~"adult",
                                           injury %in% c('burned.tongue_WCF','bee.sting_WCM','skinned.knees_BCF','broken.arm_BCM')~"child"))


#6
long6 <- d_6 %>% 
  gather(injury, rating, -c(dot, condition, cp_number, cp_age, cp_sex,
                            Qctrl1, Qctrl1.5, Qctrl2,	Qctrl2.5,	Qctrl3,	Qctrl3.5,	Qctrl4,	Qctrl4.5,
                            Q41, Q41.5, Q42, Q42.5, Q43, Q43.5, Q44, Q44.5, Q45, Q45.5, Q46, Q46.5, Q47, Q47.5, Q48, Q48.5))

long6b <- long6 %>% mutate(race=case_when(injury %in% c('bruised.leg_WAF','skinned.knees_WCF','burned.tongue_WAM','stomach.ache_WCM') ~ "white",
                                          injury %in% c('broken.arm_BAF','paper.cut_BCF','bee.sting_BAM','splinter_BCM') ~ "black"),
                           sex = case_when(injury %in% c('bruised.leg_WAF','skinned.knees_WCF','broken.arm_BAF','paper.cut_BCF') ~ "female",
                                           injury %in% c('burned.tongue_WAM','stomach.ache_WCM','bee.sting_BAM','splinter_BCM') ~ "male"), 
                           age = case_when(injury %in% c('bruised.leg_WAF','burned.tongue_WAM','broken.arm_BAF','bee.sting_BAM') ~"adult",
                                           injury %in% c('skinned.knees_WCF','stomach.ache_WCM','paper.cut_BCF','splinter_BCM')~"child"))


#7
long7 <- d_7 %>% 
  gather(injury, rating, -c(dot, condition, cp_number, cp_age, cp_sex,
                            Qctrl1, Qctrl1.5, Qctrl2,	Qctrl2.5,	Qctrl3,	Qctrl3.5,	Qctrl4,	Qctrl4.5,
                            Q49, Q49.5, Q50, Q50.5, Q51, Q51.5, Q52, Q52.5, Q53, Q53.5, Q54, Q54.5, Q55, Q55.5, Q56, Q56.5))

long7b <- long7 %>% mutate(race=case_when(injury %in% c('broken.arm_WAF','paper.cut_WCF','skinned.knees_WAM','bruised.leg_WCM') ~ "white",
                                          injury %in% c('bee.sting_BAF','splinter_BCF','stomach.ache_BAM','burned.tongue_BCM') ~ "black"),
                           sex = case_when(injury %in% c('broken.arm_WAF','paper.cut_WCF','bee.sting_BAF','splinter_BCF') ~ "female",
                                           injury %in% c('skinned.knees_WAM','bruised.leg_WCM','stomach.ache_BAM','burned.tongue_BCM') ~ "male"), 
                           age = case_when(injury %in% c('broken.arm_WAF','skinned.knees_WAM','bee.sting_BAF','stomach.ache_BAM') ~"adult",
                                           injury %in% c('paper.cut_WCF','bruised.leg_WCM','splinter_BCF','burned.tongue_BCM')~"child"))

#8
long8 <- d_8 %>% 
  gather(injury, rating, -c(dot, condition, cp_number, cp_age, cp_sex,
                            Qctrl1, Qctrl1.5, Qctrl2,	Qctrl2.5,	Qctrl3,	Qctrl3.5,	Qctrl4,	Qctrl4.5,
                            Q57, Q57.5, Q58, Q58.5, Q59, Q59.5, Q60, Q60.5, Q61, Q61.5, Q62, Q62.5, Q63, Q63.5, Q64, Q64.5))

long8b <- long8 %>% mutate(race=case_when(injury %in% c('splinter_WAF','bee.sting_WCF','paper.cut_WAM','broken.arm_WCM') ~ "white",
                                          injury %in% c('burned.tongue_BAF','stomach.ache_BCF','skinned.knees_BAM','bruised.leg_BCM') ~ "black"),
                           sex = case_when(injury %in% c('splinter_WAF','bee.sting_WCF','burned.tongue_BAF','stomach.ache_BCF') ~ "female",
                                           injury %in% c('paper.cut_WAM','broken.arm_WCM','skinned.knees_BAM','bruised.leg_BCM') ~ "male"), 
                           age = case_when(injury %in% c('splinter_WAF','paper.cut_WAM','burned.tongue_BAF','skinned.knees_BAM') ~"adult",
                                           injury %in% c('bee.sting_WCF','broken.arm_WCM','stomach.ache_BCF','bruised.leg_BCM')~"child"))



# all long data combined
long_all <- bind_rows(long1b, long2b, long3b, long4b, long5b, long6b, long7b, long8b)


### fix names of injuries
#burned tongue
long_all$injury=recode_factor(long_all$injury,
                           'burned.tongue_WAF'='burned.tongue',
                           'burned.tongue_WCF'='burned.tongue',
                           'burned.tongue_WAM'='burned.tongue',
                           'burned.tongue_WCM'='burned.tongue',
                           'burned.tongue_BAF'='burned.tongue',
                           'burned.tongue_BCF'='burned.tongue',
                           'burned.tongue_BAM'='burned.tongue',
                           'burned.tongue_BCM'='burned.tongue')
#broken arm
long_all$injury=recode_factor(long_all$injury,
                           'broken.arm_WAF'='broken.arm',
                           'broken.arm_WCF'='broken.arm',
                           'broken.arm_WAM'='broken.arm',
                           'broken.arm_WCM'='broken.arm',
                           'broken.arm_BAF'='broken.arm',
                           'broken.arm_BCF'='broken.arm',
                           'broken.arm_BAM'='broken.arm',
                           'broken.arm_BCM'='broken.arm')
#stomach ache
long_all$injury=recode_factor(long_all$injury,
                           'stomach.ache_WAF'='stomach.ache',
                           'stomach.ache_WCF'='stomach.ache',
                           'stomach.ache_WAM'='stomach.ache',
                           'stomach.ache_WCM'='stomach.ache',
                           'stomach.ache_BAF'='stomach.ache',
                           'stomach.ache_BCF'='stomach.ache',
                           'stomach.ache_BAM'='stomach.ache',
                           'stomach.ache_BCM'='stomach.ache')
#paper cut
long_all$injury=recode_factor(long_all$injury,
                           'paper.cut_WAF'='paper.cut',
                           'paper.cut_WCF'='paper.cut',
                           'paper.cut_WAM'='paper.cut',
                           'paper.cut_WCM'='paper.cut',
                           'paper.cut_BAF'='paper.cut',
                           'paper.cut_BCF'='paper.cut',
                           'paper.cut_BAM'='paper.cut',
                           'paper.cut_BCM'='paper.cut')
#skinned knees
long_all$injury=recode_factor(long_all$injury,
                           'skinned.knees_WAF'='skinned.knees',
                           'skinned.knees_WCF'='skinned.knees',
                           'skinned.knees_WAM'='skinned.knees',
                           'skinned.knees_WCM'='skinned.knees',
                           'skinned.knees_BAF'='skinned.knees',
                           'skinned.knees_BCF'='skinned.knees',
                           'skinned.knees_BAM'='skinned.knees',
                           'skinned.knees_BCM'='skinned.knees')
#bruised leg
long_all$injury=recode_factor(long_all$injury,
                           'bruised.leg_WAF'='bruised.leg',
                           'bruised.leg_WCF'='bruised.leg',
                           'bruised.leg_WAM'='bruised.leg',
                           'bruised.leg_WCM'='bruised.leg',
                           'bruised.leg_BAF'='bruised.leg',
                           'bruised.leg_BCF'='bruised.leg',
                           'bruised.leg_BAM'='bruised.leg',
                           'bruised.leg_BCM'='bruised.leg')
#splinter
long_all$injury=recode_factor(long_all$injury,
                           'splinter_WAF'='splinter',
                           'splinter_WCF'='splinter',
                           'splinter_WAM'='splinter',
                           'splinter_WCM'='splinter',
                           'splinter_BAF'='splinter',
                           'splinter_BCF'='splinter',
                           'splinter_BAM'='splinter',
                           'splinter_BCM'='splinter')
#bee sting
long_all$injury=recode_factor(long_all$injury,
                           'bee.sting_WAF'='bee.sting',
                           'bee.sting_WCF'='bee.sting',
                           'bee.sting_WAM'='bee.sting',
                           'bee.sting_WCM'='bee.sting',
                           'bee.sting_BAF'='bee.sting',
                           'bee.sting_BCF'='bee.sting',
                           'bee.sting_BAM'='bee.sting',
                           'bee.sting_BCM'='bee.sting')

# delete unnecessary columns (idk why it had to be split)
long_all<- long_all %>% 
  select( -Q1, -Q1.5, -Q2, -Q2.5, -Q3,-Q3.5, -Q4, -Q4.5, -Q5, -Q5.5, -Q6, -Q6.5, -Q7, -Q7.5, -Q8, -Q8.5)

long_all<- long_all %>% 
  select(-Qctrl1, -Qctrl1.5, -Qctrl2, -Qctrl2.5,-Qctrl3, -Qctrl3.5, -Qctrl4, -Qctrl4.5,
         -Q9, -Q9.5, -Q10, -Q10.5, -Q11, -Q11.5, -Q12, -Q12.5, -Q13, -Q13.5, -Q14,
         -Q14.5, -Q15, -Q15.5, -Q16, -Q16.5,
         -Q17, -Q17.5, -Q18, -Q18.5,-Q19,-Q19.5, -Q20, -Q20.5, -Q21, -Q21.5, -Q22,
         -Q22.5, -Q23, -Q23.5, -Q24, -Q24.5,
         -Q25, -Q25.5, -Q26, -Q26.5, -Q27, -Q27.5, -Q28, -Q28.5, -Q29, -Q29.5, -Q30,
         -Q30.5, -Q31, -Q31.5, -Q32, -Q32.5,
         -Q33, -Q33.5, -Q34, -Q34.5, -Q35, -Q35.5, -Q36, -Q36.5, -Q37, -Q37.5, -Q38,
         -Q38.5, -Q39, -Q39.5, -Q40, -Q40.5,
         -Q41, -Q41.5, -Q42, -Q42.5, -Q43, -Q43.5, -Q44, -Q44.5, -Q45, -Q45.5, -Q46,
         -Q46.5, -Q47, -Q47.5, -Q48, -Q48.5,
         -Q49, -Q49.5, -Q50, -Q50.5, -Q51, -Q51.5, -Q52, -Q52.5, -Q53, -Q53.5, -Q54,
         -Q54.5, -Q55, -Q55.5, -Q56, -Q56.5,
         -Q57, -Q57.5, -Q58, -Q58.5,-Q59, -Q59.5, -Q60, -Q60.5, -Q61, -Q61.5, -Q62,
         -Q62.5, -Q63, -Q63.5, -Q64, -Q64.5)

long_all$rating <- as.numeric(long_all$rating)
  
cp <- long_all

View(cp)


cp <- cp %>%
  mutate(bday= case_when(cp_number == "4.1.1" ~ "2018-01-10", # 4's
                         cp_number == "4.2.1" ~ "2017-12-16", 
                         cp_number == "4.4.1" ~ "2017-08-10",
                         cp_number == "4.5.1" ~ "2017-08-29",
                         cp_number == "4.6.1" ~ "2018-01-19",
                         cp_number == "4.7.1" ~ "2017-07-04",
                         cp_number == "4.8.1" ~ "2017-05-23",
                         cp_number == "4.1.2" ~ "2017-07-06",
                         cp_number == "4.2.2" ~ "2018-05-16",
                         cp_number == "4.3.2" ~ "2017-11-25",
                         cp_number == "4.4.2" ~ "2017-07-05",
                         cp_number == "4.5.2" ~ "2017-08-23",
                         cp_number == "4.6.2" ~ "2017-06-13",
                         cp_number == "4.7.2" ~ "2017-07-06",
                         cp_number == "4.8.2" ~ "2017-05-15",
                         cp_number == "4.1.3" ~ "2017-07-07",
                         cp_number == "4.2.3" ~ "2018-06-17",
                         cp_number == "4.3.3" ~ "2017-09-15",
                         cp_number == "4.4.3" ~ "2017-05-23",
                         cp_number == "4.5.3" ~ "2018-03-21",
                         cp_number == "4.6.3" ~ "2017-11-04",
                         cp_number == "4.7.3" ~ "2018-06-27",
                         cp_number == "4.8.3" ~ "2017-09-15",
                         cp_number == "4.1.4" ~ "2017-09-21",
                         cp_number == "4.2.4" ~ "2018-06-15",
                         cp_number == "4.3.4" ~ "2018-03-14",
                         cp_number == "4.4.4" ~ "2018-06-21",
                         cp_number == "5.1.1" ~ "2017-03-22", #5's
                         cp_number == "5.2.1" ~ "2016-12-30",
                         cp_number == "5.3.1" ~ "2016-06-01",
                         cp_number == "5.4.1" ~ "2016-08-09",
                         cp_number == "5.5.1" ~ "2016-08-09",
                         cp_number == "5.6.1" ~ "2016-12-31",
                         cp_number == "5.7.1" ~ "2016-08-28",
                         cp_number == "5.8.1" ~ "2017-02-23",
                         cp_number == "5.1.2" ~ "2016-12-17",
                         cp_number == "5.2.2" ~ "2016-06-14",
                         cp_number == "5.3.2" ~ "2017-02-24",
                         cp_number == "5.4.2" ~ "2016-10-30",
                         cp_number == "5.5.2" ~ "2017-03-14",
                         cp_number == "5.6.2" ~ "2016-08-30",
                         cp_number == "5.7.2" ~ "2016-12-13",
                         cp_number == "5.8.2" ~ "2016-09-13",
                         cp_number == "5.1.3" ~ "2016-10-29",
                         cp_number == "5.2.3" ~ "2016-10-19",
                         cp_number == "5.3.3" ~ "2017-01-17",
                         cp_number == "5.4.3" ~ "2016-01-16",
                         cp_number == "5.5.3" ~ "2017-03-14",
                         cp_number == "5.6.3" ~ "2016-09-12",
                         cp_number == "5.7.3" ~ "2017-01-02",
                         cp_number == "5.8.3" ~ "2016-10-24",
                         cp_number == "5.1.4" ~ "2016-10-05",
                         cp_number == "5.2.4" ~ "2016-11-10",
                         cp_number == "5.5.4" ~ "2016-10-29",
                         cp_number == "5.5.5" ~ "2016-12-08",
                         cp_number == "6.1.1" ~ "2015-12-16", # 6's
                         cp_number == "6.2.1" ~ "2015-12-10",
                         cp_number == "6.3.1" ~ "2015-10-14",
                         cp_number == "6.4.1" ~ "2015-10-12",
                         cp_number == "6.5.1" ~ "2016-03-31",
                         cp_number == "6.6.1" ~ "2016-07-14"))

# need eeptools package for this to work vvv

cp <- cp %>% 
  mutate(bday = as.Date(cp$bday, "%Y-%m-%d"))

cp <- cp %>% 
  mutate(dot = as.Date(cp$dot, "%Y-%m-%d"))

cp_exact_age <- age_calc(cp$bday,          # Convert birth to age
                  cp$dot,
                  units = "years")

cp <- cp %>%
  mutate(cp_exact_age = cp_exact_age)

View(cp)                          


# n by age group

cpsumm <- cp %>%
  group_by(cp_age) %>%
  summarise(
    n = (n()/8))

print(cpsumm)

# exclusions

exclusions <- d2 %>%
  filter(exclude == 1)
  
View(exclusions)




