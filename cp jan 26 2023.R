#Jan 26, 2023 CP data
#hopeful significance with having more than my existing thesis 4 n=31, 5 n=32? 
      #(9 6 y/os but not looking at them in thesis)

#Pasting code but changing data sheet upload path

library(readxl)
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)
library(gitcreds)

#old path: cp_d <- read.csv("/Users/nicolesteiner/Desktop/research/pain/data collection/CP/data excel/8 pain kids 12.7.22.csv") #on laptop, copy path/different file name. this is lab desktop edits.

cp_d<- read.csv("CP Github/data/cpdataposter.csv")

colnames(cp_d) #what are columns named now?

cp_d_ratings <- cp_d[-c(1:2),c(1,6,7,19,20,21,22:165,167)] ## which columns to keep/delete? ##add 19, 20, removed

#what to pivot?
colnames(cp_d_ratings) 

#pivot time, will diff from adult
cp_p <- cp_d_ratings %>% pivot_longer(cols=c(13:148),values_to = "rating", names_to = "Stimulus",values_drop_na = F)
#(same as adult data) This will still have many NA values in ratings, for the questions not seen in that condition
#Don't remove NAs before the next step, otherwise you lose the trials following "no" answer

#Check for "no" responses and add a zero for the next response (in p)
for(i in 1:nrow(cp_p)){
  if(!is.na(cp_p$rating[i])){ #check if rating[i] is NA
    if(cp_p$rating[i]=="no"){  #if so, check if it is "no"
      cp_p$rating[i+1] <- "0"    #if so, replace the next rating with "0"
    }
  }
}
colnames(cp_p)

#Keep only the numerical responses, which correspond to Questions with underscores in them
cp_q <- cp_p %>% filter(grepl("_",Stimulus)) #grepl checks for matches to the string

#make the ratings numeric (they were characters)
cp_q$rating <- as.numeric(cp_q$rating)

#Give unique IDs (this column is where 'excluded' was kept added to ID#, so may want to access unchanged later.]
cp_q$ID <- as.numeric(factor(cp_q$`Q.participant.`))


#Check who responds with non-zero to the control questions (exclusion step)
cp_k <- unique(
  c(
    cp_q[which(cp_q$Stimulus=="Qc1.5_1"& cp_q$rating!=0),]$ID, #check for non-zero for each row, then get those IDs
    cp_q[which(cp_q$Stimulus=="Qc2.5_1"& cp_q$rating!=0),]$ID,
    cp_q[which(cp_q$Stimulus=="Qc3.5_1"& cp_q$rating!=0),]$ID,
    cp_q[which(cp_q$Stimulus=="Qc4.5_1"& cp_q$rating!=0),]$ID
  )
)


drop_index <- is.element(cp_q$ID,cp_k) #illegal observations!
cp_q <- cp_q[!drop_index,] #Keep only the legal ones, ! means opposite or "not"

cp_q <- cp_q %>% filter(!grepl("c",Stimulus))

#check row by row for NA in rating column, and keep only non-NAs... huge step! yay no useless NAs now.
cp_q <- cp_q[!is.na(cp_q$rating),]

colnames(cp_d)
#Find the questions and their text so we can match stim demos to the #q's are:
#in this find corresponding rating/question text, to help add in right character demos
#stimulus = question ratings 0-7, text = question text content 'hi im rachel' etc.
cp_d_text <- cp_d[c(1),c(28:163)] %>% ## unsure if correct changes? ## especially 1:266-> ?? #add the number in this line = 136
  pivot_longer(cols=1:136,values_to = "text", 
               names_to = "Stimulus",values_drop_na = F)
#yes!!


#Put the text and questions together more conveniently
#for(i in ____) means iterate the i= to be whatever you tell it in ____
#seq(2,nrow(d_text),2) gives sequence of integers, the ,2 means every other one
#it starts at 2, goes to nrow(d_text) which equals 266 in this case
for(i in seq(2,nrow(cp_d_text),2)){
  cp_d_text[i,2] <- cp_d_text[i-1,2]
  print(i)  #check the iteration is working for the loop (should be every even row)
}

#Keep only questions with underscores   #(was a random ambiguous trait chosen that worked out the way i had things numbered w underscores)
cp_d_text <- cp_d_text %>% filter(grepl("_",Stimulus))

#make the demos... and in the correct order of the associated questions, as selected out above^
stim_names <- c("Rachel", "Sydney", "David", "Kyle", "Marissa", "Jessica", "Chris", "Tyler")

stim_race <- c("White","White","White","White","Black","Black","Black","Black")

stim_age <- c(rep(c("adult","child"),2),rep(c("child","adult"),2))

stim_sex <- c("female","female","male","male","female","female","male","male")

#Put together all the stimulus properties into one dataframe
stim_properties <- cbind(stim_names,stim_race,stim_age,stim_sex)

#Setup the new columns in the data frame as empty to be filled in
cp_q$stim_names <- cp_q$stim_race <- cp_q$stim_age <- cp_q$stim_sex <- NA

#Make a loop iterating each i
#There are 16 names, so it goes through 16 times
for(i in 1:length(stim_names)){
  #now implement that loop
  #Find the questions with each name
  #e.g. if i=1, find the questions with name stim_names[1] ("Rachel")
  stim_match <- cp_d_text[grep(stim_names[i],cp_d_text$text),1]$Stimulus
  
  #Which rows of the data frame (q) are those trials
  keep_index <- is.element(cp_q$Stimulus,stim_match)
  
  #Add the stimulus demos to the new columns for those rows identified
  #e.g. i=1 ("Rachel" trials) then add "Rachel" "white" "adult" "female"
  cp_q[keep_index,]$stim_names <-stim_properties[i,1]
  cp_q[keep_index,]$stim_race  <-stim_properties[i,2]
  cp_q[keep_index,]$stim_age   <-stim_properties[i,3]
  cp_q[keep_index,]$stim_sex   <-stim_properties[i,4]
}

InjuryList <- c("burned tongue","broken arm","stomach ache","paper cut",
                "skinned knees","bruised leg","splinter","bee sting")
InjuryPhrases <- c("burned", "broke", "stomach", "cut", "skinned", "bruised",
                   "splinter","stung")

cp_q$injury <- NA #make column everything can go into 

#Goes through each injury type and adds a column with that injury
#the key words in the strings are in InjuryPhrases
#the factor names to use are in InjuryList
for(i in 1:length(InjuryPhrases)){
  #now implement that loop
  #Find the questions with each injury keyword
  #e.g. if i=1, find the questions with injury phrase "burned"
  stim_match <- cp_d_text[grep(InjuryPhrases[i],cp_d_text$text),1]$Stimulus
  
  #Which rows of the data frame (q) are those trials
  #TRUE/FALSE for keep/not
  keep_index <- is.element(cp_q$Stimulus,stim_match)
  
  #Add the injury to the new columns for those rows identified by keep_index
  #e.g. i=1 ("burned tongue" trials) then add "burned tongue" 
  cp_q[keep_index,]$injury <- InjuryList[i]
}


#will now do exclusions:  broken arm 0's  AND THEN too short time

cp_ok <- unique(
  c(
    cp_q[which(cp_q$Stimulus=="Q2.5_1"& cp_q$rating==0),]$ID, #WCF, condition 1
    cp_q[which(cp_q$Stimulus=="Q15.5_1"& cp_q$rating==0),]$ID,#BCM, condition 2
    cp_q[which(cp_q$Stimulus=="Q22.5_1"& cp_q$rating==0),]$ID,#BCF, condition 3
    cp_q[which(cp_q$Stimulus=="Q27.5_1"& cp_q$rating==0),]$ID,#WAM, condition 4
    cp_q[which(cp_q$Stimulus=="Q40.5_1"& cp_q$rating==0),]$ID,#BAM, condition 5
    cp_q[which(cp_q$Stimulus=="Q45.5_1"& cp_q$rating==0),]$ID,#BAF, condition 6
    cp_q[which(cp_q$Stimulus=="Q49.5_1"& cp_q$rating==0),]$ID,#WAF, condition 7
    cp_q[which(cp_q$Stimulus=="Q60.5_1"& cp_q$rating==0),]$ID #WAM, condition 8
  )
)


drop_index <- is.element(cp_q$ID,cp_ok) #remove illegal observations aka broken arm 0s

cp_q <- cp_q[!drop_index,] #Keep only the legal ones, ! means opposite or "not"

cp_ok #one person dropped for rating broken arm as a 0

#add exclusion for any 'finished' that is 'False'
cp_f <- unique(
  c(
    cp_q[which(cp_q$Finished=="False"),]$ID
  )
)

drop_index <- is.element(cp_q$ID,cp_f)
cp_q <- cp_q[!drop_index,]
cp_f
#dropped none based on that (already excluded probs)
#no need to make exclusions about too fast or too slow

## need to exclude others who in 'Q.participant.' have [excluded]
#note this is a very manual, not systematic exclusion process, prob could update? if needed
cp_ex <- unique(
  c(
    cp_q[which(cp_q$Q.participant.=="4.1.3 [excluded; already tested]"),]$ID, #specific exclusion ID
    cp_q[which(cp_q$Q.participant.=="4.3.2e [excluded]"),]$ID #another specific exclusion ID
  )
)

drop_index <- is.element(cp_q$ID,cp_ex) #remove illegal observations aka broken arm 0s
cp_q <- cp_q[!drop_index,] #Keep only the legal ones, ! means opposite or "not"

#make child age column
cp_q$age<-NA

for(i in 1:nrow(cp_q)){
  number<-cp_q$`Q.participant.`[i]
  if(substr(number,1,1)=="C"){
    cp_q$age[i]<-as.numeric(substr(number,4,4))
  }else{
    cp_q$age[i]<-as.numeric(substr(number,1,1))
    
  }
  
}

cp_q$injury<- as.factor(cp_q$injury)

# column stuff
colnames(cp_q) # be mindful of cp_456s is 4-6 year olds, but cp_45s just 4/5 year olds
# rename some variables to shorten
vars = c('startdate','duration','finished','condition','participant number','p_gender',
         'practice1','practice1_rating5','practice2','practice2_rating0','practice3','practice3_rating',
         'should exclude?','should exclude text','experienced pains','Stimulus','rating','ID','stim_sex',
         'stim_age','stim_race','stim_names','injury','age')
colnames(cp_q) = vars



###### 444 555 666 777 .. AGE SECTIONS/ EXCLUDES .. 444 555 666 777 ###############
#(read below too) this is removing 7 year olds
unique(cp_q$ID) #4-7 year olds, total = 77 kids??

#keep 4,5,6 yrs, filter out 7s
cp_456s<-cp_q%>%filter(age!=7)
#idk what this is, counting?
cp_456s %>% count(rating, wt=NULL, sort=TRUE, name = NULL)

#just 4s/5s
cp_45s<-cp_q%>%filter(age!=7)%>%filter(age!=6)


#### filter singular ages ###

#ONLY 4 year olds
cp_4s<-cp_q%>%filter(age!=5)%>%filter(age!=6)%>%filter(age!=7)

#ONLY 5 year olds
cp_5s<-cp_q%>%filter(age!=4)%>%filter(age!=6)%>%filter(age!=7)



#```#```#```` #```#```````````````````````````````````````````````````#```#```` #```#```#````
#```#```#```` column renames, each filtered data set  `````````` #```#```#```` 
#```#```#```` ````````````````````````````````````````````````````````````` #```#```#````
# may have been done by cp_q, before filtered age data groups, unsure

colnames(cp_45s) # be mindful of cp_456s is 4-6 year olds, but cp_45s just 4/5 year olds
# rename some variables to shorten
vars = c('startdate','duration','finished','condition','participant number','p_gender',
         'practice1','practice1_rating5','practice2','practice2_rating0','practice3','practice3_rating',
         'should exclude?','should exclude text','experienced pains','Stimulus','rating','ID','stim_sex',
         'stim_age','stim_race','stim_names','injury','age')
colnames(cp_45s) = vars

colnames(cp_456s) #just 4/5 year olds
# rename some variables to shorten
vars = c('startdate','duration','finished','condition','participant number','p_gender',
         'practice1','practice1_rating5','practice2','practice2_rating0','practice3','practice3_rating',
         'should exclude?','should exclude text','experienced pains','Stimulus','rating','ID','stim_sex',
         'stim_age','stim_race','stim_names','injury','age')
colnames(cp_456s) = vars

colnames(cp_4s) # 4s
# rename some variables to shorten
vars = c('startdate','duration','finished','condition','participant number','p_gender',
         'practice1','practice1_rating5','practice2','practice2_rating0','practice3','practice3_rating',
         'should exclude?','should exclude text','experienced pains','Stimulus','rating','ID','stim_sex',
         'stim_age','stim_race','stim_names','injury','age')
colnames(cp_4s) = vars

colnames(cp_5s) # 5s
# rename some variables to shorten
vars = c('startdate','duration','finished','condition','participant number','p_gender',
         'practice1','practice1_rating5','practice2','practice2_rating0','practice3','practice3_rating',
         'should exclude?','should exclude text','experienced pains','Stimulus','rating','ID','stim_sex',
         'stim_age','stim_race','stim_names','injury','age')
colnames(cp_5s) = vars

    ###############################################
################    BASIC ANALYSES!!  456's  ##################
    ###############################################
#make the input cp_456s to look at ALL data, but for thesis, doing just cp_45s

#many means
cp_d.means<- cp_45s %>%
  group_by(injury, stim_race, stim_sex, stim_age) %>%
  summarize(n=n(),   
            mean.rating  = mean(rating), 
            sd.rating = sd(rating))

# just stim sex
cp_stim.sex.means <- cp_45s %>% 
  group_by(stim_sex) %>%
  summarize(n = n(),
            mean.sex = mean(rating),
            median.sex = median(rating))

#just stim race
cp_stim.race.means <- cp_45s %>%
  group_by(stim_race) %>%
  summarize(n = n(),
            mean.race = mean(rating),
            median.race = median(rating))

#just stim age
cp_stim.age.means <- cp_45s %>%
  group_by(stim_age) %>%
  summarize(n = n(),
            mean.age = mean(rating),
            median.age = median(rating)) 

#char names
cp_name.means <- cp_45s %>%
  group_by(stim_names) %>%
  summarize(n = n(),
            mean.name = mean(rating),
            median.name= median(rating)) 

#injury means
cp_injury.means <- cp_45s %>%
  group_by(injury) %>%
  summarize(n = n(),
            mean.name = mean(rating),
            median.name= median(rating)) 

#just age
cp_p_age.means <- cp_45s %>%
  group_by(age) %>%
  summarize(n = n(),
            mean.name = mean(rating),
            median.name= median(rating),
            sd.name=sd(rating))

#participant all info means
cp_genderxage.means <- cp_45s %>%
  group_by(p_gender, age) %>%
  summarize(n = n(),
            mean.p_gender = mean(rating),
            median.p_gender = median(rating),
            sd.rating=sd(rating))

#cp_agexinjury
cp_agexinjury.means <- cp_45s %>%
  group_by(age, injury) %>%
  summarize(n = n(),
            mean.page = mean(rating),
            median.page = median(rating),
            sd.rating=sd(rating))

#p_genderxstimage
cp_genxstim.age.means <- cp_45s %>%
  group_by(stim_age, p_gender) %>%
  summarize(n = n(),
            mean.page = mean(rating),
            median.page = median(rating),
            sd.rating=sd(rating))
#QUESTION: how can I count the number of 0 ratings in each injury??

######    ######    ######    ######    stats!!!!!    ######    ######    ######    ######    ######    ######


  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~...........~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~~~~~~~~~~~~~~~~~~~~~~the models redone Jan 26, 2023~~~~~~~~~~~~~~~~~~~~~~~~##
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.........~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

unique(cp_45s$ID) #70 or 71 valid 4 and 5s? rather than just 63
cp_n <- cp_45s %>% count(ID, wt=NULL, sort=TRUE, name = NULL) #repeat of ID number 95?? why?

#interactions character demos:
kcplm1 <- lmer(rating ~ injury + stim_sex * stim_race * stim_age + (1|ID), data=cp_45s) #injury/stim/race 3 way interaction between 4:5s, but not 4/5/6s.
summary(kcplm1) #5s/6s, show interaction stim_sex:stim_age
Anova(kcplm1, Type=3) #look into result of 3 way int injury:stim_race:stim_age....?

#participant demos only:
kcplm2 <- lmer(rating ~ injury * age + p_gender + (1|ID), data=cp_45s)
summary(kcplm2)
Anova(kcplm2, Type=3) #45s, interaction of participant age : injury... marginal w 456s

kcplm3 <- lmer(rating ~ injury + age + p_gender * stim_age + stim_race + stim_sex + (1|ID), data=cp_45s)
summary(kcplm3)
Anova(kcplm3, Type=3)

#make p gender /stim age
ggplot(cp_45s, aes(x = factor(stim_age), y = rating)) + 
  facet_wrap(~ p_gender)+
  geom_bar(stat = "summary", fun = "mean")+
  stat_summary(fun.data= "mean_se",
               geom="errorbar",
               width = .2)

#stim sex/race + p age
ggplot(cp_45s, aes(x = factor(stim_sex), y = rating)) + 
  facet_wrap(~ p_gender + stim_race)+
  geom_bar(stat = "summary", fun = "mean")+
  stat_summary(fun.data= "mean_se",
               geom="errorbar",
               width = .2)

#kid age/stim race
ggplot(cp_45s, aes(x = factor(age), y = rating)) + 
  facet_wrap(~ stim_race)+
  geom_bar(stat = "summary", fun = "mean")+
  stat_summary(fun.data= "mean_se",
               geom="errorbar",
               width = .2)

#age/injuries bar plot?
ggplot(cp_45s, aes(x = factor(injury), y = rating)) + 
  facet_wrap(~ age)+
  geom_bar(stat = "summary", fun = "mean")+
  stat_summary(fun.data= "mean_se",
               geom="errorbar",
               width = .2)

#re-plot 4 and 5s separately, and order them as adult avgs...
cinjurysum <- cp_4s %>%
  group_by(injury) %>%
  summarise(
    counts = n(),
    means = mean(rating, digits = 5),
    sds = sd(rating)) %>%
  mutate(ses = sds/sqrt(counts)) %>%
  mutate(cis = 1.96*ses) 

#pt 2 of inj avgs plot (pt 1 above)
cinjplot4 <- cinjurysum %>%
  mutate(injury = fct_relevel(cinjurysum$injury, "paper cut", "splinter", "bruised leg",
                              "burned tongue", "stomach ache", "bee sting", "skinned knees", 
                              "broken arm")) %>% # manually ordered by ascending mean; can just do ggplot(aes(x=reorder(injury, means), y=means) but not in rainbow order
  ggplot(aes(x=injury, y=means)) +
  geom_col(aes(fill= factor(injury))) +
  geom_errorbar(aes(ymin = means - cis, ymax = means + cis), width = 0.3) +
  labs(x="Injury", y="Mean Pain Rating (0-7)") +
  ggtitle("Plot of Mean Pain Ratings of 4-year-olds by Injury") +
  theme_bw()

print(cinjplot4)

##now 5s
cinjurysum <- cp_5s %>%
  group_by(injury) %>%
  summarise(
    counts = n(),
    means = mean(rating, digits = 5),
    sds = sd(rating)) %>%
  mutate(ses = sds/sqrt(counts)) %>%
  mutate(cis = 1.96*ses) 

#pt 2 of inj avgs plot (pt 1 above)
cinjplot5s <- cinjurysum %>%
  mutate(injury = fct_relevel(cinjurysum$injury, "paper cut", "splinter", "bruised leg",
                              "burned tongue", "stomach ache", "bee sting", "skinned knees", 
                              "broken arm")) %>% # manually ordered by ascending mean; can just do ggplot(aes(x=reorder(injury, means), y=means) but not in rainbow order
  ggplot(aes(x=injury, y=means)) +
  geom_col(aes(fill= factor(injury))) +
  geom_errorbar(aes(ymin = means - cis, ymax = means + cis), width = 0.3) +
  labs(x="Injury", y="Mean Pain Rating (0-7)") +
  ggtitle("Plot of Mean Pain Ratings of 5-year-olds by Injury") +
  theme_bw()

print(cinjplot5s)


##### singular ages analyses?
ocplm1 <- lmer(rating ~ injury + stim_sex * stim_race * stim_age + (1|ID), data=cp_5s) #injury/stim/race 3 way interaction between 4:5s, but not 4/5/6s.
summary(ocplm1) #5s/6s, show interaction stim_sex:stim_age
Anova(ocplm1, Type=3) #look into result of 3 way int injury:stim_race:stim_age....?


# ^^^^^^^^^^^^^^^^^^^^^^^^^^^    plots     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ #

#bar plot of kids injuries:
## injury sum, then geom_col w error bars

cinjurysum <- cp_45s %>%
  group_by(injury) %>%
  summarise(
    counts = n(),
    means = mean(rating, digits = 5),
    sds = sd(rating)) %>%
  mutate(ses = sds/sqrt(counts)) %>%
  mutate(cis = 1.96*ses) 

#pt 2 of inj avgs plot (pt 1 above)
cinjplot <- cinjurysum %>%
  mutate(injury = fct_relevel(cinjurysum$injury, "stomach ache", "bruised leg", "burned tongue",
                              "paper cut", "skinned knees", "splinter", "bee sting", 
                              "broken arm")) %>% # manually ordered by ascending mean; can just do ggplot(aes(x=reorder(injury, means), y=means) but not in rainbow order
  ggplot(aes(x=injury, y=means)) +
  geom_col(aes(fill= factor(injury))) +
  geom_errorbar(aes(ymin = means - cis, ymax = means + cis), width = 0.3) +
  labs(x="Injury", y="Mean Pain Rating (0-7)") +
  ggtitle("Plot of Mean Pain Rating by Injury with 95% Confidence Intervals") +
  theme_bw()

print(cinjplot)

