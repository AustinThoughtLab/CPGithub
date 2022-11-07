#CP kids analyze better... using adult code work w dr etz to streamline this
    ## the last version I did w katie took 550 lines of code, when it could take 100 this way
    ## also had to specific EVERY question number in that one, instead of more generalizing / formidable coding
library(readxl)
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)
library(gitcreds)

#but lets try!

cp_d <- read.csv("/Users/nicolesteiner/Desktop/research/pain/data collection/CP/data excel/8 pain kids 10.25.22.csv") #on laptop, copy path/different file name. this is lab desktop edits.


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
#Find the questions and their text so we can match stim demos to them #q's are:
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

#Keep only questions with underscores
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
#FILTER age

cp_45s<-cp_q%>%filter(age!=7)%>%filter(age!=6)


      ###############################################
  ################     ANALYSES!!    ##################
      ###############################################
#means

#many means
cp_d.means<- cp_q %>%
  group_by(injury, stim_race, stim_sex, stim_age) %>%
  summarize(n=n(),   
            mean.rating  = mean(rating), 
            sd.rating = sd(rating))

# just stim sex
cp_stim.sex.means <- cp_q %>% 
  group_by(stim_sex) %>%
  summarize(n = n(),
            mean.sex = mean(rating),
            median.sex = median(rating))

#just stim race... but not yet POC/white coded
cp_stim.race.means <- cp_q %>%
  group_by(stim_race) %>%
  summarize(n = n(),
            mean.race = mean(rating),
            median.race = median(rating))

#just stim age
cp_stim.age.means <- cp_q %>%
  group_by(stim_age) %>%
  summarize(n = n(),
            mean.age = mean(rating),
            median.age = median(rating)) 

#char names
cp_name.means <- cp_q %>%
  group_by(stim_names) %>%
  summarize(n = n(),
            mean.name = mean(rating),
            median.name= median(rating)) 

cp_injury.means <- cp_q %>%
  group_by(injury) %>%
  summarize(n = n(),
            mean.name = mean(rating),
            median.name= median(rating)) 

#QUESTION: how can I count the number of 0 ratings in each injury??

######    ######    ######    ######    stats!!!!!    ######    ######    ######    ######    ######    ######

colnames(cp_45s) # be mindful of cp_q include 6 and 7 year olds, but pc_45s is just 4s and 5s
# rename some variables to shorten
vars = c('startdate','duration','finished','condition','participant number','p_gender',
         'practice1','practice1_rating5','practice2','practice2_rating0','practice3','practice3_rating',
         'should exclude?','should exclude text','experienced pains','Stimulus','rating','ID','stim_sex',
         'stim_age','stim_race','stim_names','injury','age')
colnames(cp_45s) = vars

#participant info means
cp_gender.means <- cp_45s %>%
  group_by(p_gender) %>%
  summarize(n = n(),
            mean.p_gender = mean(rating),
            median.p_gender = median(rating))

## currently don't have participant race
#cp_race.means <- cp_q %>%
#  group_by(p_race) %>%
#  summarize(n = n(),
#            mean.p_race = mean(rating),
#            median.p_race = median(rating))
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#####
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ stats ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#####
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#####
#model of main effects, CHARACTER: injury + character sex/race/age (4 and 5 y/os mixed)
lm1 <- lmer(rating ~ injury + stim_sex + stim_race + stim_age + (1|ID), data=cp_45s)
summary(lm1)
Anova(lm1, Type=3) 

##model of main effects, PARTICIPANT: injury + participant sex (eventually race / age too)
lm2 <- lmer(rating ~ injury + p_gender + (1|ID), data=cp_q)
summary(lm2)
Anova(lm2, Type=3)

#interact p gender + character sex
lm3 <- lmer(rating ~ injury + stim_sex * p_gender + stim_race + stim_age + (1|ID), data=cp_45s)
summary(lm3)
Anova(lm3, Type=3) #nope

#other interactions: stim race/age (signif. in adults)
lm4 <- lmer(rating ~ injury + stim_age * stim_race + stim_sex + (1|ID), data=cp_q)
summary(lm4)
Anova(lm4, Type=3)

#interact stim sex/age (signif. in adults)
lm5 <- lmer(rating ~ injury + stim_age * stim_sex + stim_race + (1|ID), data=cp_q)
summary(lm5)
Anova(lm5, Type=3)

#nothing significant statistically,rn ... (and with 4 and 5 y/os still mixed)
##must add child age (4 vs 5, specifically... unsure how yet.. hard to do include bc condition has 4/5 too)

#experimenting...can I see number of ratings of 1-7 for each injury?
cp_q %>% count(rating, wt=NULL, sort=TRUE, name = NULL)
#wow I did it!
# rating -- n
#   7 -- 115
#   4 -- 74
#   0 -- 64
#   1 -- 60
#   3 -- 54
#   6 -- 49
#   5 -- 46
#   2 -- 34

# can I do it with injury too in there?
inj_freq_ratings <- cp_q %>% count(rating, injury, wt=NULL, sort=TRUE, name = NULL)



# so most common rating is 7, then 4, then 0.... the most, the mid, and the least.
unique(cp_q$ID)

