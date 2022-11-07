#load packages
library(readxl)
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)
library(gitcreds)

d <- read.csv("16 not 8, adult_September 23, 2022_12.53.csv")
colnames(d)

d_ratings <- d[-c(1:2),c(1,6,7,21,26, 27, 29,30:300)] #keep the rows and columns we want
#change this to keep participant country/age/gender/race?

colnames(d_ratings)

#pivot into longer format
p <- d_ratings %>% pivot_longer(cols=c(10:276),values_to = "rating", names_to = "Stimulus",values_drop_na = F)
#This will still have many NA values in ratings, for the questions not seen in that condition
#Don't remove NAs before the next step, otherwise you lose the trials following "no" answer

#Check for "no" responses and add a zero for the next response (in p)
for(i in 1:nrow(p)){
  if(!is.na(p$rating[i])){ #check if rating[i] is NA
      if(p$rating[i]=="no"){  #if so, check if it is "no"
        p$rating[i+1] <- "0"    #if so, replace the next rating with "0"
      }
    }
}
colnames(p)

#Keep only the numerical responses, which correspond to Questions with underscores in them
q <- p %>% filter(grepl("_",Stimulus)) #grepl checks for matches to the string

#make the ratings numeric (they were characters)
q$rating <- as.numeric(q$rating)

#Give unique IDs (presumably, but prolific ID is weird: "copied", email, NA maybe???)
q$ID <- as.numeric(factor(q$`Qc..prolific.ID`))


#Check who responds with non-zero to the control questions (exclusion step)
k <- unique(
  c(
       q[which(q$Stimulus=="Qc1.5_1"& q$rating!=0),]$ID, #check for non-zero for each row, then get those IDs
       q[which(q$Stimulus=="Qc2.5_1"& q$rating!=0),]$ID,
       q[which(q$Stimulus=="Qc3.5_1"& q$rating!=0),]$ID,
       q[which(q$Stimulus=="Qc4.5_1"& q$rating!=0),]$ID
  )
)

drop_index <- is.element(q$ID,k) #illegal observations!
q <- q[!drop_index,] #Keep only the legal ones, ! means opposite or "not"

q <- q %>% filter(!grepl("c",Stimulus))

#check row by row for NA in rating column, and keep only non-NAs... huge step! yay no useless NAs now.
q <- q[!is.na(q$rating),] 

colnames(d)
#Find the questions and their text so we can match stim demos to them #q's are:
    #in this find corresponding rating/question text, to help add in right character demos
      #stimulus = question ratings 0-7, text = question text content 'hi im rachel' etc.
d_text <- d[c(1),c(32:73,75:298)] %>% 
  pivot_longer(cols=1:266,values_to = "text", 
               names_to = "Stimulus",values_drop_na = F)

#Put the text and questions together more conveniently
#for(i in ____) means iterate the i= to be whatever you tell it in ____
#seq(2,nrow(d_text),2) gives sequence of integers, the ,2 means every other one
#it starts at 2, goes to nrow(d_text) which equals 266 in this case
for(i in seq(2,nrow(d_text),2)){
  d_text[i,2] <- d_text[i-1,2]
  print(i)  #check the iteration is working for the loop (should be every even row)
}

#Keep only questions with underscores
d_text <- d_text %>% filter(grepl("_",Stimulus))

#make the demos... and in the correct order of the associated questions, as selected out above^
stim_names <- c("Rachel", "Sydney", "David", "Kyle", "Marissa", "Jessica", "Chris", "Tyler",
           "Dominic","Jacob","Kendall","Sarah","Wyatt","Robert","Tara","Charlotte")

stim_race <- c("White","White","White","White","Black","Black","Black","Black",
               "Black","Black","Black","Black","White","White","White","White")

#rep creates a vector with whatever is given repeated however many times
stim_age <- c(rep(c("adult","child"),4),rep(c("child","adult"),4))

stim_sex <- c(rep(c(rep(c("female"),2),rep(c("male"),2)),2),rep(c(rep(c("male"),2),rep(c("female"),2)),2))

#Put together all the stimulus properties into one dataframe
stim_properties <- cbind(stim_names,stim_race,stim_age,stim_sex)

#Setup the new columns in the data frame as empty to be filled in
q$stim_names <- q$stim_race <- q$stim_age <- q$stim_sex <- NA

#Make a loop iterating each i
#There are 16 names, so it goes through 16 times
for(i in 1:length(stim_names)){
  #now implement that loop
  #Find the questions with each name
  #e.g. if i=1, find the questions with name stim_names[1] ("Rachel")
  stim_match <- d_text[grep(stim_names[i],d_text$text),1]$Stimulus
  
  #Which rows of the data frame (q) are those trials
  keep_index <- is.element(q$Stimulus,stim_match)
  
  #Add the stimulus demos to the new columns for those rows identified
  #e.g. i=1 ("Rachel" trials) then add "Rachel" "white" "adult" "female"
  q[keep_index,]$stim_names <-stim_properties[i,1]
  q[keep_index,]$stim_race  <-stim_properties[i,2]
  q[keep_index,]$stim_age   <-stim_properties[i,3]
  q[keep_index,]$stim_sex   <-stim_properties[i,4]
}

#Now add the injuries to the data frame 
# injury list = what it will put it in as, injury phrases = what it's finding in the questions actual words to attach 'those'injurylist' labels to

InjuryList <- c("burned tongue","broken arm","stomach ache","paper cut",
  "skinned knees","bruised leg","splinter","bee sting")
InjuryPhrases <- c("burned", "broke", "stomach", "cut", "skinned", "bruised",
                   "splinter","stung")

q$injury <- NA #make column everything can go into 

#Goes through each injury type and adds a column with that injury
#the key words in the strings are in InjuryPhrases
#the factor names to use are in InjuryList
for(i in 1:length(InjuryPhrases)){
  #now implement that loop
  #Find the questions with each injury keyword
  #e.g. if i=1, find the questions with injury phrase "burned"
  stim_match <- d_text[grep(InjuryPhrases[i],d_text$text),1]$Stimulus
  
  #Which rows of the data frame (q) are those trials
  #TRUE/FALSE for keep/not
  keep_index <- is.element(q$Stimulus,stim_match)
  
  #Add the injury to the new columns for those rows identified by keep_index
  #e.g. i=1 ("burned tongue" trials) then add "burned tongue" 
  q[keep_index,]$injury <- InjuryList[i]
}

#will now do exclusions:  broken arm 0's  AND THEN too short time

ok <- unique(
  c(
    q[which(q$Stimulus=="Q2.5_1"& q$rating==0),]$ID,# 1F, WCF
    q[which(q$Stimulus=="Q66.5_1"& q$rating==0),]$ID,# 1F, BAM
    q[which(q$Stimulus=="Q49.5_1"& q$rating==0),]$ID,# 7F, WAF
    q[which(q$Stimulus=="Q113.5_1"& q$rating==0),]$ID,# 7F, BCM
    q[which(q$Stimulus=="Q27.5_1"& q$rating==0),]$ID,#4F WAM
    q[which(q$Stimulus=="Q91.5_1"& q$rating==0),]$ID,#4F, BCF
    q[which(q$Stimulus=="Q45.5_1"& q$rating==0),]$ID,#6F, BAF
    q[which(q$Stimulus=="Q109.5_1"& q$rating==0),]$ID,#6F, WCM
    q[which(q$Stimulus=="Q60.5_1"& q$rating==0),]$ID,#8F, WCM
    q[which(q$Stimulus=="Q124.5_1"& q$rating==0),]$ID,#8F, BAF
    q[which(q$Stimulus=="Q22.5_1"& q$rating==0),]$ID,#3F, BCF
    q[which(q$Stimulus=="Q86.5_1"& q$rating==0),]$ID,#3F,WAM
    q[which(q$Stimulus=="Q104.5_1"& q$rating==0),]$ID,#5F,WAF
    q[which(q$Stimulus=="Q40.5_1"& q$rating==0),]$ID,#5F, BCM
    q[which(q$Stimulus=="Q15.5_1"& q$rating==0),]$ID,#2F, BAM
    q[which(q$Stimulus=="Q79.5_1"& q$rating==0),]$ID#2F, WCF
  )
)

drop_index <- is.element(q$ID,ok) #remove illegal observations aka broken arm 0s

q <- q[!drop_index,] #Keep only the legal ones, ! means opposite or "not"

ok
#wow it worked! looks like it dropped 6 people :-)

#add exclusion for any 'finished' that is 'False'
f <- unique(
  c(
    q[which(q$Finished=="False"),]$ID
  )
)

drop_index <- is.element(q$ID,f)
q <- q[!drop_index,]
f
#dropped 5 people. incomplete data exclusion therefore done!
unique(q$Finished)

#pt 1 time exclude: under 6 minutes, 
#duration stored in seconds = under 360 seconds

tk <- unique(
  c(
    q[which(q$Duration..in.seconds.<=360),]$ID #check for non-zero for each row, then get those IDs
  )
)

drop_index <- is.element(q$ID,tk)
q <- q[!drop_index,] #Keep only the legal ones, ! means opposite or "not"
tk
#dropped 135 people for too fast?? wow!
#check duration
unique(q$Duration..in.seconds.)

unique(q$ID)
#277? So actually not even 300 valid?

#pt 2 time exclude: over 40 minutes = 2400 plus seconds
#now to drop people OVER 40 minutes... 2400 seconds... but think its not working? 
tko <- unique(
   c(
     q[which(q$Duration..in.seconds.>=2400),]$ID #check for non-zero for each row, then get those IDs
   )
 )

drop_index <- is.element(q$ID,tko)
drop_index
tko

#checking how many people there are right now?
unique(q$duration)

unique(q$ID)
# 417 people still left... how many went over 40 min I wonder?
# I think none... when I run tko it works and I still have 417 unique IDs.


#gonna see if I can do some analyses, without participant info yet
#very general/too general
d.means<- q %>%
  group_by(injury, stim_race, stim_sex, stim_age) %>%
  summarize(n=n(),   # n's are uneven; less than they used to be but is it still an issue?
            mean.rating  = mean(rating), 
            sd.rating = sd(rating))

injury.means <- q %>%
  group_by(injury) %>%
  summarize(n = n(),
            mean.inj = mean(rating),
            median.inj = median(rating)) 

# just stim sex
stim.sex.means <- q %>% 
  group_by(stim_sex) %>%
  summarize(n = n(),
            mean.sex = mean(rating),
            median.sex = median(rating)) #female ~0.1 higher avg

#just stim race... but not yet POC/white coded
stim.race.means <- q %>%
  group_by(stim_race) %>%
  summarize(n = n(),
            mean.race = mean(rating),
            median.race = median(rating)) #no diff race

#just stim age
stim.age.means <- q %>%
  group_by(stim_age) %>%
  summarize(n = n(),
            mean.age = mean(rating),
            median.age = median(rating)) #kids ~0.6 higher avg


#specific characters/'names'
name.means <- q %>%
  group_by(stim_names) %>%
  summarize(n = n(),
            mean.name = mean(rating),
            median.name= median(rating)) #age is clear separation. 
                                          #race diff adults m/f vs kids m/f.

#real analyses: characters only, no participant stuff
lm1 <- lmer(rating ~ stim_sex + stim_race + stim_age + (1|ID) + (1|injury), data=q)
summary(lm1)
Anova(lm1, Type=3) #great! I had done my analyses right before. sex and age significant main effects. no race.

#####interactions of character demos: ######
# char sex/race interaction? no
lm1.0 <- lmer(rating ~ (1|injury) + stim_sex * p_gender +stim_race+ stim_age + (1|ID), data=q)
summary(lm1.0)
Anova(lm1.0, Type=3)

# char sex/age interaction? yes
lm1.1 <- lmer(rating ~ stim_sex * stim_age + stim_race + (1|ID) + (1|injury), data=q)
summary(lm1.1)
Anova(lm1.1, Type=3) #significant, p=0.01

# char race/age interaction? yes
lm1.2 <- lmer(rating ~ stim_race * stim_age + stim_sex + (1|ID) + (1|injury), data=q)
summary(lm1.2)
Anova(lm1.2, Type=3) #significant, p=0.01 again

#recode column name QD5 to p_race
  #make participant race white/asian/black/hispanic, and or white vs POC?


colnames(q)
# rename some variables to shorten
vars = c('startdate','duration','finished','prolific.ID','p_age','p_gender','p_race','p_education','p_parent','mturkcode','CONDITION','Stimulus','rating','ID','stim_sex','stim_age','stim_race','stim_names','injury')
colnames(q) = vars 

unique(q$p_race) # before 1s and 0s
#results mostly 'white','hispanic_latino','black_africanamerican','asian' then some mixes/prefer not to say

#participant info means
p_gender.means <- q %>%
  group_by(p_gender) %>%
  summarize(n = n(),
            mean.p_gender = mean(rating),
            median.p_gender = median(rating))

p_race.means <- q %>%
  group_by(p_race) %>%
  summarize(n = n(),
            mean.p_race = mean(rating),
            median.p_race = median(rating))

#q <- q %>%
 # mutate(p_race = as.factor(p_race))%>%
   #   mutate(p_race = ifelse(p_race =='white', 1,0))
#that worked!
#so participant race is 1 for white, 0 for anything else
         
unique(q$p_race)
unique(q$p_age) #all over. 18 to 64.

#real analyses: participant stuff
lm2  <- lmer(rating ~ + p_gender + p_race + p_age + (1|ID) + (1|injury), data=q)
summary(lm2)
Anova(lm2, Type=3) #all significant (ignore age though...make it into younger and older groups?)

#interactions of participant gender/race?

lm3 <- lmer(rating ~ + p_gender * p_race + p_age + (1|ID)+ (1|injury), data=q)
summary(lm3)
Anova(lm3, Type=3) #no interaction

unique(q$ID)

#attention check select B/ select C check. nobody answered these wrong.
unique(d$Q129)
unique(d$Q130)
#character/participant interaction?.... wait why do I have p age race gender here already? 

####  all data cleaning to do  ####
# add in: character demos (check!)
# add in: injuries (check!)
# exclude: nonzero control question respondents (check!)
# exclude: broken arm 0's respondents (check!)
# exclude: too-short/too long duration participants (check!)
# had participant demographics (were already in! no re-add) 
# then can analyze :-) )