# clear global environment
rm(list = ls()) 
# clear console
cat("\f") 
# clear plots
dev.off()

# load libraries
library("psych")
library("plyr")
library('dplyr')
library("reshape2")
library("EcoSimR")
library("openxlsx")
library("tidyr")
library("tidyverse")
library("arsenal")
library("knitr")
library("gplots")
library("ggplot2")
library("car")
library("afex")
library("emmeans")
library("Rmisc")
library("svglite")
library("cowplot")
library("readr")
library("sf")
library("mapview")
library("ggmap")
library("COVID19")
library("pryr")
library("cowplot")
library("readr")
library("ggstatsplot")
library("hrbrthemes")
library("dunn.test")
library("rcompanion")
library("corrplot")
library("tcltk2")

############################## Analysis of Survey Data to Analyze Effect of COVID19 on Physical Activity ##############################

# This script was developed to import, clean, and analyze survey data on physical activity during the COVID19 pandemic
# The data file associated with the script is COVID19_Survey_Data

############################################### Recoding and Variable Naming ########################################################## 

# select CSV file 
COVID19 <- read.csv(file.choose()) # current file name is: "COVID19_Survey_Data"
attach(COVID19)
head(COVID19)

#Remove gender = 3 from dataframe
which(COVID19$X14 == 3) # leave only male and female options (1 and 2)
COVID19 <- COVID19[-c(950, 1313), ] # remove two rows with "3" as gender
which(COVID19$X14 == 3) # should say integer(0) if all non-male and female options have been removed

#Recode Sex Group & Make Factor
COVID19$X14[COVID19$X14==1] <- "Male"
COVID19$X14[COVID19$X14==2] <- "Female"
COVID19$X14 <- as.factor(COVID19$X14)

#Recode sex by essential status
COVID19$X18[COVID19$X18==1] <- "Male (Essential)"
COVID19$X18[COVID19$X18==2] <- "Male (Non-Essential)"
COVID19$X18[COVID19$X18==3] <- "Female (Essential)"
COVID19$X18[COVID19$X18==4] <- "Female (Non-Essential)"

#Recode Kids at Home, Group & Make Factor
COVID19$X27[COVID19$X27==1] <- "Yes"
COVID19$X27[COVID19$X27==2] <- "No"
COVID19$X27 <- as.factor(COVID19$X27)

#Create new column with only employment status pre-COVID
COVID19$X229[COVID19$X34==1] <- "Employed"
COVID19$X229[COVID19$X34==2] <- "Employed"
COVID19$X229[COVID19$X34==3] <- "Employed"
COVID19$X229[COVID19$X34==4] <- "Employed"
COVID19$X229[COVID19$X34==5] <- "Unemployed"
COVID19$X229[COVID19$X34==6] <- "Unemployed"
COVID19$X229[COVID19$X34==7] <- "Unemployed"
COVID19$X229[COVID19$X34==8] <- "Unemployed"
COVID19$X229 <- as.factor(COVID19$X229)

#Recode preCOVID Employment Group & Make Factor
COVID19$X34[COVID19$X34==1] <- "Employed full-time"
COVID19$X34[COVID19$X34==2] <- "Employed full-time but furloughed or laid off"
COVID19$X34[COVID19$X34==3] <- "Employed part-time"
COVID19$X34[COVID19$X34==4] <- "Self-employed"
COVID19$X34[COVID19$X34==5] <- "Full-time student"
COVID19$X34[COVID19$X34==6] <- "Part-time student"
COVID19$X34[COVID19$X34==7] <- "Unemployed"
COVID19$X34[COVID19$X34==8] <- "Retired"
COVID19$X34 <- as.factor(COVID19$X34)

#Create new column with only employed or unemployed for post
COVID19$X230[COVID19$X35==1] <- "Employed"
COVID19$X230[COVID19$X35==2] <- "Unemployed"
COVID19$X230[COVID19$X35==3] <- "Employed"
COVID19$X230[COVID19$X35==4] <- "Employed"
COVID19$X230[COVID19$X35==5] <- "Unemployed"
COVID19$X230[COVID19$X35==6] <- "Unemployed"
COVID19$X230[COVID19$X35==7] <- "Unemployed"
COVID19$X230[COVID19$X35==8] <- "Unemployed"
COVID19$X230 <- as.factor(COVID19$X230)

#Recode post COVID Employment Group & Make Factor
COVID19$X35[COVID19$X35==1] <- "Employed full-time"
COVID19$X35[COVID19$X35==2] <- "Employed full-time but furloughed or laid off" # This counts as unemployed
COVID19$X35[COVID19$X35==3] <- "Employed part-time"
COVID19$X35[COVID19$X35==4] <- "Self-employed"
COVID19$X35[COVID19$X35==5] <- "Full-time student"
COVID19$X35[COVID19$X35==6] <- "Part-time student"
COVID19$X35[COVID19$X35==7] <- "Unemployed"
COVID19$X35[COVID19$X35==8] <- "Retired"
COVID19$X35 <- as.factor(COVID19$X35)

#Recode essential Worker  Group & Make Factor
COVID19$X36[COVID19$X36==1] <- "Yes"
COVID19$X36[COVID19$X36==2] <- "No"
COVID19$X36 <- as.factor(COVID19$X36)

#Recode COVID Infection Group & Make Factor
COVID19$X44[COVID19$X44==1] <- "Yes"
COVID19$X44[COVID19$X44==2] <- "No"
COVID19$X44 <- as.factor(COVID19$X44)

#Recode COVID Infection Group & Make Factor
# (1= self, 2= spouse/partner, 3= child, 4= relative, 5= friend, 6= roommate, 7= colleague)
### not currently working, some cells have multiple numbers ex. 4,5,6  
# -- Dr. Cortes - check below. A: I know, lets leave it for now.

COVID19$X45[COVID19$X45==1] <- "Self"
COVID19$X45[COVID19$X45==2] <- "Spouse/Partner"
COVID19$X45[COVID19$X45==3] <- "Child"
COVID19$X45[COVID19$X45==4] <- "Relative"
COVID19$X45[COVID19$X45==5] <- "Friend"
COVID19$X45[COVID19$X45==6] <- "Roommate"
COVID19$X45[COVID19$X45==7] <- "Colleague"
COVID19$X45 <- as.factor(COVID19$X45)

#Rename column names
names(COVID19)[names(COVID19) == "X13"] <- "Age"
names(COVID19)[names(COVID19) == "X14"] <- "Gender"
names(COVID19)[names(COVID19) == "X18"] <- "SexbyEssentialStatus"
names(COVID19)[names(COVID19) == "X19"] <- "EssentialStatusNoSex"
names(COVID19)[names(COVID19) == "X24"] <- "CurrentLiving"
names(COVID19)[names(COVID19) == "X27"] <- "Kids.home"
names(COVID19)[names(COVID19) == "X34"] <- "PreEmployment"
names(COVID19)[names(COVID19) == "X35"] <- "PostEmployment"
names(COVID19)[names(COVID19) == "X36"] <- "EssentialWorker"
names(COVID19)[names(COVID19) == "X40"] <- "TypeOfLivingArea"
names(COVID19)[names(COVID19) == "X44"] <- "COVIDInfection"
names(COVID19)[names(COVID19) == "X45"] <- "COVIDInfWho"
names(COVID19)[names(COVID19) == "X53"] <- "TraitPhysicalEnergy"
names(COVID19)[names(COVID19) == "X54"] <- "TraitPhysicalFatigue"
names(COVID19)[names(COVID19) == "X61"] <- "TraitMentalEnergy"
names(COVID19)[names(COVID19) == "X62"] <- "TraitMentalFatigue"
names(COVID19)[names(COVID19) == "X75"] <- "Grit"
# Activity was recoded also to cateorical, 106, 110, 115, 119
names(COVID19)[names(COVID19) == "X105"] <- "VigorousActivity"
names(COVID19)[names(COVID19) == "X109"] <- "ModerateActivity"
names(COVID19)[names(COVID19) == "X110"] <- "CategoryPhysicalActivtiy"
names(COVID19)[names(COVID19) == "X114"] <- "LightActivity"
names(COVID19)[names(COVID19) == "X118"] <- "Sitting"
names(COVID19)[names(COVID19) == "X119"] <- "SittingCategorical"
# All food was recoded but not sure why or how, the recode column is +1 of below listing
names(COVID19)[names(COVID19) == "X120"] <- "SkipBreakfast"
names(COVID19)[names(COVID19) == "X122"] <- "TakeOut"
names(COVID19)[names(COVID19) == "X124"] <- "WholeGrain"
names(COVID19)[names(COVID19) == "X126"] <- "Fruit"
names(COVID19)[names(COVID19) == "X128"] <- "Vegetables"
names(COVID19)[names(COVID19) == "X130"] <- "Dairy"
names(COVID19)[names(COVID19) == "X132"] <- "Meat"
names(COVID19)[names(COVID19) == "X134"] <- "ProcessedMeats"
names(COVID19)[names(COVID19) == "X136"] <- "Fried"
names(COVID19)[names(COVID19) == "X138"] <- "Snacks"
names(COVID19)[names(COVID19) == "X140"] <- "Butter"
names(COVID19)[names(COVID19) == "X142"] <- "Sweets"
names(COVID19)[names(COVID19) == "X144"] <- "Soda"
names(COVID19)[names(COVID19) == "X149"] <- "REAP_S"
names(COVID19)[names(COVID19) == "X158"] <- "IntensityMentalWork_Work"
names(COVID19)[names(COVID19) == "X159"] <- "IntensityMentalWork_NonWork"
names(COVID19)[names(COVID19) == "X160"] <- "HoursPerWeek"
names(COVID19)[names(COVID19) == "X191"] <- "POMSAnxiety"
names(COVID19)[names(COVID19) == "X192"] <- "POMSDepression"
names(COVID19)[names(COVID19) == "X193"] <- "POMSAnger"
names(COVID19)[names(COVID19) == "X194"] <- "POMSFatigue"
names(COVID19)[names(COVID19) == "X195"] <- "POMSConfusion"
# There is also confusion without efficiency 196 
names(COVID19)[names(COVID19) == "X197"] <- "POMSVigor"
names(COVID19)[names(COVID19) == "X198"] <- "POMSTotalMoodDisturbance"
names(COVID19)[names(COVID19) == "X199"] <- "POMSTotalMoodDisturbance_New"
names(COVID19)[names(COVID19) == "X202"] <- "PhysicalVigor"
names(COVID19)[names(COVID19) == "X203"] <- "PhysicalExhaustion"
names(COVID19)[names(COVID19) == "X204"] <- "PhysicalPep"
names(COVID19)[names(COVID19) == "X205"] <- "PhysicalWornOut"
names(COVID19)[names(COVID19) == "X206"] <- "PhysicalEnergy"
names(COVID19)[names(COVID19) == "X207"] <- "PhysicalFatigue"
names(COVID19)[names(COVID19) == "X210"] <- "MentalVigor"
names(COVID19)[names(COVID19) == "X211"] <- "MentalExhaustion"
names(COVID19)[names(COVID19) == "X212"] <- "MentalPep"
names(COVID19)[names(COVID19) == "X213"] <- "MentalWornOut"
names(COVID19)[names(COVID19) == "X214"] <- "MentalEnergy"
names(COVID19)[names(COVID19) == "X215"] <- "MentalFatigue"
names(COVID19)[names(COVID19) == "X216"] <- "MotivationMental"
names(COVID19)[names(COVID19) == "X217"] <- "MotivationPhysical"
names(COVID19)[names(COVID19) == "X219"] <- "PSQI2"
names(COVID19)[names(COVID19) == "X220"] <- "PSQI3"
names(COVID19)[names(COVID19) == "X224"] <- "PSQI5"
names(COVID19)[names(COVID19) == "X225"] <- "PSQI6"
names(COVID19)[names(COVID19) == "X227"] <- "PSQI7"
names(COVID19)[names(COVID19) == "X229"] <- "PreEmploymentStatus"
names(COVID19)[names(COVID19) == "X230"] <- "PostEmploymentStatus"
names(COVID19)[names(COVID19) == "X87"] <- "HoursOfSleep"

##################################################### filtering dataframe by employment status and country ###########################################

# make new dataframe with just post-COVID employed individuals

# we can do this by dropping rows which contain 'FALSE' in X18 and X19 as they contain sex by employment status information. FALSE = non=employed
COVID19_Employed = COVID19[!(COVID19$SexbyEssentialStatus =="FALSE" & COVID19$EssentialStatusNoSex == "FALSE"),]
COVID19_Employed = filter(COVID19_Employed, X38 == "187") # filters to just US employed 

#################################################### set variables as numeric #######################################################################

COVID19_Employed$POMSTotalMoodDisturbance_New<-as.numeric(as.character(COVID19_Employed$POMSTotalMoodDisturbance_New))
COVID19_Employed$VigorousActivity<-as.numeric(as.character(COVID19_Employed$VigorousActivity))
COVID19_Employed$ModerateActivity<-as.numeric(as.character(COVID19_Employed$ModerateActivity))
COVID19_Employed$LightActivity<-as.numeric(as.character(COVID19_Employed$LightActivity))
COVID19_Employed$HoursPerWeek<-as.numeric(as.character(COVID19_Employed$HoursPerWeek))
COVID19_Employed$Sitting<-as.numeric(as.character(COVID19_Employed$Sitting))
COVID19_Employed$HoursPerWeek<-as.numeric(as.character(COVID19_Employed$HoursPerWeek))
COVID19_Employed$Grit<-as.numeric(as.character(COVID19_Employed$Grit))
COVID19_Employed$IntensityMentalWork_Work<-as.numeric(as.character(COVID19_Employed$IntensityMentalWork_Work))
COVID19_Employed$Kids.home<-as.numeric(as.character(COVID19_Employed$Kids.home))
COVID19_Employed$HoursOfSleep<-as.numeric(as.character(COVID19_Employed$HoursOfSleep))
COVID19_Employed$SittingCategorical<-as.numeric(as.character(COVID19_Employed$SittingCategorical))
COVID19_Employed$PhysicalEnergy<-as.numeric(as.character(COVID19_Employed$PhysicalEnergy))
COVID19_Employed$PhysicalFatigue<-as.numeric(as.character(COVID19_Employed$PhysicalFatigue))
COVID19_Employed$MentalEnergy<-as.numeric(as.character(COVID19_Employed$MentalEnergy))
COVID19_Employed$MentalFatigue<-as.numeric(as.character(COVID19_Employed$MentalFatigue))

#################################################### create MVPA variable #############################################################

COVID19_Employed$MVPA <- COVID19_Employed$VigorousActivity + COVID19_Employed$ModerateActivity
COVID19_Employed$MVPA<-as.numeric(as.character(COVID19_Employed$MVPA))

################################################### Descriptive Statistics ###################################################################

# QoL data by essential/non-essential and gender

# I have replaced having two factors (essential status and gender) with the 'SexbyEssentialStatus' factor
# 1 = male, essential
# 2 = male, non-essential
# 3 = female, essential
# 4 = female, non-essential

##### Grit Descriptives ##### 

Gritdesc <- ddply(COVID19_Employed, .(SexbyEssentialStatus), summarize,  
             N    = sum(!is.na(Grit)),
             length(Grit), 
             mean = mean(Grit, na.rm="TRUE"), 
             sd = sd (Grit, na.rm="TRUE"), 
             median = median(Grit, na.rm="TRUE"),
             error = qnorm(0.975)*sd/sqrt(N),
             LowerCI = mean - error,
             UpperCI = mean + error,
             kurtosi = kurtosi(Grit, na.rm="TRUE"),
             skew = skew(Grit, na.rm="TRUE"),
             se   = sd / sqrt(N))
Gritdesc

##### MVPA Descriptives #####

MVPAdesc <- ddply(COVID19_Employed, .(SexbyEssentialStatus), summarize,  
                  N    = sum(!is.na(MVPA)),
                  length(MVPA), 
                  mean = mean(MVPA, na.rm="TRUE"), 
                  sd = sd (MVPA, na.rm="TRUE"), 
                  median = median(MVPA, na.rm="TRUE"),
                  error = qnorm(0.975)*sd/sqrt(N),
                  LowerCI = mean - error,
                  UpperCI = mean + error,
                  kurtosi = kurtosi(MVPA, na.rm="TRUE"),
                  skew = skew(MVPA, na.rm="TRUE"),
                  se   = sd / sqrt(N))
MVPAdesc

##### Vigorous Activity Descriptives #####

VigorousActivitydesc <- ddply(COVID19_Employed, .(SexbyEssentialStatus), summarize,  
                  N    = sum(!is.na(VigorousActivity)),
                  length(VigorousActivity), 
                  mean = mean(VigorousActivity, na.rm="TRUE"), 
                  sd = sd (VigorousActivity, na.rm="TRUE"), 
                  median = median(VigorousActivity, na.rm="TRUE"),
                  error = qnorm(0.975)*sd/sqrt(N),
                  LowerCI = mean - error,
                  UpperCI = mean + error,
                  kurtosi = kurtosi(VigorousActivity, na.rm="TRUE"),
                  skew = skew(VigorousActivity, na.rm="TRUE"),
                  se   = sd / sqrt(N))
VigorousActivitydesc

##### Moderate Activity Descriptives #####

ModerateActivitydesc <- ddply(COVID19_Employed, .(SexbyEssentialStatus), summarize,  
                              N    = sum(!is.na(ModerateActivity)),
                              length(ModerateActivity), 
                              mean = mean(ModerateActivity, na.rm="TRUE"), 
                              sd = sd (ModerateActivity, na.rm="TRUE"), 
                              median = median(ModerateActivity, na.rm="TRUE"),
                              error = qnorm(0.975)*sd/sqrt(N),
                              LowerCI = mean - error,
                              UpperCI = mean + error,
                              kurtosi = kurtosi(ModerateActivity, na.rm="TRUE"),
                              skew = skew(ModerateActivity, na.rm="TRUE"),
                              se   = sd / sqrt(N))
ModerateActivitydesc

##### Light Activity Descriptives #####

LightActivitydesc <- ddply(COVID19_Employed, .(SexbyEssentialStatus), summarize,  
                              N    = sum(!is.na(LightActivity)),
                              length(LightActivity), 
                              mean = mean(LightActivity, na.rm="TRUE"), 
                              sd = sd (LightActivity, na.rm="TRUE"), 
                              median = median(LightActivity, na.rm="TRUE"),
                              error = qnorm(0.975)*sd/sqrt(N),
                              LowerCI = mean - error,
                              UpperCI = mean + error,
                              kurtosi = kurtosi(LightActivity, na.rm="TRUE"),
                              skew = skew(LightActivity, na.rm="TRUE"),
                              se   = sd / sqrt(N))
LightActivitydesc

##### Sitting Descriptives #####

Sittingdesc <- ddply(COVID19_Employed, .(SexbyEssentialStatus), summarize,  
                           N    = sum(!is.na(Sitting)),
                           length(Sitting), 
                           mean = mean(Sitting, na.rm="TRUE"), 
                           sd = sd (Sitting, na.rm="TRUE"), 
                           median = median(Sitting, na.rm="TRUE"),
                           error = qnorm(0.975)*sd/sqrt(N),
                           LowerCI = mean - error,
                           UpperCI = mean + error,
                           kurtosi = kurtosi(Sitting, na.rm="TRUE"),
                           skew = skew(Sitting, na.rm="TRUE"),
                           se   = sd / sqrt(N))
Sittingdesc

##### Sitting categorical Descriptives #####

SittingCategoricaldesc <- ddply(COVID19_Employed, .(SexbyEssentialStatus), summarize,  
                     N    = sum(!is.na(SittingCategorical)),
                     length(SittingCategorical), 
                     mean = mean(SittingCategorical, na.rm="TRUE"), 
                     sd = sd (SittingCategorical, na.rm="TRUE"), 
                     median = median(SittingCategorical, na.rm="TRUE"),
                     error = qnorm(0.975)*sd/sqrt(N),
                     LowerCI = mean - error,
                     UpperCI = mean + error,
                     kurtosi = kurtosi(SittingCategorical, na.rm="TRUE"),
                     skew = skew(SittingCategorical, na.rm="TRUE"),
                     se   = sd / sqrt(N))
SittingCategoricaldesc

##### HoursOfSleep Descriptives #####

Sleepdesc <- ddply(COVID19_Employed, .(SexbyEssentialStatus), summarize,  
                    N    = sum(!is.na(HoursOfSleep)),
                    length(HoursOfSleep), 
                    mean = mean(HoursOfSleep, na.rm="TRUE"), 
                    sd = sd (HoursOfSleep, na.rm="TRUE"), 
                    median = median(HoursOfSleep, na.rm="TRUE"),
                    error = qnorm(0.975)*sd/sqrt(N),
                    LowerCI = mean - error,
                    UpperCI = mean + error,
                    kurtosi = kurtosi(HoursOfSleep, na.rm="TRUE"),
                    skew = skew(HoursOfSleep, na.rm="TRUE"),
                    se   = sd / sqrt(N))
Sleepdesc

##### Hours Worked per Week Descriptives #####

HoursPerWeekdesc <- ddply(COVID19_Employed, .(SexbyEssentialStatus), summarize,  
                          N    = sum(!is.na(HoursPerWeek)),
                          length(HoursPerWeek), 
                          mean = mean(HoursPerWeek, na.rm="TRUE"), 
                          sd = sd (HoursPerWeek, na.rm="TRUE"), 
                          median = median(HoursPerWeek, na.rm="TRUE"),
                          error = qnorm(0.975)*sd/sqrt(N),
                          LowerCI = mean - error,
                          UpperCI = mean + error,
                          kurtosi = kurtosi(HoursPerWeek, na.rm="TRUE"),
                          skew = skew(HoursPerWeek, na.rm="TRUE"),
                          se   = sd / sqrt(N))
HoursPerWeekdesc

##### POMS Total Mood Disturbance Descriptives #####

POMSTotalMoodDisturbancedesc <- ddply(COVID19_Employed, .(SexbyEssentialStatus), summarize,  
                   N    = sum(!is.na(POMSTotalMoodDisturbance_New)),
                   length(POMSTotalMoodDisturbance_New), 
                   mean = mean(POMSTotalMoodDisturbance_New, na.rm="TRUE"), 
                   sd = sd (POMSTotalMoodDisturbance_New, na.rm="TRUE"), 
                   median = median(POMSTotalMoodDisturbance_New, na.rm="TRUE"),
                   error = qnorm(0.975)*sd/sqrt(N),
                   LowerCI = mean - error,
                   UpperCI = mean + error,
                   kurtosi = kurtosi(POMSTotalMoodDisturbance_New, na.rm="TRUE"),
                   skew = skew(POMSTotalMoodDisturbance_New, na.rm="TRUE"),
                   se   = sd / sqrt(N))
POMSTotalMoodDisturbancedesc

#####  Physical Energy Descriptives #####

PhysicalEnergydesc <- ddply(COVID19_Employed, .(SexbyEssentialStatus), summarize,  
                   N    = sum(!is.na(PhysicalEnergy)),
                   length(PhysicalEnergy), 
                   mean = mean(PhysicalEnergy, na.rm="TRUE"), 
                   sd = sd (PhysicalEnergy, na.rm="TRUE"), 
                   median = median(PhysicalEnergy, na.rm="TRUE"),
                   error = qnorm(0.975)*sd/sqrt(N),
                   LowerCI = mean - error,
                   UpperCI = mean + error,
                   kurtosi = kurtosi(PhysicalEnergy, na.rm="TRUE"),
                   skew = skew(PhysicalEnergy, na.rm="TRUE"),
                   se   = sd / sqrt(N))
PhysicalEnergydesc

#####  Physical Fatigue Descriptives #####

PhysicalFatiguedesc <- ddply(COVID19_Employed, .(SexbyEssentialStatus), summarize,  
                   N    = sum(!is.na(PhysicalFatigue)),
                   length(PhysicalFatigue), 
                   mean = mean(PhysicalFatigue, na.rm="TRUE"), 
                   sd = sd (PhysicalFatigue, na.rm="TRUE"), 
                   median = median(PhysicalFatigue, na.rm="TRUE"),
                   error = qnorm(0.975)*sd/sqrt(N),
                   LowerCI = mean - error,
                   UpperCI = mean + error,
                   kurtosi = kurtosi(PhysicalFatigue, na.rm="TRUE"),
                   skew = skew(PhysicalFatigue, na.rm="TRUE"),
                   se   = sd / sqrt(N))
PhysicalFatiguedesc

#####  Mental Energy Descriptives #####

MentalEnergydesc <- ddply(COVID19_Employed, .(SexbyEssentialStatus), summarize,  
                   N    = sum(!is.na(MentalEnergy)),
                   length(MentalEnergy), 
                   mean = mean(MentalEnergy, na.rm="TRUE"), 
                   sd = sd (MentalEnergy, na.rm="TRUE"), 
                   median = median(MentalEnergy, na.rm="TRUE"),
                   error = qnorm(0.975)*sd/sqrt(N),
                   LowerCI = mean - error,
                   UpperCI = mean + error,
                   kurtosi = kurtosi(MentalEnergy, na.rm="TRUE"),
                   skew = skew(MentalEnergy, na.rm="TRUE"),
                   se   = sd / sqrt(N))
MentalEnergydesc

#####  Mental Fatigue Descriptives #####

MentalFatiguedesc <- ddply(COVID19_Employed, .(SexbyEssentialStatus), summarize,  
                   N    = sum(!is.na(MentalFatigue)),
                   length(MentalFatigue), 
                   mean = mean(MentalFatigue, na.rm="TRUE"), 
                   sd = sd (MentalFatigue, na.rm="TRUE"), 
                   median = median(MentalFatigue, na.rm="TRUE"),
                   error = qnorm(0.975)*sd/sqrt(N),
                   LowerCI = mean - error,
                   UpperCI = mean + error,
                   kurtosi = kurtosi(MentalFatigue, na.rm="TRUE"),
                   skew = skew(MentalFatigue, na.rm="TRUE"),
                   se   = sd / sqrt(N))
MentalFatiguedesc

##### Mental Motivation Descriptives #####

COVID19_Employed$MotivationMental<-as.numeric(as.character(COVID19_Employed$MotivationMental))

MotivationMentaldesc <- ddply(COVID19_Employed, .(SexbyEssentialStatus), summarize,  
                   N    = sum(!is.na(MotivationMental)),
                   length(MotivationMental), 
                   mean = mean(MotivationMental, na.rm="TRUE"), 
                   sd = sd (MotivationMental, na.rm="TRUE"), 
                   median = median(MotivationMental, na.rm="TRUE"),
                   error = qnorm(0.975)*sd/sqrt(N),
                   LowerCI = mean - error,
                   UpperCI = mean + error,
                   kurtosi = kurtosi(MotivationMental, na.rm="TRUE"),
                   skew = skew(MotivationMental, na.rm="TRUE"),
                   se   = sd / sqrt(N))
MotivationMentaldesc

##### Physical Motivation Descriptives #####

COVID19_Employed$MotivationPhysical<-as.numeric(as.character(COVID19_Employed$MotivationPhysical))

MotivationPhysicaldesc <- ddply(COVID19_Employed, .(SexbyEssentialStatus), summarize,  
                   N    = sum(!is.na(MotivationPhysical)),
                   length(MotivationPhysical), 
                   mean = mean(MotivationPhysical, na.rm="TRUE"), 
                   sd = sd (MotivationPhysical, na.rm="TRUE"), 
                   median = median(MotivationPhysical, na.rm="TRUE"),
                   error = qnorm(0.975)*sd/sqrt(N),
                   LowerCI = mean - error,
                   UpperCI = mean + error,
                   kurtosi = kurtosi(MotivationPhysical, na.rm="TRUE"),
                   skew = skew(MotivationPhysical, na.rm="TRUE"),
                   se   = sd / sqrt(N))
MotivationPhysicaldesc

#################################################### export descriptives to excel #########################################################

library("xlsx")
write.xlsx(Gritdesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="Grit", append = FALSE)
write.xlsx(VigorousActivitydesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="VigorousActivity", append = TRUE)
write.xlsx(ModerateActivitydesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="ModerateActivity", append = TRUE)
write.xlsx(LightActivitydesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="LightActivity", append = TRUE)
write.xlsx(Sittingdesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="Sitting", append = TRUE)
write.xlsx(PSQI2desc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="PSQI2", append = TRUE)
write.xlsx(PSQI3desc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="PSQI3", append = TRUE)
write.xlsx(PSQI5desc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="PSQI5", append = TRUE)
write.xlsx(PSQI6desc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="PSQI6", append = TRUE)
write.xlsx(PSQI7desc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="PSQI7", append = TRUE)
write.xlsx(Reap_Sdesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="REAP_S", append = TRUE)
write.xlsx(POMSAnxietydesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="POMSAnxiety", append = TRUE)
write.xlsx(POMSDepressiondesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="POMSDepression", append = TRUE)
write.xlsx(POMSAngerdesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="POMSAnger", append = TRUE)
write.xlsx(POMSFatiguedesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="POMSFatigue", append = TRUE)
write.xlsx(POMSConfusiondesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="POMSConfusion", append = TRUE)
write.xlsx(POMSVigordesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="POMSVigor", append = TRUE)
write.xlsx(POMSTotalMoodDisturbancedesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="POMSTotalMoodDisturbance", append = TRUE)
write.xlsx(PhysicalEnergydesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="PhysicalEnergy", append = TRUE)
write.xlsx(PhysicalFatiguedesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="PhysicalFatigue", append = TRUE)
write.xlsx(MentalEnergydesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="MentalEnergy", append = TRUE)
write.xlsx(MentalFatiguedesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="MentalFatigue", append = TRUE)
write.xlsx(MotivationMentaldesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="MotivationMental", append = TRUE)
write.xlsx(MotivationPhysicaldesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="MotivationPhysical", append = TRUE)
write.xlsx(HoursPerWeekdesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="HoursPerWeek", append = TRUE)
write.xlsx(TraitPhysicalEnergydesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="TraitPhysicalEnergy", append = TRUE)
write.xlsx(TraitPhysicalFatiguedesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="TraitPhysicalFatigue", append = TRUE)
write.xlsx(TraitMentalEnergydesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="TraitMentalEnergy", append = TRUE)
write.xlsx(TraitMentalFatiguedesc, file="/Users/ncortes/Desktop/COVID19descriptives.xlsx", sheetName="TraitMentalFatigue", append = TRUE)


#################################################################### Grit Normality #################################################################

# Grit Histogram

Grithistogram %<a-% # %>% is a 'pipe' operator. This operator will forward a value, or the result of an expression, into the next function call/expression
  {par(mfrow=c(2,2) ,oma = c(0, 0, 2, 0))
    # 1st row
    hist(COVID19_Employed$Grit[COVID19_Employed$SexbyEssentialStatus == "Male (Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Male (Essential)")

    
    # 1st row
    hist(COVID19_Employed$Grit[COVID19_Employed$SexbyEssentialStatus == "Male (Non-Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         #ylab = NULL,
         main = "Male (Non-Essential)")
    
    # 2nd row
    hist(COVID19_Employed$Grit[COVID19_Employed$SexbyEssentialStatus == "Female (Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Female (Essential")

    # 2nd row
    hist(COVID19_Employed$Grit[COVID19_Employed$SexbyEssentialStatus == "Female (Non-Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Female (Non-Essential")
    
    mtext("Grit", outer = TRUE, cex = 2)}

Grithistogram

# Grit boxplots

Gritboxplots %<a-%
  {par(mfrow=c(1,1))
  boxplot(Grit~SexbyEssentialStatus, COVID19_Employed, 
           main="Grit", xlab = "Sex by Essential Status", ylab = "Grit")
    mtext("Grit", outer = TRUE, cex = 2)}
Gritboxplots

# Grit QQ-plot

Gritqqplot %<a-%
  {par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
    qqPlot(COVID19_Employed$Grit[COVID19_Employed$SexbyEssentialStatus== "Male (Essential)"], main = "Male (Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$Grit[COVID19_Employed$SexbyEssentialStatus== "Male (Non-Essential)"], main = "Male (Non-Essential) ", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$Grit[COVID19_Employed$SexbyEssentialStatus== "Female (Essential)"], main = "Female (Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$Grit[COVID19_Employed$SexbyEssentialStatus== "Female (Non-Essential)"], main = "Female (Non-Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    mtext("Grit", outer = TRUE, cex = 2)}

Gritqqplot

# Grit rainplot

source("R_rainclouds.R")

Gritrain <- ggplot(COVID19_Employed,aes(x=SexbyEssentialStatus,y=Grit, fill = SexbyEssentialStatus, colour = SexbyEssentialStatus))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_boxplot(aes(x = as.numeric(SexbyEssentialStatus)+0.25, y = Grit),outlier.shape = NA, alpha = 0.1, width = .1, colour = "BLACK") +
  ylab('Grit')+xlab('Essential Status by Sex')+theme_classic()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_grey()+
  ggtitle("Grit")

Gritrain

# Grit interaction plot

# we can't use 'SexbyEssentialStatus' here as we need two variables to show interaction

mean.rm.na <- function(x) mean(x,na.rm=T)

par(mfrow=c(1,1))

interaction.plot(x.factor     = COVID19_Employed$EssentialWorker,
                 trace.factor = COVID19_Employed$Gender,
                 response     = COVID19_Employed$Grit,
                 fun = mean.rm.na,
                 type="b",
                 col=c("black","red","green"),  
                 pch=c(19, 17, 15),             
                 fixed=TRUE,
                 xlab = "Essential",
                 ylab = "Grit",
                 leg.bty = "o")

# Grit violin plot with ANOVA

gritANOVA <- ggbetweenstats(
  data = COVID19_Employed,
  x = SexbyEssentialStatus,
  y = Grit,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.annotation = "p.value",
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  effsize.type = "biased",
  partial = TRUE,
  effsize.noncentral = TRUE,
  bf.prior = 0.707,
  bf.message = TRUE,
  sphericity.correction = TRUE,
  results.subtitle = TRUE,
  xlab = "Essential Worker by Sex",
  ylab = "Grit",
  caption = "",
  title = "",
  subtitle = "",
  sample.size.label = TRUE,
  k = 2,
  conf.level = 0.95,
  nboot = 100,
  tr = 0.1,
  mean.plotting = TRUE,
  mean.ci = TRUE,
  mean.point.args = list(size = 5, color = "darkred"),
  mean.label.args = list(size = 3),
  point.path = TRUE,
  point.path.args = list(alpha = 0.5, linetype = "dashed"),
  mean.path = TRUE,
  mean.path.args = list(color = "red", size = 1, alpha = 0.5),
  notch = TRUE,
  notchwidth = 0.5,
  outlier.tagging = FALSE,
  outlier.label = NULL,
  outlier.coef = 1.5,
  outlier.label.args = list(),
  outlier.point.args = list(),
  violin.args = list(width = 0.5, alpha = 0.2),
  ggtheme = ggthemes::theme_tufte(),
  ggstatsplot.layer = TRUE,
  package = "RColorBrewer",
  palette = "Dark2",
  direction = 1,
  ggplot.component = NULL,
  output = "plot",
  messages = TRUE
)
gritANOVA

#################################################################### MVPA Normality #################################################################

# MVPA Histogram

MVPAhistogram %<a-% # %>% is a 'pipe' operator. This operator will forward a value, or the result of an expression, into the next function call/expression
  {par(mfrow=c(2,2) ,oma = c(0, 0, 2, 0))
    # 1st row
    hist(COVID19_Employed$MVPA[COVID19_Employed$SexbyEssentialStatus == "Male (Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Male (Essential)")
    
    
    # 1st row
    hist(COVID19_Employed$MVPA[COVID19_Employed$SexbyEssentialStatus == "Male (Non-Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         #ylab = NULL,
         main = "Male (Non-Essential)")
    
    # 2nd row
    hist(COVID19_Employed$MVPA[COVID19_Employed$SexbyEssentialStatus == "Female (Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Female (Essential")
    
    # 2nd row
    hist(COVID19_Employed$MVPA[COVID19_Employed$SexbyEssentialStatus == "Female (Non-Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Female (Non-Essential")
    
    mtext("MVPA", outer = TRUE, cex = 2)}

MVPAhistogram

# MVPA boxplots

MVPAboxplots %<a-%
  {par(mfrow=c(1,1))
    boxplot(MVPA~SexbyEssentialStatus, COVID19_Employed, 
            main="MVPA", xlab = "Sex by Essential Status", ylab = "MVPA")
    mtext("MVPA", outer = TRUE, cex = 2)}
MVPAboxplots

# MVPA QQ-plot

MVPAqqplot %<a-%
  {par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
    qqPlot(COVID19_Employed$MVPA[COVID19_Employed$SexbyEssentialStatus== "Male (Essential)"], main = "Male (Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$MVPA[COVID19_Employed$SexbyEssentialStatus== "Male (Non-Essential)"], main = "Male (Non-Essential) ", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$MVPA[COVID19_Employed$SexbyEssentialStatus== "Female (Essential)"], main = "Female (Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$MVPA[COVID19_Employed$SexbyEssentialStatus== "Female (Non-Essential)"], main = "Female (Non-Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    mtext("MVPA", outer = TRUE, cex = 2)}

MVPAqqplot

# MVPA rainplot

source("R_rainclouds.R")

MVPArain <- ggplot(COVID19_Employed,aes(x=SexbyEssentialStatus,y=MVPA, fill = SexbyEssentialStatus, colour = SexbyEssentialStatus))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_boxplot(aes(x = as.numeric(SexbyEssentialStatus)+0.25, y = MVPA),outlier.shape = NA, alpha = 0.1, width = .1, colour = "BLACK") +
  ylab('MVPA')+xlab('Essential Status by Sex')+theme_classic()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_grey()+
  ggtitle("MVPA")

MVPArain

# MVPA interaction plot

# we can't use 'SexbyEssentialStatus' here as we need two variables to show interaction

mean.rm.na <- function(x) mean(x,na.rm=T)

par(mfrow=c(1,1))

interaction.plot(x.factor     = COVID19_Employed$EssentialWorker,
                 trace.factor = COVID19_Employed$Gender,
                 response     = COVID19_Employed$MVPA,
                 fun = mean.rm.na,
                 type="b",
                 col=c("black","red","green"),  
                 pch=c(19, 17, 15),             
                 fixed=TRUE,
                 xlab = "Essential",
                 ylab = "MVPA",
                 leg.bty = "o")

# MVPA violin plot with ANOVA

MVPAANOVA <- ggbetweenstats(
  data = COVID19_Employed,
  x = SexbyEssentialStatus,
  y = MVPA,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.annotation = "p.value",
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  effsize.type = "biased",
  partial = TRUE,
  effsize.noncentral = TRUE,
  bf.prior = 0.707,
  bf.message = TRUE,
  sphericity.correction = TRUE,
  results.subtitle = TRUE,
  xlab = "Essential Worker by Sex",
  ylab = "MVPA",
  caption = "",
  title = "",
  subtitle = "",
  sample.size.label = TRUE,
  k = 2,
  conf.level = 0.95,
  nboot = 100,
  tr = 0.1,
  mean.plotting = TRUE,
  mean.ci = TRUE,
  mean.point.args = list(size = 5, color = "darkred"),
  mean.label.args = list(size = 3),
  point.path = TRUE,
  point.path.args = list(alpha = 0.5, linetype = "dashed"),
  mean.path = TRUE,
  mean.path.args = list(color = "red", size = 1, alpha = 0.5),
  notch = TRUE,
  notchwidth = 0.5,
  outlier.tagging = FALSE,
  outlier.label = NULL,
  outlier.coef = 1.5,
  outlier.label.args = list(),
  outlier.point.args = list(),
  violin.args = list(width = 0.5, alpha = 0.2),
  ggtheme = ggthemes::theme_tufte(),
  ggstatsplot.layer = TRUE,
  package = "RColorBrewer",
  palette = "Dark2",
  direction = 1,
  ggplot.component = NULL,
  output = "plot",
  messages = TRUE
)
MVPAANOVA


################################################################ vigorous activity normality #########################################################

# vigorous histogram

VigorousActivityhistogram %<a-% # %>% is a 'pipe' operator. This operator will forward a value, or the result of an expression, into the next function call/expression
  {par(mfrow=c(2,2) ,oma = c(0, 0, 2, 0))
    # 1st row
    hist(COVID19_Employed$VigorousActivity[COVID19_Employed$SexbyEssentialStatus == "Male (Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Male (Essential)")
    
    
    # 1st row
    hist(COVID19_Employed$VigorousActivity[COVID19_Employed$SexbyEssentialStatus == "Male (Non-Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         #ylab = NULL,
         main = "Male (Non-Essential)")
    
    # 2nd row
    hist(COVID19_Employed$VigorousActivity[COVID19_Employed$SexbyEssentialStatus == "Female (Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Female (Essential")
    
    # 2nd row
    hist(COVID19_Employed$VigorousActivity[COVID19_Employed$SexbyEssentialStatus == "Female (Non-Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Female (Non-Essential")
    
    mtext("VigorousActivity", outer = TRUE, cex = 2)}

VigorousActivityhistogram

# vigorous boxplots

VigorousActivityboxplots %<a-%
  {par(mfrow=c(1,1))
    boxplot(VigorousActivity~SexbyEssentialStatus, COVID19_Employed, 
            main="VigorousActivity", xlab = "Sex by Essential Status", ylab = "VigorousActivity")
    mtext("VigorousActivity", outer = TRUE, cex = 2)}
VigorousActivityboxplots

# vigorous QQ plots

VigorousActivityqqplot %<a-%
  {par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
    qqPlot(COVID19_Employed$VigorousActivity[COVID19_Employed$SexbyEssentialStatus== "Male (Essential)"], main = "Male (Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$VigorousActivity[COVID19_Employed$SexbyEssentialStatus== "Male (Non-Essential)"], main = "Male (Non-Essential) ", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$VigorousActivity[COVID19_Employed$SexbyEssentialStatus== "Female (Essential)"], main = "Female (Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$VigorousActivity[COVID19_Employed$SexbyEssentialStatus== "Female (Non-Essential)"], main = "Female (Non-Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    mtext("VigorousActivity", outer = TRUE, cex = 2)}

VigorousActivityqqplot

# vigorous raincloud plot

source("R_rainclouds.R")

VigorousActivityrain <- ggplot(COVID19_Employed,aes(x=SexbyEssentialStatus,y=VigorousActivity, fill = SexbyEssentialStatus, colour = SexbyEssentialStatus))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_boxplot(aes(x = as.numeric(SexbyEssentialStatus)+0.25, y = VigorousActivity),outlier.shape = NA, alpha = 0.1, width = .1, colour = "BLACK") +
  ylab('VigorousActivity')+xlab('Essential Status by Sex')+theme_classic()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_grey()+
  ggtitle("VigorousActivity")

VigorousActivityrain

# vigorous interaction plot

# we can't use 'SexbyEssentialStatus' here as we need two variables to show interaction

mean.rm.na <- function(x) mean(x,na.rm=T)

par(mfrow=c(1,1))

interaction.plot(x.factor     = COVID19_Employed$EssentialWorker,
                 trace.factor = COVID19_Employed$Gender,
                 response     = COVID19_Employed$VigorousActivity,
                 fun = mean.rm.na,
                 type="b",
                 col=c("black","red","green"),  
                 pch=c(19, 17, 15),             
                 fixed=TRUE,
                 xlab = "Essential",
                 ylab = "VigorousActivity",
                 leg.bty = "o")

# vigorous violin plot

VigorousActivityANOVA <- ggbetweenstats(
  data = COVID19_Employed,
  x = SexbyEssentialStatus,
  y = VigorousActivity,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.annotation = "p.value",
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  effsize.type = "biased",
  partial = TRUE,
  effsize.noncentral = TRUE,
  bf.prior = 0.707,
  bf.message = TRUE,
  sphericity.correction = TRUE,
  results.subtitle = TRUE,
  xlab = "Essential Worker by Sex",
  ylab = "VigorousActivity",
  caption = "",
  title = "",
  subtitle = "",
  sample.size.label = TRUE,
  k = 2,
  conf.level = 0.95,
  nboot = 100,
  tr = 0.1,
  mean.plotting = TRUE,
  mean.ci = TRUE,
  mean.point.args = list(size = 5, color = "darkred"),
  mean.label.args = list(size = 3),
  point.path = TRUE,
  point.path.args = list(alpha = 0.5, linetype = "dashed"),
  mean.path = TRUE,
  mean.path.args = list(color = "red", size = 1, alpha = 0.5),
  notch = TRUE,
  notchwidth = 0.5,
  outlier.tagging = FALSE,
  outlier.label = NULL,
  outlier.coef = 1.5,
  outlier.label.args = list(),
  outlier.point.args = list(),
  violin.args = list(width = 0.5, alpha = 0.2),
  ggtheme = ggthemes::theme_tufte(),
  ggstatsplot.layer = TRUE,
  package = "RColorBrewer",
  palette = "Dark2",
  direction = 1,
  ggplot.component = NULL,
  output = "plot",
  messages = TRUE
)
VigorousActivityANOVA

################################################################ Moderate activity normality #########################################################

# Moderate histogram

ModerateActivityhistogram %<a-% # %>% is a 'pipe' operator. This operator will forward a value, or the result of an expression, into the next function call/expression
  {par(mfrow=c(2,2) ,oma = c(0, 0, 2, 0))
    # 1st row
    hist(COVID19_Employed$ModerateActivity[COVID19_Employed$SexbyEssentialStatus == "Male (Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Male (Essential)")
    
    
    # 1st row
    hist(COVID19_Employed$ModerateActivity[COVID19_Employed$SexbyEssentialStatus == "Male (Non-Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         #ylab = NULL,
         main = "Male (Non-Essential)")
    
    # 2nd row
    hist(COVID19_Employed$ModerateActivity[COVID19_Employed$SexbyEssentialStatus == "Female (Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Female (Essential")
    
    # 2nd row
    hist(COVID19_Employed$ModerateActivity[COVID19_Employed$SexbyEssentialStatus == "Female (Non-Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Female (Non-Essential")
    
    mtext("ModerateActivity", outer = TRUE, cex = 2)}

ModerateActivityhistogram

# Moderate boxplots

ModerateActivityboxplots %<a-%
  {par(mfrow=c(1,1))
    boxplot(ModerateActivity~SexbyEssentialStatus, COVID19_Employed, 
            main="ModerateActivity", xlab = "Sex by Essential Status", ylab = "ModerateActivity")
    mtext("ModerateActivity", outer = TRUE, cex = 2)}
ModerateActivityboxplots

# Moderate QQ plots

ModerateActivityqqplot %<a-%
  {par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
    qqPlot(COVID19_Employed$ModerateActivity[COVID19_Employed$SexbyEssentialStatus== "Male (Essential)"], main = "Male (Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$ModerateActivity[COVID19_Employed$SexbyEssentialStatus== "Male (Non-Essential)"], main = "Male (Non-Essential) ", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$ModerateActivity[COVID19_Employed$SexbyEssentialStatus== "Female (Essential)"], main = "Female (Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$ModerateActivity[COVID19_Employed$SexbyEssentialStatus== "Female (Non-Essential)"], main = "Female (Non-Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    mtext("ModerateActivity", outer = TRUE, cex = 2)}

ModerateActivityqqplot

# Moderate raincloud plot

source("R_rainclouds.R")

ModerateActivityrain <- ggplot(COVID19_Employed,aes(x=SexbyEssentialStatus,y=ModerateActivity, fill = SexbyEssentialStatus, colour = SexbyEssentialStatus))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_boxplot(aes(x = as.numeric(SexbyEssentialStatus)+0.25, y = ModerateActivity),outlier.shape = NA, alpha = 0.1, width = .1, colour = "BLACK") +
  ylab('ModerateActivity')+xlab('Essential Status by Sex')+theme_classic()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_grey()+
  ggtitle("ModerateActivity")

ModerateActivityrain

# Moderate interaction plot

# we can't use 'SexbyEssentialStatus' here as we need two variables to show interaction

mean.rm.na <- function(x) mean(x,na.rm=T)

par(mfrow=c(1,1))

interaction.plot(x.factor     = COVID19_Employed$EssentialWorker,
                 trace.factor = COVID19_Employed$Gender,
                 response     = COVID19_Employed$ModerateActivity,
                 fun = mean.rm.na,
                 type="b",
                 col=c("black","red","green"),  
                 pch=c(19, 17, 15),             
                 fixed=TRUE,
                 xlab = "Essential",
                 ylab = "ModerateActivity",
                 leg.bty = "o")

# Moderate violin plot

ModerateActivityANOVA <- ggbetweenstats(
  data = COVID19_Employed,
  x = SexbyEssentialStatus,
  y = ModerateActivity,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.annotation = "p.value",
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  effsize.type = "biased",
  partial = TRUE,
  effsize.noncentral = TRUE,
  bf.prior = 0.707,
  bf.message = TRUE,
  sphericity.correction = TRUE,
  results.subtitle = TRUE,
  xlab = "Essential Worker by Sex",
  ylab = "ModerateActivity",
  caption = "",
  title = "",
  subtitle = "",
  sample.size.label = TRUE,
  k = 2,
  conf.level = 0.95,
  nboot = 100,
  tr = 0.1,
  mean.plotting = TRUE,
  mean.ci = TRUE,
  mean.point.args = list(size = 5, color = "darkred"),
  mean.label.args = list(size = 3),
  point.path = TRUE,
  point.path.args = list(alpha = 0.5, linetype = "dashed"),
  mean.path = TRUE,
  mean.path.args = list(color = "red", size = 1, alpha = 0.5),
  notch = TRUE,
  notchwidth = 0.5,
  outlier.tagging = FALSE,
  outlier.label = NULL,
  outlier.coef = 1.5,
  outlier.label.args = list(),
  outlier.point.args = list(),
  violin.args = list(width = 0.5, alpha = 0.2),
  ggtheme = ggthemes::theme_tufte(),
  ggstatsplot.layer = TRUE,
  package = "RColorBrewer",
  palette = "Dark2",
  direction = 1,
  ggplot.component = NULL,
  output = "plot",
  messages = TRUE
)
ModerateActivityANOVA

################################################################ Light activity normality #########################################################

# Light histogram

LightActivityhistogram %<a-% # %>% is a 'pipe' operator. This operator will forward a value, or the result of an expression, into the next function call/expression
  {par(mfrow=c(2,2) ,oma = c(0, 0, 2, 0))
    # 1st row
    hist(COVID19_Employed$LightActivity[COVID19_Employed$SexbyEssentialStatus == "Male (Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Male (Essential)")
    
    
    # 1st row
    hist(COVID19_Employed$LightActivity[COVID19_Employed$SexbyEssentialStatus == "Male (Non-Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         #ylab = NULL,
         main = "Male (Non-Essential)")
    
    # 2nd row
    hist(COVID19_Employed$LightActivity[COVID19_Employed$SexbyEssentialStatus == "Female (Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Female (Essential")
    
    # 2nd row
    hist(COVID19_Employed$LightActivity[COVID19_Employed$SexbyEssentialStatus == "Female (Non-Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Female (Non-Essential")
    
    mtext("LightActivity", outer = TRUE, cex = 2)}

LightActivityhistogram

# Light boxplots

LightActivityboxplots %<a-%
  {par(mfrow=c(1,1))
    boxplot(LightActivity~SexbyEssentialStatus, COVID19_Employed, 
            main="LightActivity", xlab = "Sex by Essential Status", ylab = "LightActivity")
    mtext("LightActivity", outer = TRUE, cex = 2)}
LightActivityboxplots

# Light QQ plots

LightActivityqqplot %<a-%
  {par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
    qqPlot(COVID19_Employed$LightActivity[COVID19_Employed$SexbyEssentialStatus== "Male (Essential)"], main = "Male (Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$LightActivity[COVID19_Employed$SexbyEssentialStatus== "Male (Non-Essential)"], main = "Male (Non-Essential) ", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$LightActivity[COVID19_Employed$SexbyEssentialStatus== "Female (Essential)"], main = "Female (Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$LightActivity[COVID19_Employed$SexbyEssentialStatus== "Female (Non-Essential)"], main = "Female (Non-Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    mtext("LightActivity", outer = TRUE, cex = 2)}

LightActivityqqplot

# Light raincloud plot

source("R_rainclouds.R")

LightActivityrain <- ggplot(COVID19_Employed,aes(x=SexbyEssentialStatus,y=LightActivity, fill = SexbyEssentialStatus, colour = SexbyEssentialStatus))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_boxplot(aes(x = as.numeric(SexbyEssentialStatus)+0.25, y = LightActivity),outlier.shape = NA, alpha = 0.1, width = .1, colour = "BLACK") +
  ylab('LightActivity')+xlab('Essential Status by Sex')+theme_classic()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_grey()+
  ggtitle("LightActivity")

LightActivityrain

# Light interaction plot

# we can't use 'SexbyEssentialStatus' here as we need two variables to show interaction

mean.rm.na <- function(x) mean(x,na.rm=T)

par(mfrow=c(1,1))

interaction.plot(x.factor     = COVID19_Employed$EssentialWorker,
                 trace.factor = COVID19_Employed$Gender,
                 response     = COVID19_Employed$LightActivity,
                 fun = mean.rm.na,
                 type="b",
                 col=c("black","red","green"),  
                 pch=c(19, 17, 15),             
                 fixed=TRUE,
                 xlab = "Essential",
                 ylab = "LightActivity",
                 leg.bty = "o")

# Light violin plot

LightActivityANOVA <- ggbetweenstats(
  data = COVID19_Employed,
  x = SexbyEssentialStatus,
  y = LightActivity,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.annotation = "p.value",
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  effsize.type = "biased",
  partial = TRUE,
  effsize.noncentral = TRUE,
  bf.prior = 0.707,
  bf.message = TRUE,
  sphericity.correction = TRUE,
  results.subtitle = TRUE,
  xlab = "Essential Worker by Sex",
  ylab = "LightActivity",
  caption = "",
  title = "",
  subtitle = "",
  sample.size.label = TRUE,
  k = 2,
  conf.level = 0.95,
  nboot = 100,
  tr = 0.1,
  mean.plotting = TRUE,
  mean.ci = TRUE,
  mean.point.args = list(size = 5, color = "darkred"),
  mean.label.args = list(size = 3),
  point.path = TRUE,
  point.path.args = list(alpha = 0.5, linetype = "dashed"),
  mean.path = TRUE,
  mean.path.args = list(color = "red", size = 1, alpha = 0.5),
  notch = TRUE,
  notchwidth = 0.5,
  outlier.tagging = FALSE,
  outlier.label = NULL,
  outlier.coef = 1.5,
  outlier.label.args = list(),
  outlier.point.args = list(),
  violin.args = list(width = 0.5, alpha = 0.2),
  ggtheme = ggthemes::theme_tufte(),
  ggstatsplot.layer = TRUE,
  package = "RColorBrewer",
  palette = "Dark2",
  direction = 1,
  ggplot.component = NULL,
  output = "plot",
  messages = TRUE
)
LightActivityANOVA

################################################################ Sitting normality #########################################################

# histogram

Sittinghistogram %<a-% # %>% is a 'pipe' operator. This operator will forward a value, or the result of an expression, into the next function call/expression
  {par(mfrow=c(2,2) ,oma = c(0, 0, 2, 0))
    # 1st row
    hist(COVID19_Employed$Sitting[COVID19_Employed$SexbyEssentialStatus == "Male (Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Male (Essential)")
    
    
    # 1st row
    hist(COVID19_Employed$Sitting[COVID19_Employed$SexbyEssentialStatus == "Male (Non-Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         #ylab = NULL,
         main = "Male (Non-Essential)")
    
    # 2nd row
    hist(COVID19_Employed$Sitting[COVID19_Employed$SexbyEssentialStatus == "Female (Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Female (Essential")
    
    # 2nd row
    hist(COVID19_Employed$Sitting[COVID19_Employed$SexbyEssentialStatus == "Female (Non-Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Female (Non-Essential")
    
    mtext("Sitting", outer = TRUE, cex = 2)}

Sittinghistogram

# boxplots

Sittingboxplots %<a-%
  {par(mfrow=c(1,1))
    boxplot(Sitting~SexbyEssentialStatus, COVID19_Employed, 
            main="Sitting", xlab = "Sex by Essential Status", ylab = "Sitting")
    mtext("Sitting", outer = TRUE, cex = 2)}
Sittingboxplots

# QQ plots

Sittingqqplot %<a-%
  {par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
    qqPlot(COVID19_Employed$Sitting[COVID19_Employed$SexbyEssentialStatus== "Male (Essential)"], main = "Male (Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$Sitting[COVID19_Employed$SexbyEssentialStatus== "Male (Non-Essential)"], main = "Male (Non-Essential) ", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$Sitting[COVID19_Employed$SexbyEssentialStatus== "Female (Essential)"], main = "Female (Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$Sitting[COVID19_Employed$SexbyEssentialStatus== "Female (Non-Essential)"], main = "Female (Non-Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    mtext("Sitting", outer = TRUE, cex = 2)}

Sittingqqplot

# raincloud plot

source("R_rainclouds.R")

Sittingrain <- ggplot(COVID19_Employed,aes(x=SexbyEssentialStatus,y=Sitting, fill = SexbyEssentialStatus, colour = SexbyEssentialStatus))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_boxplot(aes(x = as.numeric(SexbyEssentialStatus)+0.25, y = Sitting),outlier.shape = NA, alpha = 0.1, width = .1, colour = "BLACK") +
  ylab('Sitting')+xlab('Essential Status by Sex')+theme_classic()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_grey()+
  ggtitle("Sitting")

Sittingrain

# interaction plot

# we can't use 'SexbyEssentialStatus' here as we need two variables to show interaction

mean.rm.na <- function(x) mean(x,na.rm=T)

par(mfrow=c(1,1))

interaction.plot(x.factor     = COVID19_Employed$EssentialWorker,
                 trace.factor = COVID19_Employed$Gender,
                 response     = COVID19_Employed$Sitting,
                 fun = mean.rm.na,
                 type="b",
                 col=c("black","red","green"),  
                 pch=c(19, 17, 15),             
                 fixed=TRUE,
                 xlab = "Essential",
                 ylab = "Sitting",
                 leg.bty = "o")

# violin plot

SittingANOVA <- ggbetweenstats(
  data = COVID19_Employed,
  x = SexbyEssentialStatus,
  y = Sitting,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.annotation = "p.value",
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  effsize.type = "biased",
  partial = TRUE,
  effsize.noncentral = TRUE,
  bf.prior = 0.707,
  bf.message = TRUE,
  sphericity.correction = TRUE,
  results.subtitle = TRUE,
  xlab = "Essential Worker by Sex",
  ylab = "Sitting",
  caption = "",
  title = "",
  subtitle = "",
  sample.size.label = TRUE,
  k = 2,
  conf.level = 0.95,
  nboot = 100,
  tr = 0.1,
  mean.plotting = TRUE,
  mean.ci = TRUE,
  mean.point.args = list(size = 5, color = "darkred"),
  mean.label.args = list(size = 3),
  point.path = TRUE,
  point.path.args = list(alpha = 0.5, linetype = "dashed"),
  mean.path = TRUE,
  mean.path.args = list(color = "red", size = 1, alpha = 0.5),
  notch = TRUE,
  notchwidth = 0.5,
  outlier.tagging = FALSE,
  outlier.label = NULL,
  outlier.coef = 1.5,
  outlier.label.args = list(),
  outlier.point.args = list(),
  violin.args = list(width = 0.5, alpha = 0.2),
  ggtheme = ggthemes::theme_tufte(),
  ggstatsplot.layer = TRUE,
  package = "RColorBrewer",
  palette = "Dark2",
  direction = 1,
  ggplot.component = NULL,
  output = "plot",
  messages = TRUE
)
SittingANOVA

############################################################# Sitting Categorical Normality ###########################################################

# This will plot gender by essential in a bar plot and output a chi square test statistic. 
# I believe the ** vs *** indicates which one is significantly different in the group, but cant find
# anything to confirm that. The graph is cool to show the breakdown of scores, but I think a kruskal 
# Wallis test followed by dunn tests may be more appropriate so I included them below as well. 


SittingCategoricalchi <- ggbarstats(
  data = COVID19_Employed,
  x = SittingCategorical,
  y = SexbyEssentialStatus,
  sampling.plan = "jointMulti",
  title = "SittingCategorical by Essential Worker and Sex",
  xlab = "Essential Worker by Sex",
  legend.title = "SittingCategorical rating",
  ggtheme = ggplot2::theme_bw(),
  ggplot.component = list(scale_x_discrete(guide = guide_axis(n.dodge = 2))),
  palette = "Set1",
  messages = FALSE
)
SittingCategoricalchi

# This gives same Chi Square result, just have to find post hoc results.
# post hoc results would not even run for this data, kept saying data was not incorrect 
# I.e. was supposed to be non negative and finite or something, I forget but like I said Dunn is prolly best. 
CHItableSittingCategorical <- table(COVID19_Employed$SittingCategorical, COVID19_Employed$SexbyEssentialStatus)
CHItableSittingCategorical
chisq.test(CHItableSittingCategorical,correct=FALSE)

# I believe the more appropriate test is the Kruskal wallis, deals directly with scales (i.e. 1-4 ratings as seen here for SittingCategorical)
SittingCategorical_Krus <- kruskal.test(COVID19_Employed$SittingCategorical, COVID19_Employed$SexbyEssentialStatus)
SittingCategorical_Krus
dunn.test(COVID19_Employed$SittingCategorical, COVID19_Employed$SexbyEssentialStatus, method = "bonferroni")
#If  you would like to add effect size with 95% CI here it is. 
epsilonSquared(COVID19_Employed$SittingCategorical, COVID19_Employed$SexbyEssentialStatus, ci = "TRUE")


################################################################ POMS Total mood disturbance normality #########################################################

# histogram

POMSTotalMoodDisturbancehistogram %<a-% # %>% is a 'pipe' operator. This operator will forward a value, or the result of an expression, into the next function call/expression
  {par(mfrow=c(2,2) ,oma = c(0, 0, 2, 0))
    # 1st row
    hist(COVID19_Employed$POMSTotalMoodDisturbance[COVID19_Employed$SexbyEssentialStatus == "Male (Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Male (Essential)")
    
    
    # 1st row
    hist(COVID19_Employed$POMSTotalMoodDisturbance[COVID19_Employed$SexbyEssentialStatus == "Male (Non-Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         #ylab = NULL,
         main = "Male (Non-Essential)")
    
    # 2nd row
    hist(COVID19_Employed$POMSTotalMoodDisturbance[COVID19_Employed$SexbyEssentialStatus == "Female (Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Female (Essential")
    
    # 2nd row
    hist(COVID19_Employed$POMSTotalMoodDisturbance[COVID19_Employed$SexbyEssentialStatus == "Female (Non-Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Female (Non-Essential")
    
    mtext("POMSTotalMoodDisturbance", outer = TRUE, cex = 2)}

POMSTotalMoodDisturbancehistogram

# boxplots

POMSTotalMoodDisturbanceboxplots %<a-%
  {par(mfrow=c(1,1))
    boxplot(POMSTotalMoodDisturbance~SexbyEssentialStatus, COVID19_Employed, 
            main="POMSTotalMoodDisturbance", xlab = "Sex by Essential Status", ylab = "POMSTotalMoodDisturbance")
    mtext("POMSTotalMoodDisturbance", outer = TRUE, cex = 2)}
POMSTotalMoodDisturbanceboxplots

# QQ plots

POMSTotalMoodDisturbanceqqplot %<a-%
  {par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
    qqPlot(COVID19_Employed$POMSTotalMoodDisturbance[COVID19_Employed$SexbyEssentialStatus== "Male (Essential)"], main = "Male (Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$POMSTotalMoodDisturbance[COVID19_Employed$SexbyEssentialStatus== "Male (Non-Essential)"], main = "Male (Non-Essential) ", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$POMSTotalMoodDisturbance[COVID19_Employed$SexbyEssentialStatus== "Female (Essential)"], main = "Female (Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$POMSTotalMoodDisturbance[COVID19_Employed$SexbyEssentialStatus== "Female (Non-Essential)"], main = "Female (Non-Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    mtext("POMSTotalMoodDisturbance", outer = TRUE, cex = 2)}

POMSTotalMoodDisturbanceqqplot

# raincloud plot

source("R_rainclouds.R")

POMSTotalMoodDisturbancerain <- ggplot(COVID19_Employed,aes(x=SexbyEssentialStatus,y=POMSTotalMoodDisturbance, fill = SexbyEssentialStatus, colour = SexbyEssentialStatus))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_boxplot(aes(x = as.numeric(SexbyEssentialStatus)+0.25, y = POMSTotalMoodDisturbance),outlier.shape = NA, alpha = 0.1, width = .1, colour = "BLACK") +
  ylab('POMSTotalMoodDisturbance')+xlab('Essential Status by Sex')+theme_classic()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_grey()+
  ggtitle("POMSTotalMoodDisturbance")

POMSTotalMoodDisturbancerain

# interaction plot

# we can't use 'SexbyEssentialStatus' here as we need two variables to show interaction

mean.rm.na <- function(x) mean(x,na.rm=T)

par(mfrow=c(1,1))

interaction.plot(x.factor     = COVID19_Employed$EssentialWorker,
                 trace.factor = COVID19_Employed$Gender,
                 response     = COVID19_Employed$POMSTotalMoodDisturbance,
                 fun = mean.rm.na,
                 type="b",
                 col=c("black","red","green"),  
                 pch=c(19, 17, 15),             
                 fixed=TRUE,
                 xlab = "Essential",
                 ylab = "POMSTotalMoodDisturbance",
                 leg.bty = "o")

# violin plot

POMSTotalMoodDisturbanceANOVA <- ggbetweenstats(
  data = COVID19_Employed,
  x = SexbyEssentialStatus,
  y = POMSTotalMoodDisturbance,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.annotation = "p.value",
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  effsize.type = "biased",
  partial = TRUE,
  effsize.noncentral = TRUE,
  bf.prior = 0.707,
  bf.message = TRUE,
  sphericity.correction = TRUE,
  results.subtitle = TRUE,
  xlab = "Essential Worker by Sex",
  ylab = "POMSTotalMoodDisturbance",
  caption = "",
  title = "",
  subtitle = "",
  sample.size.label = TRUE,
  k = 2,
  conf.level = 0.95,
  nboot = 100,
  tr = 0.1,
  mean.plotting = TRUE,
  mean.ci = TRUE,
  mean.point.args = list(size = 5, color = "darkred"),
  mean.label.args = list(size = 3),
  point.path = TRUE,
  point.path.args = list(alpha = 0.5, linetype = "dashed"),
  mean.path = TRUE,
  mean.path.args = list(color = "red", size = 1, alpha = 0.5),
  notch = TRUE,
  notchwidth = 0.5,
  outlier.tagging = FALSE,
  outlier.label = NULL,
  outlier.coef = 1.5,
  outlier.label.args = list(),
  outlier.point.args = list(),
  violin.args = list(width = 0.5, alpha = 0.2),
  ggtheme = ggthemes::theme_tufte(),
  ggstatsplot.layer = TRUE,
  package = "RColorBrewer",
  palette = "Dark2",
  direction = 1,
  ggplot.component = NULL,
  output = "plot",
  messages = TRUE
)
POMSTotalMoodDisturbanceANOVA

################################################################ Hours per week normality #########################################################

# histogram

HoursPerWeekhistogram %<a-% # %>% is a 'pipe' operator. This operator will forward a value, or the result of an expression, into the next function call/expression
  {par(mfrow=c(2,2) ,oma = c(0, 0, 2, 0))
    # 1st row
    hist(COVID19_Employed$HoursPerWeek[COVID19_Employed$SexbyEssentialStatus == "Male (Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Male (Essential)")
    
    
    # 1st row
    hist(COVID19_Employed$HoursPerWeek[COVID19_Employed$SexbyEssentialStatus == "Male (Non-Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         #ylab = NULL,
         main = "Male (Non-Essential)")
    
    # 2nd row
    hist(COVID19_Employed$HoursPerWeek[COVID19_Employed$SexbyEssentialStatus == "Female (Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Female (Essential")
    
    # 2nd row
    hist(COVID19_Employed$HoursPerWeek[COVID19_Employed$SexbyEssentialStatus == "Female (Non-Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Female (Non-Essential")
    
    mtext("HoursPerWeek", outer = TRUE, cex = 2)}

HoursPerWeekhistogram

# boxplots

HoursPerWeekboxplots %<a-%
  {par(mfrow=c(1,1))
    boxplot(HoursPerWeek~SexbyEssentialStatus, COVID19_Employed, 
            main="HoursPerWeek", xlab = "Sex by Essential Status", ylab = "HoursPerWeek")
    mtext("HoursPerWeek", outer = TRUE, cex = 2)}
HoursPerWeekboxplots

# QQ plots

HoursPerWeekqqplot %<a-%
  {par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
    qqPlot(COVID19_Employed$HoursPerWeek[COVID19_Employed$SexbyEssentialStatus== "Male (Essential)"], main = "Male (Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$HoursPerWeek[COVID19_Employed$SexbyEssentialStatus== "Male (Non-Essential)"], main = "Male (Non-Essential) ", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$HoursPerWeek[COVID19_Employed$SexbyEssentialStatus== "Female (Essential)"], main = "Female (Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$HoursPerWeek[COVID19_Employed$SexbyEssentialStatus== "Female (Non-Essential)"], main = "Female (Non-Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    mtext("HoursPerWeek", outer = TRUE, cex = 2)}

HoursPerWeekqqplot

# raincloud plot

source("R_rainclouds.R")

HoursPerWeekrain <- ggplot(COVID19_Employed,aes(x=SexbyEssentialStatus,y=HoursPerWeek, fill = SexbyEssentialStatus, colour = SexbyEssentialStatus))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_boxplot(aes(x = as.numeric(SexbyEssentialStatus)+0.25, y = HoursPerWeek),outlier.shape = NA, alpha = 0.1, width = .1, colour = "BLACK") +
  ylab('HoursPerWeek')+xlab('Essential Status by Sex')+theme_classic()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_grey()+
  ggtitle("HoursPerWeek")

HoursPerWeekrain

# interaction plot

# we can't use 'SexbyEssentialStatus' here as we need two variables to show interaction

mean.rm.na <- function(x) mean(x,na.rm=T)

par(mfrow=c(1,1))

interaction.plot(x.factor     = COVID19_Employed$EssentialWorker,
                 trace.factor = COVID19_Employed$Gender,
                 response     = COVID19_Employed$HoursPerWeek,
                 fun = mean.rm.na,
                 type="b",
                 col=c("black","red","green"),  
                 pch=c(19, 17, 15),             
                 fixed=TRUE,
                 xlab = "Essential",
                 ylab = "HoursPerWeek",
                 leg.bty = "o")

# violin plot

HoursPerWeekANOVA <- ggbetweenstats(
  data = COVID19_Employed,
  x = SexbyEssentialStatus,
  y = HoursPerWeek,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.annotation = "p.value",
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  effsize.type = "biased",
  partial = TRUE,
  effsize.noncentral = TRUE,
  bf.prior = 0.707,
  bf.message = TRUE,
  sphericity.correction = TRUE,
  results.subtitle = TRUE,
  xlab = "Essential Worker by Sex",
  ylab = "HoursPerWeek",
  caption = "",
  title = "",
  subtitle = "",
  sample.size.label = TRUE,
  k = 2,
  conf.level = 0.95,
  nboot = 100,
  tr = 0.1,
  mean.plotting = TRUE,
  mean.ci = TRUE,
  mean.point.args = list(size = 5, color = "darkred"),
  mean.label.args = list(size = 3),
  point.path = TRUE,
  point.path.args = list(alpha = 0.5, linetype = "dashed"),
  mean.path = TRUE,
  mean.path.args = list(color = "red", size = 1, alpha = 0.5),
  notch = TRUE,
  notchwidth = 0.5,
  outlier.tagging = FALSE,
  outlier.label = NULL,
  outlier.coef = 1.5,
  outlier.label.args = list(),
  outlier.point.args = list(),
  violin.args = list(width = 0.5, alpha = 0.2),
  ggtheme = ggthemes::theme_tufte(),
  ggstatsplot.layer = TRUE,
  package = "RColorBrewer",
  palette = "Dark2",
  direction = 1,
  ggplot.component = NULL,
  output = "plot",
  messages = TRUE
)
HoursPerWeekANOVA

################################################################ REAP_S normality #########################################################

# histogram

REAP_Shistogram %<a-% # %>% is a 'pipe' operator. This operator will forward a value, or the result of an expression, into the next function call/expression
  {par(mfrow=c(2,2) ,oma = c(0, 0, 2, 0))
    # 1st row
    hist(COVID19_Employed$REAP_S[COVID19_Employed$SexbyEssentialStatus == "Male (Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Male (Essential)")
    
    
    # 1st row
    hist(COVID19_Employed$REAP_S[COVID19_Employed$SexbyEssentialStatus == "Male (Non-Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         #ylab = NULL,
         main = "Male (Non-Essential)")
    
    # 2nd row
    hist(COVID19_Employed$REAP_S[COVID19_Employed$SexbyEssentialStatus == "Female (Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Female (Essential")
    
    # 2nd row
    hist(COVID19_Employed$REAP_S[COVID19_Employed$SexbyEssentialStatus == "Female (Non-Essential)"], 
         col="lightgray", # column color
         border="black",
         prob = TRUE, # show densities instead of frequencies - logarithmic transformation of frequency
         xlab = NULL,
         ylab = NULL,
         main = "Female (Non-Essential")
    
    mtext("REAP_S", outer = TRUE, cex = 2)}

REAP_Shistogram

# boxplots

REAP_Sboxplots %<a-%
  {par(mfrow=c(1,1))
    boxplot(REAP_S~SexbyEssentialStatus, COVID19_Employed, 
            main="REAP_S", xlab = "Sex by Essential Status", ylab = "REAP_S")
    mtext("REAP_S", outer = TRUE, cex = 2)}
REAP_Sboxplots

# QQ plots

REAP_Sqqplot %<a-%
  {par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
    qqPlot(COVID19_Employed$REAP_S[COVID19_Employed$SexbyEssentialStatus== "Male (Essential)"], main = "Male (Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$REAP_S[COVID19_Employed$SexbyEssentialStatus== "Male (Non-Essential)"], main = "Male (Non-Essential) ", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$REAP_S[COVID19_Employed$SexbyEssentialStatus== "Female (Essential)"], main = "Female (Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    qqPlot(COVID19_Employed$REAP_S[COVID19_Employed$SexbyEssentialStatus== "Female (Non-Essential)"], main = "Female (Non-Essential)", ylab = "", xlab = "", col.lines=carPalette()[1], grid = FALSE)
    mtext("REAP_S", outer = TRUE, cex = 2)}

REAP_Sqqplot

# raincloud plot

source("R_rainclouds.R")

REAP_Srain <- ggplot(COVID19_Employed,aes(x=SexbyEssentialStatus,y=REAP_S, fill = SexbyEssentialStatus, colour = SexbyEssentialStatus))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_boxplot(aes(x = as.numeric(SexbyEssentialStatus)+0.25, y = REAP_S),outlier.shape = NA, alpha = 0.1, width = .1, colour = "BLACK") +
  ylab('REAP_S')+xlab('Essential Status by Sex')+theme_classic()+guides(fill = FALSE, colour = FALSE) +
  scale_fill_grey()+
  ggtitle("REAP_S")

REAP_Srain

# interaction plot

# we can't use 'SexbyEssentialStatus' here as we need two variables to show interaction

mean.rm.na <- function(x) mean(x,na.rm=T)

par(mfrow=c(1,1))

interaction.plot(x.factor     = COVID19_Employed$EssentialWorker,
                 trace.factor = COVID19_Employed$Gender,
                 response     = COVID19_Employed$REAP_S,
                 fun = mean.rm.na,
                 type="b",
                 col=c("black","red","green"),  
                 pch=c(19, 17, 15),             
                 fixed=TRUE,
                 xlab = "Essential",
                 ylab = "REAP_S",
                 leg.bty = "o")

# violin plot

REAP_SANOVA <- ggbetweenstats(
  data = COVID19_Employed,
  x = SexbyEssentialStatus,
  y = REAP_S,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.annotation = "p.value",
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  effsize.type = "biased",
  partial = TRUE,
  effsize.noncentral = TRUE,
  bf.prior = 0.707,
  bf.message = TRUE,
  sphericity.correction = TRUE,
  results.subtitle = TRUE,
  xlab = "Essential Worker by Sex",
  ylab = "REAP_S",
  caption = "",
  title = "",
  subtitle = "",
  sample.size.label = TRUE,
  k = 2,
  conf.level = 0.95,
  nboot = 100,
  tr = 0.1,
  mean.plotting = TRUE,
  mean.ci = TRUE,
  mean.point.args = list(size = 5, color = "darkred"),
  mean.label.args = list(size = 3),
  point.path = TRUE,
  point.path.args = list(alpha = 0.5, linetype = "dashed"),
  mean.path = TRUE,
  mean.path.args = list(color = "red", size = 1, alpha = 0.5),
  notch = TRUE,
  notchwidth = 0.5,
  outlier.tagging = FALSE,
  outlier.label = NULL,
  outlier.coef = 1.5,
  outlier.label.args = list(),
  outlier.point.args = list(),
  violin.args = list(width = 0.5, alpha = 0.2),
  ggtheme = ggthemes::theme_tufte(),
  ggstatsplot.layer = TRUE,
  package = "RColorBrewer",
  palette = "Dark2",
  direction = 1,
  ggplot.component = NULL,
  output = "plot",
  messages = TRUE
)
REAP_SANOVA



#------------------------------------------------------------#
#    Graphs With ANOVAs -- TESTING                           #
#------------------------------------------------------------#
# Based on https://github.com/IndrajeetPatil/ggstatsplot


######################################################### remove outliers ###################################################

# Check for outliers using custom function created to remove outliers based on +- 3SD; called 'RemoveOutl

outlierAbove3sd <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  s1 <- sd(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  #outlier <-(var_name > (m1 + (s1*3)))
  mo <- mean(var_name > (m1 + (s1*3)))
  var_name <- ifelse(var_name > (m1 + (s1*3)), NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}

#outlierAbove3sd(COVID19_Employed, POMSTotalMoodDisturbance_New)
outlierAbove3sd(COVID19_Employed, VigorousActivity)
outlierAbove3sd(COVID19_Employed, ModerateActivity)
outlierAbove3sd(COVID19_Employed, LightActivity)
outlierAbove3sd(COVID19_Employed, Sitting)
#outlierAbove3sd(COVID19_Employed, REAP_S)
outlierAbove3sd(COVID19_Employed, HoursPerWeek)
#outlierAbove3sd(COVID19_Employed, Grit)
outlierAbove3sd(COVID19_Employed, MVPA)
#outlierAbove3sd(COVID19_Employed, HoursOfSleep)
#outlierAbove3sd(COVID19_Employed, TraitPhysicalEnergy)
#outlierAbove3sd(COVID19_Employed, TraitPhysicalFatigue)
#outlierAbove3sd(COVID19_Employed, TraitMentalEnergy)
#outlierAbove3sd(COVID19_Employed, TraitMentalFatigue)

outlierBelow3sd <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  s1 <- sd(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  #outlier <-(var_name > (m1 + (s1*3)))
  mo <- mean(var_name < (m1 - (s1*3)))
  var_name <- ifelse(var_name < (m1 - (s1*3)), NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}

#outlierBelow3sd(COVID19_Employed, POMSTotalMoodDisturbance_New)
outlierBelow3sd(COVID19_Employed, VigorousActivity)
outlierBelow3sd(COVID19_Employed, ModerateActivity)
outlierBelow3sd(COVID19_Employed, LightActivity)
outlierBelow3sd(COVID19_Employed, MVPA)
outlierBelow3sd(COVID19_Employed, Sitting)
#outlierBelow3sd(COVID19_Employed, REAP_S)
outlierBelow3sd(COVID19_Employed, HoursPerWeek)
#utlierBelow3sd(COVID19_Employed, Grit)

############################################################ Check for multicollinearity #####################################################

# create dataframe of just variables desired for models

correlation <- data.frame(COVID19_Employed$POMSTotalMoodDisturbance_New,
                          COVID19_Employed$Grit,
                          COVID19_Employed$TraitMentalEnergy,
                          COVID19_Employed$TraitMentalFatigue,
                          COVID19_Employed$TraitPhysicalEnergy,
                          COVID19_Employed$TraitPhysicalFatigue,
                          COVID19_Employed$VigorousActivity, 
                          COVID19_Employed$ModerateActivity,
                          COVID19_Employed$LightActivity,
                          COVID19_Employed$Sitting,
                          COVID19_Employed$HoursPerWeek, 
                          COVID19_Employed$REAP_S,
                          COVID19_Employed$HoursOfSleep)

# rename column headers to shorter names  

names(correlation)[names(correlation) == "COVID19_Employed.POMSTotalMoodDisturbance_New"] <- "POMSTotal"
names(correlation)[names(correlation) == "COVID19_Employed.VigorousActivity"] <- "VigAct"
names(correlation)[names(correlation) == "COVID19_Employed.ModerateActivity"] <- "ModAct"
names(correlation)[names(correlation) == "COVID19_Employed.LightActivity"] <- "LightAct"
names(correlation)[names(correlation) == "COVID19_Employed.Sitting"] <- "Sit"
names(correlation)[names(correlation) == "COVID19_Employed.HoursPerWeek"] <- "HoursPW"
names(correlation)[names(correlation) == "COVID19_Employed.REAP_S"] <- "REAP_S"
names(correlation)[names(correlation) == "COVID19_Employed.HoursOfSleep"] <- "Sleep"
names(correlation)[names(correlation) == "COVID19_Employed.Grit"] <- "Grit"
names(correlation)[names(correlation) == "COVID19_Employed.TraitMentalFatigue"] <- "Mfatigue"
names(correlation)[names(correlation) == "COVID19_Employed.TraitMentalEnergy"] <- "Menergy"
names(correlation)[names(correlation) == "COVID19_Employed.TraitPhysicalFatigue"] <- "PAfatigue"
names(correlation)[names(correlation) == "COVID19_Employed.TraitPhysicalEnergy"] <- "PAenergy"

# omit NA cells
correlation <- na.omit(correlation)
# run correlation
multicollinearity = cor(correlation)
# reset graphic parameters
dev.off()
# plot correlation matrix
corrplot = corrplot(multicollinearity, method="color")
# with values (has color still as well)
corrplot = corrplot(multicollinearity, method="number")

##################################################### multiple regression models for mood #####################################################

# run multiple regression and remove multivatiate outlier based on cooks distance, 
# then run again. 
# removing cooks outliers?
# plotting regression line
### Everything I find for regression lines cannot plot more than 3 IVs (such as ggPredict)
POMSmodel <- lm(POMSTotalMoodDisturbance_New ~ VigorousActivity + ModerateActivity + 
                  LightActivity + Sitting + HoursPerWeek + 
                  Gender + REAP_S + HoursOfSleep, data=COVID19_Employed)
summary (POMSmodel)
plot(POMSmodel)
vif(POMSmodel) #VIF > 10 should be removed

POMSmodel2 <- lm(POMSTotalMoodDisturbance_New ~ HoursPerWeek + 
                  Gender + REAP_S + HoursOfSleep, data=COVID19_Employed)
summary (POMSmodel2)
plot(POMSmodel2)
anova(POMSmodel, POMSmodel2)

DepressModel <- lm(POMSDepression ~ VigorousActivity + ModerateActivity + 
                  LightActivity + Sitting + HoursPerWeek + 
                  Gender + REAP_S + HoursOfSleep , data=COVID19_Employed)
summary (DepressModel)
plot(DepressModel)

AnxietyModel <- lm(POMSAnxiety ~ VigorousActivity + ModerateActivity + 
                  LightActivity + Sitting + HoursPerWeek + 
                  Gender + REAP_S + HoursOfSleep , data=COVID19_Employed)
summary (AnxietyModel)
plot(AnxietyModel)


############################################ multiple regression models for physical acticity levels ##################################################


MVPAModel = lm(MVPA ~ Gender + EssentialStatusNoSex + HoursPerWeek, data = COVID19_Employed)
summary(MVPAModel)

VigorousModel = lm(VigorousActivity ~ Gender + EssentialWorker + HoursPerWeek, data = COVID19_Employed)
summary(VigorousModel)

# categorical model as IV?

VigorousActivityModel = lm(VigorousActivity ~ SexbyEssentialStatus, COVID19_Employed)
summary(SittingModel)
# which model to run with continous DV and categorical as IV?
# regression line visualization

###################################################### univariate ANOVAs #######################################################

SitANOVA = aov(Sitting ~ SexbyEssentialStatus, data = COVID19_Employed)
summary(SitANOVA)
TukeyHSD(SitANOVA)

SittingANOVA <- ggbetweenstats(
  data = COVID19_Employed,
  x = SexbyEssentialStatus,
  y = Sitting,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.annotation = "p.value",
  pairwise.display = "significant",
  p.adjust.method = "bonferroni",
  effsize.type = "biased",
  partial = TRUE,
  effsize.noncentral = TRUE,
  bf.prior = 0.707,
  bf.message = TRUE,
  sphericity.correction = TRUE,
  results.subtitle = TRUE,
  xlab = "Essential Worker by Sex",
  ylab = "Sitting",
  caption = "",
  title = "",
  subtitle = "",
  sample.size.label = TRUE,
  k = 2,
  conf.level = 0.95,
  nboot = 100,
  tr = 0.1,
  mean.plotting = TRUE,
  mean.ci = TRUE,
  mean.point.args = list(size = 5, color = "darkred"),
  mean.label.args = list(size = 3),
  point.path = TRUE,
  point.path.args = list(alpha = 0.5, linetype = "dashed"),
  mean.path = TRUE,
  mean.path.args = list(color = "red", size = 1, alpha = 0.5),
  notch = TRUE,
  notchwidth = 0.5,
  outlier.tagging = FALSE,
  outlier.label = NULL,
  outlier.coef = 1.5,
  outlier.label.args = list(),
  outlier.point.args = list(),
  violin.args = list(width = 0.5, alpha = 0.2),
  ggtheme = ggthemes::theme_tufte(),
  ggstatsplot.layer = TRUE,
  package = "RColorBrewer",
  palette = "Dark2",
  direction = 1,
  ggplot.component = NULL,
  output = "plot",
  messages = TRUE
)
SittingANOVA

#-------------------------------------------------#
#    Map Coding                                   #
#-------------------------------------------------#

COVID19_Employed$X204[COVID19_Employed$Gender =="Male" & COVID19_Employed$EssentialWorker == "Yes"] <- "Male Essential"
COVID19_Employed$X204[COVID19_Employed$Gender =="Female" & COVID19_Employed$EssentialWorker == "Yes"] <- "Female Essential"
COVID19_Employed$X204[COVID19_Employed$Gender =="Male" & COVID19_Employed$EssentialWorker == "No"] <- "Male Non-Essential"
COVID19_Employed$X204[COVID19_Employed$Gender =="Female" & COVID19_Employed$EssentialWorker == "No"] <- "Female Non-Essential"

names(COVID19_Employed)[names(COVID19_Employed) == "X204"] <- "PostEmploymentStatusGender"

COVID19_Employed <- filter(COVID19_Employed, X36 == "187") # filters to just US employed 

names(COVID19_Employed)[names(COVID19_Employed) == "EssentialWorker"] <- "Essential Worker"

names(COVID19_Employed)[names(COVID19_Employed) == "PostEmploymentStatusGender"] <- "Essential Worker by Sex"

COVID19Map <- COVID19_Employed[c(9:10, 204)]
COVID19Map <- na.omit(COVID19Map)

COVID19MapLocations <- as_tibble(COVID19Map)

Responses <- st_as_sf(COVID19MapLocations, coords = c("X10", "X9"), crs = 4326)

COVID_map <- mapview(Responses,  legend = mapviewGetOption("legend"), 
        legend.opacity = 2, 
        color = mapviewGetOption("vector.palette"), 
        homebutton = FALSE,
        verbose = mapviewGetOption("verbose"),
        col.regions = mapviewGetOption("vector.palette"), 
        map.types = NULL, pane = "auto")

COVID_map

mapshot(
  COVID_map,
  file = ("map.png"),
  remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar",
                      "drawToolbar", "easyButton")
)

mapshot(COVID_map, URL = NULL, "C:\\Users\\blindse3\\Dropbox\\R COVID19\\map.png",
        remove_controls = c("homeButton", "layersControl"))

mapshot(COVID_map, URL = NULL, "C:\\Users\\blindse3\\Dropbox\\R COVID19\\map.jpeg",
        remove_controls = c("homeButton", "layersControl"))

mapshot(COVID_map, URL = NULL, "C:\\Users\\blindse3\\Desktop\\map.png",
        remove_controls = c("homeButton", "layersControl"))

#-------------------------------------------------#
#    Load Public data based on COVID 19 Package   #
#-------------------------------------------------#
# https://cran.r-project.org/web/packages/COVID19/readme/README.html

# other packages covid19.analytics -> potentially explore for maps

COVID19Public <- covid19("USA", level = 3, end = Sys.Date())


