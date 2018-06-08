#Use this script to do our inital (logistic regression) predictions on whether a player made it to the NBA or not

#Assign simple categories that tell us if a player made it to the NBA or not

source('reduced_NBA_categories.R')
#This gives different designations to players
reduced_data_categories<-reduced_NBA_categories(merged_data)

#For right now we can't use the last season, also adding in a binary for whether they played in the NBA or not.

merged_data_categories<-reduced_data_categories%>%
  filter(between(year,2002,2017)) %>%
  #Defining this will be critically important... not sure if this is the right approach, or whether they need to have stayed in the league both years
  mutate(played_NBA= ifelse(nba_category=="Never NBA" & nextyr_cat == "Never NBA",0,1))

#We also only want to look at players who played in both leagues
drops_list<-c("Age.x","NBA_WS.dum","G_WS.dum","dup.x","dup.y")
ML_data<-merged_data_categories%>%
  filter(G_G>=1) %>%
  select(-one_of(drops_list))

#so we have 2892 players that played in both leagues

non_missing<-colnames(ML_data[c(3,73,seq(77,139,1),142,143,144)])
ML_data<-ML_data[names(ML_data) %in% non_missing]
#Looking at only columns with non-missing data and players who have complete data
ML_data<-na.omit(ML_data)
#2017 players remaining

ML_data$played_NBA<-as.factor(ML_data$played_NBA)

training_data<-ML_data%>%
  filter(between(year,2002,2015))
#1508 players
testing_data<-ML_data%>%
  filter(between(year,2016,2017))
#509 players --- so we have about an ideal 2:1 split

lm_outcome_variables<-'played_NBA'
#Start by looking at games, minutes and then 36 and advanced stats
predictor_variables<-as.list(colnames(ML_data))
#Per 36 
#predictor_variables<-predictor_variables[seq(3,50,1)]
#All numeric
predictor_variables<-predictor_variables[seq(3,65,1)]


lm_fmla<-as.formula(paste(paste(lm_outcome_variables,collapse = "+"),"~",paste(predictor_variables,collapse = "+")))

#First do a logistic regression to see if we can predict whether they're in the NBA or not.

log_reg<-glm(lm_fmla,family = binomial(link='logit'),data = training_data)

training_data$predict_prob<-predict.glm(log_reg,training_data,type = "response")

#What was important to the regression?

coefficients<-log_reg$coefficients

print(sort(coefficients, decreasing = TRUE))


#So from our training data, how many actually made the NBA?

made_NBA_train<-length(training_data[training_data$played_NBA==1,]$Player)

#so 487 players made played in the NBA at least 1 year.

#Predict + Make rate

predict_NBA_train<-length(training_data[(training_data$played_NBA==1 &training_data$predict_prob>.5),]$Player)

#So we have 342 players who made the NBA and had a greater than .5 probabilty of being predicted.

#342/487 = .7   is actually slightly higher than that, because it did a good job of predicting players who would have name changes
#ex. Dennis with an umlot vs without, Jose Barea vs JJ etc.

#And now testing it on our Testing data

testing_data$predict_prob<-predict.glm(log_reg,testing_data,type = "response")








