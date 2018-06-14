
#Then run a random forest on our players that we're predicting will make the NBA (otherwise it's too easy for the algorhythm to guess all No_NBA)
library(ranger)

training_data_RF<-training_data[training_data$predict_prob>.5,]

testing_data_RF<-testing_data[testing_data$predict_prob>.5,]

syt_outcome_variable<-'nba_category'

syt_predictor_variables<-as.list(colnames(training_data_RF))
#All numeric
syt_predictor_variables<-syt_predictor_variables[seq(3,65,1)]
syt_fmla<-as.formula(paste(paste(syt_outcome_variable,collapse = "+"),"~",paste(syt_predictor_variables,collapse = "+")))


same_year_tree<-ranger(syt_fmla,training_data_RF,num.trees=1000,mtry=30,write.forest=TRUE)

training_data_RF$same_year_prediction<-same_year_tree$predictions


##Next year tree

nyt_outcome_variable<-'nextyr_cat'

nyt_fmla<-as.formula(paste(paste(nyt_outcome_variable,collapse = "+"),"~",paste(syt_predictor_variables,collapse = "+")))


next_yr_tree<-ranger(nyt_fmla,training_data_RF,num.trees=10000,mtry=50,write.forest=TRUE)

training_data_RF$next_yr_prediction<-next_yr_tree$predictions



predictions<-predict(next_yr_tree,data = testing_data_RF,type = 'response')

testing_data_RF$predictions<-predictions$predictions

#Need to re-structure the data to ensure that all of the predictions can pan out (ie no cut off date), also need to maybe re-work categories to give more variety
