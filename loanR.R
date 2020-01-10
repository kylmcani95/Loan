

#Load in data
loanDf <- read.csv("loan_data.csv")


head(loanDf)


#Need to convert to numeric factors and adjust to start at 0
loanDf$Status_Checking_Acc = as.numeric(as.factor(loanDf$Status_Checking_Acc))
loanDf$Credit_History = as.numeric(as.factor(loanDf$Credit_History))
loanDf$Purposre_Credit_Taken = as.numeric(as.factor(loanDf$Purposre_Credit_Taken))
loanDf$Savings_Acc = as.numeric(as.factor(loanDf$Savings_Acc))
loanDf$Years_At_Present_Employment = as.numeric(as.factor(loanDf$Years_At_Present_Employment))
loanDf$Marital_Status_Gender = as.numeric(as.factor(loanDf$Marital_Status_Gender))
loanDf$Other_Debtors_Guarantors = as.numeric(as.factor(loanDf$Other_Debtors_Guarantors))
loanDf$Property = as.numeric(as.factor(loanDf$Property))
loanDf$Other_Inst_Plans = as.numeric(as.factor(loanDf$Other_Inst_Plans))
loanDf$Housing = as.numeric(as.factor(loanDf$Housing))
loanDf$Job = as.numeric(as.factor(loanDf$Job))
loanDf$Telephone = as.numeric(as.factor(loanDf$Telephone))
loanDf$Foreign_Worker = as.numeric(as.factor(loanDf$Foreign_Worker))

loanDf$Status_Checking_Acc = loanDf$Status_Checking_Acc - 1
loanDf$Credit_History = loanDf$Credit_History - 1
loanDf$Purposre_Credit_Taken = loanDf$Purposre_Credit_Taken - 1
loanDf$Savings_Acc = loanDf$Savings_Acc - 1
loanDf$Years_At_Present_Employment = loanDf$Years_At_Present_Employment - 1
loanDf$Marital_Status_Gender = loanDf$Marital_Status_Gender - 1
loanDf$Other_Debtors_Guarantors = loanDf$Other_Debtors_Guarantors - 1
loanDf$Property = loanDf$Property - 1
loanDf$Other_Inst_Plans = loanDf$Other_Inst_Plans - 1
loanDf$Housing = loanDf$Housing - 1
loanDf$Job = loanDf$Job - 1
loanDf$Telephone = loanDf$Telephone - 1
loanDf$Foreign_Worker = loanDf$Foreign_Worker - 1

#Don't want to consider customer ID in regression, so lets just get rid of it for now
loanDf$Customer_ID = NULL

#Create regression of data
glm_model = glm(Default_On_Payment~.,
                data = loanDf,
                family = "binomial")


#Now, lets use the model against the training data
loanDf$prediction = predict(glm_model, loanDf, type = "response")


#Now lets write out to a file
write.csv(loanDf, file = "loan_with_prediction01.csv", row.names = FALSE)


loanRight <- subset(loanDf, (loanDf$prediction > .125 & loanDf$Default_On_Payment == 1) 
                      | (loanDf$prediction < .125 & loanDf$Default_On_Payment == 0))

loanWrong <- subset(loanDf, (loanDf$prediction < .125 & loanDf$Default_On_Payment == 1) 
                      | (loanDf$prediction > .125 & loanDf$Default_On_Payment == 0))
loanBeenFine <- subset(loanWrong, loanDf$Default_On_Payment == 0)

hist(loanBeenFine$Status_Checking_Acc, xlab = "Status of Checking Account", main = "Frequency of Wrongly Denied")
