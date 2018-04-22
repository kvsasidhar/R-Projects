library(tidyr)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(lubridate)
library(dplyr)
library(tidyr)
library(scales)



install.packages("ROCR")
library(ROCR)

install.packages("randomForest")
library(randomForest)

 diabetis <- read.csv("diabetic_data.csv",na.strings = c("","?"))
 nrow(diabetis)#101766
 str(diabetis)
 
 #replace "?" with NA in the df
 #diabetis <- diabetis[diabetis == "?"] <- NA
# is.na(#)
 
 ncol(diabetis)
 nrow(diabetis)
 
 # find duplicate variables if any
 #sum(duplicated(colnames(diabetis)))# zero, so no duplicate variables

 #remove unnecessary and redundant variables
diabetis_model <- diabetis[,c(5,10,13:22,24,42,48,49,50)]
ncol(diabetis_model)#17 variables

#check for columns with NA in diabetis_model dataframe

colnames(diabetis_model)[colSums(is.na(diabetis_model)) > 0]# "diag_1" "diag_2" "diag_3"

#converting factor to numeric
# warning!!! - give some gap between each line execution if you are getting "coercion" error
diabetis_model$diag_1 <- as.numeric(as.character(diabetis_model$diag_1))
diabetis_model$diag_2 <- as.numeric(as.character(diabetis_model$diag_2))
diabetis_model$diag_3 <- as.numeric(as.character(diabetis_model$diag_3))

# replacing NA with 250, 394, and 459 codes for diag_1,diag_2,diag_3 respectively
diabetis_model$diag_1[which(is.na( diabetis_model$diag_1))] <- 0
diabetis_model$diag_2[which(is.na( diabetis_model$diag_2))] <- 0
diabetis_model$diag_3[which(is.na( diabetis_model$diag_3))] <- 0

#
#o- other; d - Diabatis; c - Circulatory disease
diabetis_model$diag_11 <- ifelse((diabetis_model$diag_1 > 250.00 & diabetis_model$diag_1 < 251),'d',
                                 ifelse((diabetis_model$diag_1 > 394 & diabetis_model$diag_1 < 459), 'c','o'))

diabetis_model$diag_22 <- ifelse((diabetis_model$diag_2 > 250.00 & diabetis_model$diag_2 < 251),'d',
                                 ifelse((diabetis_model$diag_2 > 394 & diabetis_model$diag_2 < 459), 'c','o'))

diabetis_model$diag_33 <- ifelse((diabetis_model$diag_3 > 250.00 & diabetis_model$diag_3 < 251),'d',
                                 ifelse((diabetis_model$diag_3 > 394 & diabetis_model$diag_3 < 459), 'c','o'))


#concatenate the 3 diagnosis
diabetis_model$comorbidity <- paste(diabetis_model$diag_11,diabetis_model$diag_22,diabetis_model$diag_33)



#o- other; d - Diabatis; c - Circulatory disease
diabetis_model$comorbidity[which(diabetis_model$comorbidity=="d d d")]=1
diabetis_model$comorbidity[which(diabetis_model$comorbidity=="d d c")]=3
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="d d o")]=1
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="d c o")]=3
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="d c c")]=3
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="d o o")]=1
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="d o c")]=3
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="o d d")]=1
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="o d o")]=1
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="c d c")]=3
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="o o d")]=1
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="c c d")]=3
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="c c c")]=2
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="c c o")]=2
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="o o c")]=2
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="o c o")]=2
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="o c c")]=2
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="c o c")]=2
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="o o o")]=0
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="c d d")]=3
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="c o d")]=3
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="c d o")]=3
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="o d c")]=3
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="o c d")]=3
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="d o d")]=1
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="c o o")]=2
  diabetis_model$comorbidity[which(diabetis_model$comorbidity=="d c d")]=3
  
  #convert comorbidity to factor
  diabetis_model$comorbidity <- as.factor(diabetis_model$comorbidity)
  

#As comorbity variable has been created, remove the diag_1,diag_2,diag_3, diag_11,diag_22,diag_33 variables from diabetis_model
diabetis_model <- diabetis_model[,-c(9,10,11,18,19,20)]

#club >30 and <30 to "YES"
levels(diabetis_model$readmitted) <- c('YES','YES','NO')

#Exploratory analytics

#In the whole population if you see in the age group other than 40 - 70,  females are more suffering from diabetis compard to males
ggplot(diabetis, aes(x=diabetis_model$age,fill=gender))+ geom_bar(position = "fill")+ scale_y_continuous(labels=scales::percent)

#the below bar graph shows that the number diabetic patients gradually increase from 0-10 age group till 70-80 age group and decreases there on.
#The reason for decline in the graph after 80 could be that the population after 80  itself is less It decreases after as the average life span of a person 
#is less than 80
ggplot(diabetis_model, aes(x=age))+ geom_bar(position = "stack")

# From the plot we can observe that the proportion of  patients diagnosed as Diabetics have high A1Cresult score(>8) 
# compared to those who been diagnosd with both diabetics and circulatory disease. This implies that the pure diabetic patients 
# disease is not controlled in the last 3 months
ggplot(diabetis_model, aes(x=diabetis_model$comorbidity,fill=A1Cresult))+ geom_bar(position = "fill")+ scale_y_continuous(labels=scales::percent)


#boxplot for num_medications
boxplot(diabetis_model$num_medications)
#treating outliers
qnt <- quantile(diabetis_model$num_medications, probs=c(.25, .75))
caps <- quantile(diabetis_model$num_medications, probs=c(.05, .92))
H <- 1.5 * IQR(diabetis_model$num_medications)
diabetis_model$num_medications[diabetis_model$num_medications < (qnt[1] - H)] <- caps[1]
diabetis_model$num_medications[diabetis_model$num_medications > (qnt[2] + H)] <- caps[2]

boxplot(diabetis_model$num_medications)


#boxplot for number_inpatient
boxplot(diabetis_model$number_inpatient)
#treating outliers
qnt <- quantile(diabetis_model$number_inpatient, probs=c(.25, .75))
caps <- quantile(diabetis_model$number_inpatient, probs=c(.05, .92))
H <- 1.5 * IQR(diabetis_model$number_inpatient)
diabetis_model$number_inpatient[diabetis_model$number_inpatient < (qnt[1] - H)] <- caps[1]
diabetis_model$number_inpatient[diabetis_model$number_inpatient > (qnt[2] + H)] <- caps[2]

boxplot(diabetis_model$number_inpatient)

#boxplot for number_emergency
boxplot(diabetis$number_emergency)
#treating outliers
qnt <- quantile(diabetis_model$number_emergency, probs=c(.25, .75))
caps <- quantile(diabetis_model$number_emergency, probs=c(.05, .92))
H <- 1.5 * IQR(diabetis_model$number_emergency)
diabetis_model$number_emergency[diabetis_model$number_emergency < (qnt[1] - H)] <- caps[1]
diabetis_model$number_emergency[diabetis_model$number_emergency > (qnt[2] + H)] <- caps[2]

boxplot(diabetis_model$number_emergency)

#boxplot for number_diagnoses
boxplot(diabetis$number_diagnoses)
#treating outliers
qnt <- quantile(diabetis_model$number_diagnoses, probs=c(.25, .75))
caps <- quantile(diabetis_model$number_diagnoses, probs=c(.05, .92))
H <- 1.5 * IQR(diabetis_model$number_diagnoses)
diabetis_model$number_diagnoses[diabetis_model$number_diagnoses < (qnt[1] - H)] <- caps[1]
diabetis_model$number_diagnoses[diabetis_model$number_diagnoses > (qnt[2] + H)] <- caps[2]


#assigning 0,1 levels for binary factor variables
levels(diabetis_model$change) <- c(1,0)
levels(diabetis_model$diabetesMed) <- c(0,1)
levels(diabetis_model$readmitted) <- c(1,0)

diabetis_model$readmitted <- as.numeric(levels(diabetis_model$readmitted))[diabetis_model$readmitted]

# create dummy variables
#creating the dummy variable for the variables which is having more than 2 levels
#A1Cresult
dummy_1<- data.frame(model.matrix(~A1Cresult,data = diabetis_model))
dummy_1<- dummy_1[,-1]

#insulin
dummy_2<- data.frame(model.matrix(~insulin,data = diabetis_model))
dummy_2<- dummy_2[,-1]

#comorbidity
dummy_3<- data.frame(model.matrix(~comorbidity,data = diabetis_model))
dummy_3<- dummy_3[,-1]

dummy_4<- data.frame(model.matrix(~age,data = diabetis_model))
dummy_4<- dummy_4[,-1]

# binding teh dummy variables to the diabetis datase
diabetis_model_1 <- cbind(diabetis_model[,-c(1,10:13,15)],dummy_1,dummy_2,dummy_3,dummy_4)

# Normalising continuous features
#time_in_hospital
diabetis_model_1$time_in_hospital<- scale(diabetis_model_1$time_in_hospital)

#num_lab_procedures
diabetis_model_1$num_lab_procedures<- scale(diabetis_model_1$num_lab_procedures)

#num_procedures
diabetis_model_1$num_procedures<- scale(diabetis_model_1$num_procedures)

#num_medications
diabetis_model_1$num_medications<- scale(diabetis_model_1$num_medications)

#number_outpatient
diabetis_model_1$number_outpatient<- scale(diabetis_model_1$number_outpatient)

#number_emergency
diabetis_model_1$number_emergency<- scale(diabetis_model_1$number_emergency)

#number_inpatient
diabetis_model_1$number_inpatient<- scale(diabetis_model_1$number_inpatient)

#number_diagnoses
diabetis_model_1$number_diagnoses<- scale(diabetis_model_1$number_diagnoses)


# splitting the data between train and test
set.seed(100)
trainindices= sample(1:nrow(diabetis_model_1), 0.7*nrow(diabetis_model_1))
train = diabetis_model_1[trainindices,]
test = diabetis_model_1[-trainindices,]


#using Logistic regression on the train dataset
model_1 = glm(readmitted ~ ., data = train, family = "binomial")
summary(model_1) 

# Stepwise Backward selection
model_2<- stepAIC(model_1, direction="both")

summary(model_2)#AIC: 92923
sort(vif(model_2),decreasing = TRUE)

#of all thevariables insulipup has high p value.lets remove it and see

model_3 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
      num_procedures + number_outpatient + number_emergency + number_inpatient + 
      number_diagnoses + A1CresultNone + A1CresultNorm + insulinNo + 
      insulinSteady + comorbidity1 + comorbidity2 + 
      comorbidity3 + age.10.20. + age.20.30. + age.30.40. + age.40.50. + 
      age.50.60. + age.60.70. + age.70.80. + age.80.90. + age.90.100., 
    family = "binomial", data = train)

summary(model_3)#AIC: 92924
sort(vif(model_3),decreasing = TRUE)

# Removing variable age.80.90. as it has high p value and its VIF is also high 


model_4 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + A1CresultNone + A1CresultNorm + insulinNo + 
                 insulinSteady + comorbidity1 + comorbidity2 + 
                 comorbidity3 + age.10.20. + age.20.30. + age.30.40. + age.40.50. + 
                 age.50.60. + age.60.70. + age.70.80. + age.90.100., 
               family = "binomial", data = train)

summary(model_4)
sort(vif(model_4),decreasing = TRUE)

#theh vifs of all the variables is below 2 in this iteratio. BUt age.60.70. is highly insignificant. so lets remove it

model_5 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + A1CresultNone + A1CresultNorm + insulinNo + 
                 insulinSteady + comorbidity1 + comorbidity2 + 
                 comorbidity3 + age.10.20. + age.20.30. + age.30.40. + age.40.50. + 
                 age.50.60. + age.70.80. + age.90.100., 
               family = "binomial", data = train)

summary(model_5)
sort(vif(model_5),decreasing = TRUE)


#rewmoving age.10.20 as it is highly insisgificant

model_6 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + A1CresultNone + A1CresultNorm + insulinNo + 
                 insulinSteady + comorbidity1 + comorbidity2 + 
                 comorbidity3 + age.20.30. + age.30.40. + age.40.50. + 
                 age.50.60. + age.70.80. + age.90.100., 
               family = "binomial", data = train)

summary(model_6)
sort(vif(model_6),decreasing = TRUE)


#rewmoving A1CresultNone as it is highly insisgificant

model_7 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + A1CresultNorm + insulinNo + 
                 insulinSteady + comorbidity1 + comorbidity2 + 
                 comorbidity3 + age.20.30. + age.30.40. + age.40.50. + 
                 age.50.60. + age.70.80. + age.90.100., 
               family = "binomial", data = train)

summary(model_7)
sort(vif(model_7),decreasing = TRUE)


#rewmoving age.70.80 as it is highly insisgificant

model_8 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + A1CresultNorm + insulinNo + 
                 insulinSteady + comorbidity1 + comorbidity2 + 
                 comorbidity3 + age.20.30. + age.30.40. + age.40.50. + 
                 age.50.60. + age.90.100., 
               family = "binomial", data = train)

summary(model_8)
sort(vif(model_8),decreasing = TRUE)

#rewmoving age.20.30 as it is highly insisgificant

model_9 <- glm(formula = readmitted ~ time_in_hospital + num_lab_procedures + 
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + A1CresultNorm + insulinNo + 
                 insulinSteady + comorbidity1 + comorbidity2 + 
                 comorbidity3 + age.30.40. + age.40.50. + 
                 age.50.60. + age.90.100., 
               family = "binomial", data = train)

summary(model_9)
sort(vif(model_9),decreasing = TRUE)

#rewmoving num_lab_procedures as it is highly insisgificant

model_10 <- glm(formula = readmitted ~ time_in_hospital +  
                 num_procedures + number_outpatient + number_emergency + number_inpatient + 
                 number_diagnoses + A1CresultNorm + insulinNo + 
                 insulinSteady + comorbidity1 + comorbidity2 + 
                 comorbidity3 + age.30.40. + age.40.50. + 
                 age.50.60. + age.90.100., 
               family = "binomial", data = train)

summary(model_10)
sort(vif(model_10),decreasing = TRUE)


#As you can see below all the variables are significant and all the vifs are below 2.
#So we have arrived at stable model - model_10

'
Call:
  glm(formula = readmitted ~ time_in_hospital + num_procedures + 
        number_outpatient + number_emergency + number_inpatient + 
        number_diagnoses + A1CresultNorm + insulinNo + insulinSteady + 
        comorbidity1 + comorbidity2 + comorbidity3 + age.30.40. + 
        age.40.50. + age.50.60. + age.90.100., family = "binomial", 
      data = train)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-3.9081  -1.0367  -0.8413   1.2115   1.7693  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)       -0.118663   0.021907  -5.417 6.07e-08 ***
  time_in_hospital   0.049067   0.008258   5.942 2.82e-09 ***
  num_procedures    -0.086817   0.008142 -10.662  < 2e-16 ***
  number_outpatient  0.108969   0.009134  11.930  < 2e-16 ***
  number_emergency   0.207653   0.013758  15.093  < 2e-16 ***
  number_inpatient   0.454224   0.010425  43.569  < 2e-16 ***
  number_diagnoses   0.165801   0.008326  19.914  < 2e-16 ***
  A1CresultNorm     -0.144797   0.036300  -3.989 6.64e-05 ***
  insulinNo         -0.165028   0.020546  -8.032 9.57e-16 ***
  insulinSteady     -0.159212   0.021794  -7.305 2.76e-13 ***
  comorbidity1       0.247769   0.027566   8.988  < 2e-16 ***
  comorbidity2       0.175081   0.018266   9.585  < 2e-16 ***
  comorbidity3       0.513600   0.031385  16.364  < 2e-16 ***
  age.30.40.        -0.184838   0.043697  -4.230 2.34e-05 ***
  age.40.50.        -0.135577   0.027974  -4.847 1.26e-06 ***
  age.50.60.        -0.092949   0.021510  -4.321 1.55e-05 ***
  age.90.100.       -0.302489   0.048312  -6.261 3.82e-10 ***
  ---



  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 98321  on 71235  degrees of freedom
Residual deviance: 92919  on 71219  degrees of freedom
AIC: 92953

Number of Fisher Scoring iterations: 4

> sort(vif(model_10),decreasing = TRUE)
insulinNo     insulinSteady      comorbidity2      comorbidity1      comorbidity3  time_in_hospital 
1.725897          1.647360          1.370561          1.288978          1.178019          1.123410 
number_diagnoses    num_procedures        age.40.50.        age.30.40.        age.50.60.  number_emergency 
1.109462          1.084032          1.069847          1.059464          1.056553          1.052276 
number_inpatient number_outpatient       age.90.100.     A1CresultNorm 
1.052154          1.024220          1.018803          1.005600 '


#Final MOdel formula

'Risk of readmission = -0.118663 + 0.049067*time_in_hospital - 0.086817*num_procedures + 0.108969*number_outpatient + 0.207653*number_emergency
+0.454224*number_inpatient + 0.165801*number_diagnoses - 0.144797*A1CresultNorm - 0.165028*insulinNo - 0.159212*insulinSteady
+ 0.247769*comorbidity1 + 0.175081*comorbidity2 + 0.513600*comorbidity3 - 0.184838*age.30.40. - 0.135577*age.40.50.
- 0.092949*age.50.60. - 0.302489*age.90.100.'


#MOdel Evaluation using test data
final_model <- model_10

test_pred = predict(final_model, type = "response", 
                    newdata = test)
summary(test_pred)# probabilities range from 19% to 100% 

test$prob<- test_pred

#Let's Choose the cutoff value. 

# Let's find out the optimal probalility cutoff 


test_actual_readmitted <- factor(ifelse(test$readmitted==1,"Yes","No"))

perform_fn <- function(cutoff) 
{
  predicted_readmission <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_readmission, test_actual_readmitted, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(test_pred)

s = seq(.01,.95,length=1000)

OUT = matrix(0,1000,3)


for(i in 1:1000)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)|
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.4343644 for final model
# [1] 0.4334234 0.4343644 0.4353053

test_cutoff_readmission <- factor(ifelse(test_pred >= 0.4343644, "Yes", "No"))

conf_final <- confusionMatrix(test_actual_readmitted, test_cutoff_readmission, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc #0.6146086

sens #0.582017

spec #0.6421791

#Since accuracy, sensitivity and specificity are almost same, so this a pretty good model.

#                                  ---------------------------------
#                                 | Readmission risk stratification |
#                                  ---------------------------------                                              

#
test$readmission_risk <- ifelse(test$prob <= 0.3, "LOW RISK",
                                ifelse(test$prob > 0.3 & test$prob <=0.7, "MEDIUM RISK", "HIGH RISK"))

test$readmission_risk <- as.factor(test$readmission_risk)
View(test)


'                                   MOdel evaluation using KS Static
                                   ----------------------------------'
  
test_cutoff_readmission <- ifelse(test_cutoff_readmission =="Yes",1,0)
test_actual_readmitted <- ifelse(test_actual_readmitted=="Yes",1,0)

#on testing  data
pred_object_test<- prediction(test_cutoff_readmission, test_actual_readmitted)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])
max(ks_table_test)#0.2221681

#KS-statistic is 22.2% which is less than 40%  implies our model is not a good model.

# Lift & Gain Chart 

# plotting the lift chart

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

readmission_decile = lift(test_actual_readmitted, test_pred, groups = 10)
readmission_decile

#At 4th decile cummulative gain is 51.9% and lift is 1.29 i.e. our model is 129% better than
#random model.

# A tibble: 10 x 6
'bucket total totalresp Cumresp      Gain  Cumlift
<int> <int>     <dbl>   <dbl>     <dbl>    <dbl>
  1      1  3053      2243    2243  15.95192 1.595192
2      2  3053      1874    4117  29.27957 1.463978
3      3  3053      1663    5780  41.10661 1.370220
4      4  3053      1525    7305  51.95221 1.298805
5      5  3053      1415    8720  62.01550 1.240310
6      6  3053      1288   10008  71.17559 1.186260
7      7  3053      1215   11223  79.81651 1.140236
8      8  3053      1087   12310  87.54712 1.094339
9      9  3053       970   13280  94.44563 1.049396
10     10  3053       781   14061 100.00000 1.000000'
 

####                                 Random Forest Model
#                                    -------------------

data.rf <- randomForest(readmitted ~ ., data=train, proximity=FALSE,
                        ntree=1000, do.trace=TRUE)
data.rf
testPred_rf <- predict(data.rf, newdata=test)
summary(testPred_rf)# Min: 3.9% Max:98.2%
test$prob_RF <- testPred_rf


#                                     Risk Strtification
#                                     -------------------

test$readmission_risk_RF <- ifelse(test$prob_RF <= 0.3, "LOW RISK",
                                ifelse(test$prob_RF > 0.3 & test$prob_RF <=0.7, "MEDIUM RISK", "HIGH RISK"))

test$readmission_risk_RF <- as.factor(test$readmission_risk)
View(test)


plot_grid( 
ggplot(data=test, aes(x=readmission_risk, group =1)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  labs(title="Readmission Risk - Logistic Regression",y="percentage (%)")+
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5),


ggplot(data=test, aes(x=readmission_risk_RF, group =1)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  labs(title="Readmission Risk  - Random Forest",y="percentage (%)")+
  geom_text(aes( label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.5))

# IN the above plot we see that both the models have stratified the patient population into 3 buckets
#of high medium and low with same percentage . 

test_actual_Readmission_RF <- factor(ifelse(test$readmitted==1,"Yes","No"))

perform_fn_RF <- function(cutoff) 
{
  predicted_readmitted_RF <- factor(ifelse(testPred_rf >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_readmitted_RF, test_actual_Readmission_RF, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s_RF = seq(.01,.95,length=1000)

OUT_RF = matrix(0,1000,3)


for(i in 1:1000)
{
  OUT_RF[i,] = perform_fn_RF(s_RF[i])
} 

plot(s, OUT_RF[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)|
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_RF[,2],col="darkgreen",lwd=2)
lines(s,OUT_RF[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff_RF <- s_RF[which(abs(OUT_RF[,1]-OUT_RF[,2])<0.01)]
cutoff_RF #0.4541241 0.4550651 0.4560060 0.4569469

test_cutoff_readmission_RF <- factor(ifelse(testPred_rf >=0.4550651, "Yes", "No"))

conf_final_RF <- confusionMatrix(test_cutoff_readmission_RF, test_actual_Readmission_RF, positive = "Yes")

acc_RF <- conf_final_RF$overall[1]

sens_RF <- conf_final_RF$byClass[1]

spec_RF <- conf_final_RF$byClass[2]

acc_RF #0.6040943

sens_RF #0.6056468

spec_RF #0.6027688

#Since accuracy, sensitivity and specificity are almost same, so this a pretty good model.

### KS -statistic - Test Data ######

test_cutoff_readmission_RF <- ifelse(test_cutoff_readmission_RF=="Yes",1,0)
test_actual_Readmission_RF <- ifelse(test_actual_Readmission_RF=="Yes",1,0)

#on testing  data
pred_object_test_RF<- prediction(test_cutoff_readmission_RF, test_actual_Readmission_RF)

performance_measures_test_RF<- performance(pred_object_test_RF, "tpr", "fpr")

ks_table_test_RF <- attr(performance_measures_test_RF, "y.values")[[1]] - 
  (attr(performance_measures_test_RF, "x.values")[[1]])

max(ks_table_test_RF)#0.2084157

#KS-statistic is 20.8% which is less than 40% which implies our model is not a good model

# Lift & Gain Chart 

# plotting the lift chart

readmission_decile_RF = lift(test_actual_Readmission_RF, testPred_rf, groups = 10)
readmission_decile_RF
#At 4th decile cummulative gain is 51.2% and lift is 1.27 i.e. our model is 127% better than
#random model.

# A tibble: 10 x 6
'bucket total totalresp Cumresp      Gain  Cumlift
<int> <int>     <dbl>   <dbl>     <dbl>    <dbl>
  1      1  3053      2200    2200  15.64611 1.564611
2      2  3053      1850    4050  28.80307 1.440154
3      3  3053      1637    5687  40.44520 1.348173
4      4  3053      1506    7193  51.15568 1.278892
5      5  3053      1417    8610  61.23320 1.224664
6      6  3053      1342    9952  70.77733 1.179622
7      7  3053      1205   11157  79.34713 1.133530
8      8  3053      1112   12269  87.25553 1.090694
9      9  3053       953   13222  94.03314 1.044813
10     10  3053       839   14061 100.00000 1.000000'

#Conclusion: Both models have displayed almost nearly equal statistics and have stratified the patient
# population into 3 buckets with same percentages.

             #Random Forest                          Logistic Regression
#            --------------                         -------------------
'accuracy      60.4%                                      61.2%

sensitivity    60.5%                                      61.0%

specificty     60.2%                                      61.3%

KS Static      20.8%                                      22.2%

Gain           51.2%                                      51.9%
