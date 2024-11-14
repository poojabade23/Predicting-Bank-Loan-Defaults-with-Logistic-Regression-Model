#Read the data from the working directory, create your own working directly to read the dataset.

# --- Set working directory

setwd("C:\\Users\\Dell\\Desktop\\fall 2024\\bua 650")

data1 <- read.csv ("loan_default.csv",header=TRUE,sep=",")

data2<-data.frame(data1)

#perform exploratory data analysis to know about the data 

# display top 6 rows of dataset to see how data look like

head (data2)

# display bottom 6 rows

tail(data2)

# describe the structure of data

str(data2) 

#display the column name of the data

names(data2)

# display the datatype

class(data2)

#display the summary or descriptive statistics of the data

summary(data2$Amount)

#Let’s check the missing values present in the data 

is.na(data2)

#To find out the correlation between the variables

corr <- cor.test(data2$Default, data2$Term,method = "pearson" )
corr

#building logistic regression model using glm on full data

fullmodel1 <-glm(Default~.,data = data2,family=binomial (link=logit ))

summary(fullmodel1)

#removing insignificant variables in order to build final logistic model on full data

fullmodel2<-glm(Default~Checking_amount+Term+Credit_score
                +Saving_amount+Age,data = data2,family=binomial(link=logit))

summary(fullmodel2)

#splitting data set into training and validation dataset 
#in 70:30 

train_obs <- floor (0.7*nrow (data2))
print(train_obs)

#Set seed in order to reproduce the sample

set.seed(2) 
train_ind <- sample(seq_len(nrow(data2)),size=train_obs)
test <-  -train_ind

#No of observations in train dataset

train_data<-data2[train_ind,] 

# No of observations  in test dataset

test_data<-data2[-train_ind,]

testing_high = data2$Default[test]

#Building logistic regression model using glm on training data

model1<-glm(Default~.,data= train_data,family=binomial(link=logit))

summary(model1)


#After removing insignificant variable inorder to build final logistic model on training data

model2<-glm(Default~Checking_amount+Term+Credit_score
            +Emp_status+Saving_amount+Age,data= train_data,family=binomial(link=logit))

summary(model2)

#Check for variance inflation factor, VIF > 5 to 10 -high correlation

#install car package 

install.packages("car")

library(car)

vif(model2)

# Predicting the model using test data

Prob <-predict(model2,test_data,type ="response")

prob1<- data.frame(Prob)

# setting the cutoff for probability values

results <- ifelse(prob1 > 0.7,1,0)

#Display the confusion matrix or classification table

table(testing_high,results)

#Calculating the error rate

misclasificationerror <- mean(results != testing_high) 
misclasificationerror

# Calculating the accuracy rate	

accuracyrate <- 1-misclasificationerror
print(accuracyrate)

#MODEL FIT TEST 
#Hosmer-Lemeshow Test
#Install  MKmisc package

install.packages("MKmisc ")

library(MKmisc)

HLgof.test(fit = fitted(model2), obs = train_data$Default)

#Install ResourceSelection package

install.packages("ResourceSelection ")

library(ResourceSelection)

hoslem.test(train_data$Default, fitted(model2), g=10)

#Likelihood Ratio Test

model3<-glm(Default~Checking_amount+Term+Amount,
            data= train_data,family=binomial(link=logit))

summary(model3)

#Install lmtest package

install.packages("lmtest")

library(lmtest)

lrtest(model1, model3)

# Statistical Tests for Individual Predictors: Wald Test#
#Install survey package

install.packages("survey")

library(survey)

# Wald test for Credit_Score

regTermTest(model2,"Credit_score")

# Wald test for Age

regTermTest(model2, "Age")

# conducting Receiver operating characteristic (ROC) test and Area under curve (AUC)

install.packages("ROCR")

library(ROCR)

# Compute AUC for predicting Default with the model

prob <- predict(model2, newdata=test_data, type="response")

pred <- prediction(prob, test_data$Default)

pmf <- performance(pred, measure = "tpr", x.measure = "fpr")

plot(pmf,col= "red" )

auc <- performance(pred, measure = "auc")

auc <- auc@y.values[[1]]

auc	

#######Additional #############
# Marginal Effects for Credit Score
marginal_effects <- margins(fullmodel2, variables = "Credit_score")
print(marginal_effects)

# Define specific credit scores and estimate probabilities
specific_scores <- c(655, 745)
coefficients <- coef(fullmodel2)

estimated_probs <- sapply(specific_scores, function(score) {
  linear_combination <- coefficients[1] + coefficients['Credit_score'] * score + 
    coefficients['Checking_amount'] * mean(data2$Checking_amount) + 
    coefficients['Term'] * mean(data2$Term) + 
    coefficients['Saving_amount'] * mean(data2$Saving_amount) + 
    coefficients['Age'] * mean(data2$Age)
  prob <- exp(linear_combination) / (1 + exp(linear_combination))
  return(prob)
})

names(estimated_probs) <- specific_scores
print(estimated_probs)

# Calculate Odds for 655 and 745
odds_655 <- estimated_probs[1] / (1 - estimated_probs[1])
odds_745 <- estimated_probs[2] / (1 - estimated_probs[2])

cat("Odds for 655:", odds_655, "\n")
cat("Odds for 745:", odds_745, "\n")

# Calculate the odds ratio for a 90-point increase
odds_ratio_90 <- odds_745 / odds_655
cat("Odds Ratio for 90-point increase (655 to 745):", odds_ratio_90, "\n")

# --- Step 9: Incremental Changes in Probabilities and Odds (10-Point Increments) ---
fico_range <- seq(600, 700, by = 10)

incremental_results <- data.frame(
  Credit_Score = fico_range,
  Probability = sapply(fico_range, function(x) {
    linear_combination <- coefficients[1] + coefficients['Credit_score'] * x + 
      coefficients['Checking_amount'] * mean(data2$Checking_amount) + 
      coefficients['Term'] * mean(data2$Term) + 
      coefficients['Saving_amount'] * mean(data2$Saving_amount) + 
      coefficients['Age'] * mean(data2$Age)
    prob <- exp(linear_combination) / (1 + exp(linear_combination))
    return(prob)
  }),
  Odds = sapply(fico_range, function(x) {
    linear_combination <- coefficients[1] + coefficients['Credit_score'] * x + 
      coefficients['Checking_amount'] * mean(data2$Checking_amount) + 
      coefficients['Term'] * mean(data2$Term) + 
      coefficients['Saving_amount'] * mean(data2$Saving_amount) + 
      coefficients['Age'] * mean(data2$Age)
    prob <- exp(linear_combination) / (1 + exp(linear_combination))
    odds <- prob / (1 - prob)
    return(odds)
  })
)

print(incremental_results)

# --- Step 10: Graphs ---

# Plot for estimated probabilities (655 and 745)
prob_data <- data.frame(
  Credit_Score = specific_scores,
  Probability = estimated_probs
)

ggplot(prob_data, aes(x = Credit_Score, y = Probability)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 30) +
  geom_text(aes(label = round(Probability, 3)), vjust = -0.5, size = 5) +
  theme_minimal() +
  labs(title = "Estimated Probabilities for Credit Scores",
       x = "Credit Score", y = "Estimated Probability")

# Plot for incremental changes
ggplot(incremental_results, aes(x = Credit_Score)) +
  geom_line(aes(y = Probability, color = "Probability"), size = 1.2) +
  geom_line(aes(y = Odds, color = "Odds"), size = 1.2, linetype = "dashed") +
  theme_minimal() +
  scale_color_manual(values = c("Probability" = "blue", "Odds" = "red")) +
  labs(title = "Incremental Changes in Probability and Odds",
       x = "Credit Score", y = "Value", color = "Legend") +
  geom_point(aes(y = Probability), color = "blue", size = 3) +
  geom_point(aes(y = Odds), color = "red", size = 3)

# Load the necessary library
library(ROCR)

# Step 1: Ensure that 'test_data' is a data frame
test_data <- as.data.frame(test_data)

# Step 2: Generate predicted probabilities using 'fullmodel2'
# Convert 'Default' to a numeric factor (if not already)
test_data$Default <- as.numeric(as.character(test_data$Default))

# Step 3: Predict probabilities for the test data
prob_test <- predict(fullmodel2, newdata = test_data, type = "response")

# Step 4: Create a prediction object using ROCR
pred <- prediction(prob_test, test_data$Default)

# Step 5: Calculate the ROC performance
roc_perf <- performance(pred, measure = "tpr", x.measure = "fpr")

# Step 6: Calculate AUC
auc_perf <- performance(pred, measure = "auc")
auc_value <- auc_perf@y.values[[1]]

# Step 7: Plot the ROC curve
plot(roc_perf, col = "red", main = "ROC Curve for Logistic Regression Model", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal line (random guess)
text(0.6, 0.2, paste("AUC =", round(auc_value, 3)), col = "blue", cex = 1.2)

# --- Step 11: Calculate Credit Score for Specific Odds Ratios ---

# For 50% probability of approval
even_odds_score <- (log(1) - coefficients[1] - 
                      coefficients['Checking_amount'] * mean(data2$Checking_amount) - 
                      coefficients['Term'] * mean(data2$Term) - 
                      coefficients['Saving_amount'] * mean(data2$Saving_amount) - 
                      coefficients['Age'] * mean(data2$Age)) / coefficients['Credit_score']

cat("Credit Score for 50% probability of approval:", round(even_odds_score), "\n")

# For 3:1 odds ratio
target_odds <- 3
score_for_3_1_odds <- (log(target_odds) - coefficients[1] - 
                         coefficients['Checking_amount'] * mean(data2$Checking_amount) - 
                         coefficients['Term'] * mean(data2$Term) - 
                         coefficients['Saving_amount'] * mean(data2$Saving_amount) - 
                         coefficients['Age'] * mean(data2$Age)) / coefficients['Credit_score']

cat("Credit Score for 3:1 odds ratio:", round(score_for_3_1_odds), "\n")

