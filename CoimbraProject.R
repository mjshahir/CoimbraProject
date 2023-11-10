#DBST 667
#Munira Shahir
#Breast Cancer Coimbra

#Set Directory
setwd("Documents/DBST 667")

#Load Packages
library(ggplot2)
library(gridExtra)
library(arules)
library(pROC)
library(party)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

#Load Data
coimbra <-read.csv(file="coimbra.csv")

#Descriptive Analysis
#Structure
str(coimbra)

#Calculate Adiponectin/Leptin Ratio
coimbra$Ratio <- coimbra$Adiponectin/coimbra$Leptin

#Summary Statistics
summary(coimbra)

## Patients, classification = 2
coimbra_p <- coimbra[coimbra$Classification == 2,]
summary(coimbra_p)

## Healthy, classification = 1
coimbra_h <- coimbra[coimbra$Classification == 1,]
summary(coimbra_h)
#Missing Data check
colSums((is.na(coimbra)))

# Histograms
## All predictive attributes

## Age
age_hist <- ggplot(data = coimbra, aes(x = Age, y = after_stat(density))) + 
  geom_histogram(color = "black", fill = "gray") + 
  geom_density(color="black", linewidth = 1) + xlab("Age") + 
  labs(title = "Histogram and Density Plot of Age")

age_hist_h <- ggplot(data = coimbra_h, aes(x = Age, y = after_stat(density))) + 
  geom_histogram(color = "black", fill = "gray") + 
  geom_density(color = "black", linewidth = 1) + xlab("Age") +
  labs(title = "Histogram and Density Plot of Age for Healthy Patients")

age_hist_p <- ggplot(data = coimbra_p, aes(x = Age, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") + 
  geom_density(color = "black", linewidth = 1) + xlab("Age") + 
  labs(title = "Histogram and Density Plot of Age for Cancer Patients")

age_hist
age_hist_h
age_hist_p

## BMI
bmi_hist <- ggplot(data = coimbra, aes(x = BMI, y = after_stat(density))) + 
  geom_histogram(color = "black", fill = "gray") + 
  geom_density(color = "black", linewidth = 1) + xlab("BMI") +
  labs(title = "Histogram and Density Plot of BMI")

bmi_hist_h <- ggplot(data = coimbra_h, aes(x = BMI, y = after_stat(density))) + 
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("BMI") + 
  labs(title = "Histogram and Density Plot of BMI for Healthy Patients")

bmi_hist_p <- ggplot(data = coimbra_p, aes(x = BMI, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("BMI") +
  labs(title = "Histogram and Density Plot of BMI for Cancer Patients")

bmi_hist
bmi_hist_h
bmi_hist_p

## Glucose
glu_hist <- ggplot(data = coimbra, aes(x = Glucose, y = after_stat(density))) + 
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("Glucose") +
  labs(title = "Histogram and Density Plot of Glucose")

glu_hist_h <- ggplot(data = coimbra, aes(x = Glucose, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("Glucose") +
  labs(title = "Histogram and Density Plot of Glucose for Healthy Patients")

glu_hist_p <- ggplot(data = coimbra, aes(x = Glucose, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("Glucose") +
  labs(title = "Histogram and Density Plot of Glucose for Cancer Patients")


glu_hist
glu_hist_h
glu_hist_p


## Insulin

ins_hist <- ggplot(data = coimbra, aes(x = Insulin, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("Insulin") +
  labs("Histogram and Density Plot of Insulin")

ins_hist_h <- ggplot(data = coimbra_h, aes(x = Insulin, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") + 
  geom_density(color = "black", linewidth = 1) + xlab("Insulin") +
  labs("Histogram and Density Plot of Insulin for Healthy Patients")

ins_hist_p <- ggplot(data = coimbra_p, aes(x = Insulin, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("Insulin") + 
  labs("Histogram and Density Plot of Insulin for Cancer Patients")


ins_hist
ins_hist_h
ins_hist_p

## HOMA

hom_hist <- ggplot(data = coimbra, aes(x = HOMA, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") + 
  geom_density(color = "black", linewidth = 1) + xlab("HOMA") +
  labs("Histogram and Density Plot of HOMA")

hom_hist_h <- ggplot(data = coimbra_h, aes(x = HOMA, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("HOMA") +
  labs("Histogram and Density Plot of HOMA for Healthy Patients")

hom_hist_p <- ggplot(data = coimbra_p, aes(x = HOMA, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("HOMA") +
  labs("Histogram and Density Plot of HOMA for Cancer Patients")


hom_hist
hom_hist_h
hom_hist_p

## Leptin

lep_hist <- ggplot(data = coimbra, aes(x = Leptin, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") + 
  geom_density(color = "black", linewidth = 1) + xlab("Leptin") +
  labs("Histogram and Density Plot of Leptin")

lep_hist_h <- ggplot(data = coimbra_h, aes(x = Leptin, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("Leptin") +
  labs("Histogram and Density Plot of Leptin for Healthy Patients")

lep_hist_p <- ggplot(data = coimbra_p, aes(x = Leptin, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("Leptin") +
  labs("Histogram and Density Plot of Leptin for Cancer Patients")

lep_hist
lep_hist_h
lep_hist_p


## Adiponectin

adi_hist <- ggplot(data = coimbra, aes(x = Adiponectin, y = after_stat(density))) + 
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("Adiponectin") +
  labs("Histogram and Density Plot of Adiponectin")

adi_hist_h <- ggplot(data = coimbra_h, aes(x = Adiponectin, y = after_stat(density))) + 
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("Adiponectin") +
  labs("Histogram and Density Plot of Adiponectin for Healthy Patients")

adi_hist_p <- ggplot(data = coimbra_p, aes(x = Adiponectin, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("Adiponectin") +
  labs("Histogram and Density Plot of Adiponectin for Cancer Patients")


adi_hist
adi_hist_h
adi_hist_p

## Resistin

res_hist <- ggplot(data = coimbra, aes(x = Resistin, y = after_stat(density))) + 
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("Resistin") +
  labs("Histogram and Density Plot of Resistin")

res_hist_h <- ggplot(data = coimbra_h, aes(x = Resistin, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("Resistin") +
  labs("Histogram and Density Plot of Resistin for Healthy Patients")

res_hist_p <- ggplot(data = coimbra_p, aes(x = Resistin, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("Resistin") +
  labs("Histogram and Density Plot of Resistin for Cancer Patients")


res_hist
res_hist_h
res_hist_p

## MCP.1

mcp_hist <- ggplot(data = coimbra, aes(x = MCP.1, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("MCP.1") +
  labs("Histogram and Density Plot of MCP-1")

mcp_hist_h <- ggplot(data = coimbra_h, aes(x = MCP.1, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("MCP.1") +
  labs("Histogram and Density Plot of MCP-1 for Healthy Patients")

mcp_hist_p <- ggplot(data = coimbra_p, aes(x = MCP.1, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") + 
  geom_density(color = "black", linewidth = 1) + xlab("MCP.1") +
  labs("Histogram and Density Plot of MCP-1 for Cancer Patients")


mcp_hist
mcp_hist_h
mcp_hist_p

## Ratio

rat_hist <- ggplot(data = coimbra, aes(x = Ratio, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("Adiponectin/Leptin") +
  labs("Histogram and Density Plot of Adiponectin/Leptin")

rat_hist_h <- ggplot(data = coimbra_h, aes(x = Ratio, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("Adiponectin/Leptin") +
  labs("Histogram and Density Plot of Adiponectin/Leptin for Healthy Patients")

rat_hist_p <- ggplot(data = coimbra_p, aes(x = Ratio, y = after_stat(density))) +
  geom_histogram(color = "black", fill = "gray") +
  geom_density(color = "black", linewidth = 1) + xlab("Adiponectin/Leptin") +
  labs("Histogram and Density Plot of Adiponectin/Leptin for Cancer Patients")


rat_hist
rat_hist_h
rat_hist_p

## All
grid.arrange(age_hist, bmi_hist, glu_hist, ins_hist, hom_hist, lep_hist, adi_hist, res_hist, mcp_hist,rat_hist, ncol = 5)

grid.arrange(age_hist_h, bmi_hist_h, glu_hist_h, ins_hist_h, hom_hist_h, lep_hist_h, adi_hist_h, res_hist_h, mcp_hist_h, rat_hist_h, ncol = 2)

grid.arrange(age_hist_p, bmi_hist_p, glu_hist_p, ins_hist_p, hom_hist_p, lep_hist_p, adi_hist_p, res_hist_p, mcp_hist_p, rat_hist_p, ncol = 2)

# Boxplots

coimbraplot <- coimbra
coimbraplot$Classification<-as.factor(coimbraplot$Classification)

## Age

age_plot <- ggplot(data = coimbraplot, aes(x = Classification, y = Age)) +
  geom_boxplot() + ggtitle(label = "Boxplot of Age")

age_plot

## BMI

bmi_plot <- ggplot(data = coimbraplot, aes(x = Classification, y = BMI)) +
  geom_boxplot() + ggtitle(label = "Boxplot of BMI")


bmi_plot

## Glucose

glu_plot <- ggplot(data = coimbraplot, aes(x = Classification, y = Glucose)) +
  geom_boxplot() + ggtitle(label = "Boxplot of Glucose")


glu_plot


## Insulin

ins_plot <- ggplot(data = coimbraplot, aes(x = Classification, y = Insulin)) +
  geom_boxplot() + ggtitle(label = "Boxplot of Insulin")

ins_plot


## HOMA

hom_plot <- ggplot(data = coimbraplot, aes(x = Classification, y = HOMA)) +
  geom_boxplot() + ggtitle(label = "Boxplot of HOMA")

hom_plot


## Leptin

lep_plot <- ggplot(data = coimbraplot, aes(x = Classification, y = Leptin)) +
  geom_boxplot() + ggtitle(label = "Boxplot of Leptin")

lep_plot


## Adiponectin

adi_plot <- ggplot(data = coimbraplot, aes(x = Classification, y = Adiponectin)) +
  geom_boxplot() + ggtitle(label = "Boxplot of Adiponectin")

adi_plot

## Resistin

res_plot <- ggplot(data = coimbraplot, aes(x = Classification, y = Resistin)) +
  geom_boxplot() + ggtitle(label = "Boxplot of Resistin")

res_plot

## MCP.1

mcp_plot <- ggplot(data = coimbraplot, aes(x = Classification, y = MCP.1)) +
  geom_boxplot() + ggtitle(label = "Boxplot of MCP-1")

mcp_plot

## Ratio

rat_plot <- ggplot(data = coimbraplot, aes(x = Classification, y = Ratio)) +
  geom_boxplot() + ggtitle(label = "Boxplot of Adiponectin/Leptin")

rat_plot

## All
grid.arrange(age_plot, bmi_plot, glu_plot, ins_plot, hom_plot, lep_plot, adi_plot, res_plot, mcp_plot, rat_plot, ncol = 2)

# Data Pre-Processing

## Copy
coimbracopy <- coimbra

# Relabel Classification
coimbracopy$Classification <- coimbra$Classification-1

## Discretization
### Age
coimbracopy$Age <- discretize(coimbra$Age,method="fixed",breaks = c(0,40,71,89), labels = c("Low","Middle","High"))

### BMI
coimbracopy$BMI <- discretize(coimbra$BMI, method = "fixed", breaks = c(0,18.5,25,30,50),labels = c("Underweight","Healthy","Overweight","Obesity"))

### Glucose
coimbracopy$Glucose <- discretize(coimbra$Glucose, method = "fixed", breaks = c(0,100,125.9,300), labels = c("Normal","Prediabetes","Diabetes"))

### Insulin
coimbracopy$Insulin <- discretize(coimbra$Insulin, method = "fixed", breaks = c(0,10,25,70), labels = c("Low","Normal","High"))

### HOMA
coimbracopy$HOMA <- discretize(coimbra$HOMA, method = "fixed", breaks = c(0,1,1.9,2.9,30), labels = c("Optimal","Normal","Early_IR","Sig_IR"))

### Leptin
coimbracopy$Leptin <- discretize(coimbra$Leptin, method = "fixed", breaks = c(0,0.5,15.19,100), labels = c("Low","Normal","High"))

### Adiponectin
coimbracopy$Adiponectin <- discretize(coimbra$Adiponectin, method = "fixed", breaks = c(0,5,28.00,50), labels = c("Low","Normal","High"))

### Resistin
coimbracopy$Resistin <- discretize(coimbra$Resistin, method = "fixed", breaks = c(0,7,22,100),labels = c("Low","Middle","High"))

### MCP-1
coimbracopy$MCP.1 <- discretize(coimbra$MCP.1, method = "frequency", breaks = 3, labels = c("Low","Medium","High"))

### Ratio (Adiponectin/Leptin)
coimbracopy$Ratio <- discretize(coimbra$Ratio, method = "fixed", breaks = c(0, 0.5, 1.0 ,10), labels = c("Severe","Moderate","Normal"))


# Analysis

## Divide the Data into Training and Test sStet
set.seed(12345)
ind <- sample(2, nrow(coimbracopy), replace = TRUE, prob = c(0.7,0.3))
train.data <- coimbracopy[ind == 1,]
test.data <- coimbracopy[ind == 2,]

## Logistic Regression
lrmod_1 <- glm(Classification~., family = binomial, data = train.data)
print(lrmod_1)
### Summary for lrmod_1
summary(lrmod_1)

### Confusion Matrix for lrmod_1, training data
table(round(predict(lrmod_1,train.data,type="response")),train.data$Classification)

### Predicted probabilities for test data
pred_lrmod_1 <-round(predict(lrmod_1,test.data,type="response"))

### Confusion Matrix for lrmod_1, testing data
table(pred_lrmod_1,test.data$Classification)

#AUC of Logistic Regression Model, training data
auc(train.data$Classification,round(predict(lrmod_1,train.data,type="response")))

### AUC of Logistic Regression Model,testing data
auc(test.data$Classification,pred_lrmod_1)

### ROC of Logistic Regression Model, training data
roc_lr_train <- roc(train.data$Classification,round(predict(lrmod_1,train.data,type="response")))
plot(roc_lr_train$sensitivities,roc_lr_train$specificities, xlab = "Sensitivity", ylab = "Specificity", type = "b", main = "ROC of Logistion Regression Model: All Attributes, Training Data")

### ROC of Logistic Regression Model, testing data
roc_lr_test <-roc(test.data$Classification,pred_lrmod_1)
plot(roc_lr_test$sensitivities,roc_lr_test$specificities, xlab="Sensitivity", ylab = "Specificity",type ="b",main ="ROC of Logistic Regression Model: All Attributes, Testing Data")

### Minimum Adequate Model
summary(step(lrmod_1))
lrmod_fin  <- glm(Classification ~ Ratio+Age+MCP.1+HOMA+Glucose+BMI+Resistin, family = binomial, data = train.data)
summary(lrmod_fin)

### Confusion Matrix for lrmod_fin, training data
table(round(predict(lrmod_fin,train.data,type="response")),train.data$Classification)
###Predicted Probabilities for test data
pred_lrmod_fin <- round(predict(lrmod_fin,test.data,type="response"))
###Confusion Matrix for lrmod_fin testing data
table(pred_lrmod_fin,test.data$Classification)
### AUC of Final Logistic Regression Model, training data
auc(train.data$Classification,round(predict(lrmod_fin,train.data,type="response")))
### AUC of Final Logistic Regression Model, testing data
auc(test.data$Classification,pred_lrmod_fin)

### ROC of Final Logistic Regression Model, training data
roc_lr_fin_train <- roc(train.data$Classification,round(predict(lrmod_fin,train.data,type="response")))
plot(roc_lr_fin_train$sensitivities,roc_lr_fin_train$specificities, xlab="Sensitivity",ylab="Specificity",type="b",main="ROC of Reduced Logistic Regression Model, Training Data")

### ROC of Final Logistic Regression Model, testing data
roc_lr_fin_test <- roc(test.data$Classification,pred_lrmod_fin)
plot(roc_lr_fin_test$sensitivities,roc_lr_fin_test$specificities, xlab = "Sensitivity",ylab="Specificity",type="b",main="ROC of Reduced Logistic Regression Model, Testing Data")

## Decision Tree

### Traditional
#### Complete Attribute Set
trad_tree <-rpart(formula= Classification~.,data = train.data, method ="class",parms = list(split = "information"))
trad_train_pred<-predict(object=trad_tree,newdata=train.data,type="class")
table(trad_train_pred,as.factor(train.data$Classification))
trad_test_pred<-predict(object=trad_tree,newdata=test.data,type="class")
table(trad_test_pred,as.factor(test.data$Classification))
importance <- varImp(trad_tree, scale = FALSE)
print(importance)

### Conditional Inference, Complete Attibute Set
train.data.tree <- train.data
train.data.tree$Classification<-as.factor(train.data$Classification)
test.data.tree <- test.data
test.data.tree$Classification<-as.factor(test.data$Classification)
con_tree <- ctree(formula = Classification~.,data = train.data.tree)
plot(con_tree)
con_tree_pred <- predict(object=con_tree,newdata=train.data.tree)
table(con_tree_pred,train.data.tree$Classification)
con_test_pred <- predict(object=con_tree,newdata=test.data.tree)
table(con_test_pred,test.data.tree$Classification)

## Naive Bayes
nb <- naiveBayes(Classification~.,train.data.tree)
print(nb)
### Confusion Matrix
table(predict(nb,train.data.tree), train.data.tree$Classification)
table(predict(nb,test.data.tree), test.data.tree$Classification)
### Reduced Attribute Set: Ratio,Age,MCP.1,HOMA,Glucose,BMI,Resistin
nb_red<-naiveBayes(Classification ~ Ratio + Age + MCP.1 + HOMA + Glucose + BMI + Resistin,train.data.tree,laplace = 1)
print(nb_red)
table(predict(nb_red,train.data.tree), train.data.tree$Classification)
table(predict(nb_red,test.data.tree), test.data.tree$Classification)
