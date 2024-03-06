#Clear Environment
rm(list = ls())

# library(pacman) install pacman if not installed

sessionInfo() #This tells you all the whereabouts regarding your environment and installed version.
options(max.print=100000) #Displays full output

# install.packages(c("zoo","xts","quantmod"))
# install.packages( "C:/Users/44777/Downloads/DMwR_0.4.1.tar.gz", repos=NULL, type="source")

#Install required libraries:
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr, ggpubr, missForest, Amelia, DataExplorer,
               caret,intervals,gstat,UBL,smotefamily,rpart,nnet,funModeling,xgboost,Ckmeans.1d.dp,scutr,ranger)

# Clear packages
# p_unload(all)  # Remove all add-ons
# detach("package:", unload = TRUE)

#Setting up the working Directory 
setwd("C:/Users/44777/Documents/Earthquake_dmg_Prediction/")
getwd()

#Read csv file from source
df<- read.csv("Earthquake_Damage.csv")

# Details regarding the dataset
head(df)
summary(df)
str(df)

# Data Wrangling
df_new <- rename (df,
                  "S_id" = X,
                  "Town_City" = geo_level_1_id,
                  "Village" = geo_level_2_id,
                  "Streets" = geo_level_3_id,
)




# Frequency of Target variable in Population 
table(df_new$damage_grade)
#Plot Histogram to determine the distribution Frequency of Target variable in Population
ggplot(df_new, aes(damage_grade)) +
  geom_bar(fill = "#FF6666") +
  theme_pubclean()

# obtain stratified sample with 20 % of entire population.
strat_sample <- df_new %>%
  group_by(damage_grade) %>%
  sample_frac(size=.20)
# Frequency of Target variable in sample 
table(strat_sample$damage_grade)
#Plot Histogram to determine the distribution Frequency of Target variable in sample
ggplot(strat_sample, aes(damage_grade)) +
  geom_bar(fill = "#FF6666") +
  theme_pubclean()

#Smoothed Density estimate plot of each class variable in sample
strat_sample$damage_grade_fac <- as.factor(strat_sample$damage_grade)
ggplot(strat_sample, aes(x = damage_grade_fac, colour = damage_grade_fac)) + 
  geom_density(aes(group = damage_grade_fac, fill = damage_grade_fac), alpha = 0.3) +
  labs(title = "Distribution of each Type")

# Check for Missing Values
nrow(strat_sample[!complete.cases(strat_sample),])

#Generate random missing values in columns count of floors, age, area percentage and height percentage in sample.
ss_mis_c_a_ap_hp_org <- prodNA(strat_sample[,6:9], noNA = 0.005)
ss_mis_c_a_ap_hp<-rename(ss_mis_c_a_ap_hp_org,
                         "CF" = count_floors_pre_eq,
                         "A" = age,
                         "AP" = area_percentage,
                         "HP" = height_percentage,
)

# Convert the datatype to int
CF_int<-as.integer(ss_mis_c_a_ap_hp_org$count_floors_pre_eq)
A_int<-as.integer(ss_mis_c_a_ap_hp_org$age)
AP_int<-as.integer(ss_mis_c_a_ap_hp_org$area_percentage)
HP_int<-as.integer(ss_mis_c_a_ap_hp_org$height_percentage)


#determine the percentage of missing values in missing values generated column in sample
DataExplorer::profile_missing(ss_mis_c_a_ap_hp)

#Create a new dataframe with needed features.
impdata<-data.frame(strat_sample$Town_City,strat_sample$Village,strat_sample$Streets,strat_sample$count_families,CF_int,A_int,AP_int,HP_int,strat_sample$building_id,strat_sample$damage_grade_fac)

data_to_imp<-rename(impdata,
                    "Town_City" = "strat_sample.Town_City",
                    "Village" = "strat_sample.Village",
                    "Streets" = "strat_sample.Streets",
                    "count_families" = "strat_sample.count_families",
                    "building_id" = "strat_sample.building_id",
                    "damage_grade" = "strat_sample.damage_grade_fac"
)








#determine the percentage of missing values in missing values generated column in sample
DataExplorer::profile_missing(data_to_imp)

#impute values with MICE
set.seed(12345)
library(mice)

#Get the pattern of Data
md.pattern(data_to_imp)

#Plot to get missing values in percentages. 
library(VIM)
aggr_plot <- aggr(data_to_imp, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data_to_imp), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#Use MICE to impute missing values.
miceData <- mice(data_to_imp,m=5,maxit=50,meth='pmm',seed=500)
summary(miceData)


#check mice imputation on CF_int column.
miceData$imp$CF_int

#Find the method of imputation algorithm
miceData$meth


#Obtain the completed dataset after imputation.
EQ_Data <- complete(miceData,1)

#Check for percentage of missing values in complete dataset after miceimputation
DataExplorer::profile_missing(EQ_Data)

#Determine levels of target variable
levels(EQ_Data$damage_grade)

#Exploratory data analysis, data preparation and model performance of mice imputed EQ dataset
funModeling::df_status(EQ_Data)


# From the head and tail output, you can notice the data is not shuffled. This is a big issue! When you will split your data between a train set and test set, you will select only the damage grade from class 1 and 2 (No damage grade from class 3 are in the top 80 percent of the observations), which means the algorithm will never see the features of damage grade of class 3. This mistake will lead to poor prediction.
shuffle_index <- sample(1:nrow(EQ_Data))
head(shuffle_index)

EQ <- EQ_Data[shuffle_index, ]


# Label encoding and Train Test Split
set.seed(1234)
EQ$damage_grade<-as.integer(EQ$damage_grade)-1
splitIndex<-createDataPartition(EQ$damage_grade, p = 0.70,list = FALSE, times =1)

trainSplit<-EQ[splitIndex,]
testSplit<-EQ[-splitIndex,]

table(trainSplit$damage_grade)
table(testSplit$damage_grade)

prop.table(table(trainSplit$damage_grade))
prop.table(table(testSplit$damage_grade))


# Build model

ctrl<-trainControl(method='cv',number = 5)
tbmodel<-train(damage_grade~.,data = trainSplit,method = 'treebag',
               trControl = ctrl)





predictors<-names(trainSplit)[names(trainSplit)!='damage_grade']
pred<-predict(tbmodel$finalModel, testSplit[,predictors])

library(pROC)
auc<-multiclass.roc(testSplit$damage_grade,pred)
auc


# NEED FOR SMOTE:
table(trainSplit$damage_grade)
trainSplit_EQ_dmg_grade_imb <- trainSplit[trainSplit$damage_grade %in% c(0,1,2), ]
trainSplit_EQ_dmg_grade_imb$damage_grade <- as.numeric(trainSplit_EQ_dmg_grade_imb$damage_grade)

table(trainSplit_EQ_dmg_grade_imb$damage_grade)


train_scutted <- SCUT(trainSplit_EQ_dmg_grade_imb, "damage_grade", undersample = undersample_kmeans,
                      usamp_opts = list(k=5))

table(train_scutted$damage_grade)

# SMOTED Build model
ctrl<-trainControl(method='cv',number = 5)

tbmodel_sm<-train(damage_grade~.,data = train_scutted,method = 'treebag',
                  trControl = ctrl)

predictors_sm<-names(train_scutted)[names(train_scutted)!='damage_grade']
testSplit$damage_grade <- as.numeric(testSplit$damage_grade)
pred_sm<-predict(tbmodel_sm$finalModel, testSplit[,predictors_sm])

dmg_pred_sm <- ranger(
  damage_grade ~ .,
  data = train_scutted,
  save.memory = TRUE, 
  probability = FALSE
)

probabilitiesDmgGrade <- predict(
  dmg_pred_sm,
  data = testSplit,
  verbose = TRUE
)
str(probabilitiesDmgGrade)

table(probabilitiesDmgGrade$predictions,
      testSplit$damage_grade
) %>% confusionMatrix()

# ROC_AUC curve
auc_sm<-multiclass.roc(testSplit$damage_grade,pred_sm)
auc_sm

str(testSplit$damage_grade)
str(pred_sm)
confusionMatrix(testSplit$damage_grade,type = 'raw')
?confusionMatrix




# Label Encoding Conversion since we deal 
dmg_grade<-EQ_Data$damage_grade
label <-as.integer(EQ_Data$damage_grade)-1
EQ_Data$damage_grade = NULL

#Split Dataframe into Train and Test
n = nrow(EQ_Data)
train.index = sample(n,floor(0.75*n))
train.data = as.matrix(EQ_Data[train.index,])
train.label = label[train.index]
test.data = as.matrix(EQ_Data[-train.index,])
test.label = label[-train.index]

# Data Modelling with MLM 
set.seed(123)
formula <- "damage_grade ~ AP_int+A_int+CF_int+HP_int"
mod <- multinom(formula, data=EQ_Data)
output <- summary(mod)
print(output)


# Transform the two data sets into xgb.Matrix
xgb.train = xgb.DMatrix(data=train.data,label=train.label)
xgb.test = xgb.DMatrix(data=test.data,label=test.label)

num_class = length(levels(dmg_grade))
params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

# Train the XGBoost classifer
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=10000,
  nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=0
)

xgb.fit

# Predict outcomes with the test data
xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = levels(dmg_grade)

# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = levels(dmg_grade)[test.label+1]

# Calculate the final accuracy
result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))

table(Prediction = prediction$max_prob, Actual = prediction$label)

prediction <- matrix(pred, nrow = nc, ncol = length(pred)/nc) %>%
  t() %>%
  data.frame() %>%
  mutate(label = , max_prob = max.col(., "last")-1)

# Hypothesis :
# What are locations that has impact with damge level of building?
# H0:Geo ids have inpact towards damage level of building.
# H1:Geo ids doesnot have inpact towards damage level of building.







#-------------------------------------------------------------

#Inspecting the distribution of original values and imputed values in missing data imputed columns
xyplot(miceData,damage_grade ~ AP_int+A_int+CF_int+HP_int,pch=18,cex=1)
xyplot

#Density plot
densityplot(miceData)

# Determine the missing value count now
(na_count_full_imputed <-data.frame(sapply(completedData, function(y) sum(length(which(is.na(y)))))))

diabetes_clean2$Diabetes <- as.factor(diabetes_clean2$Diabetes)

# Pooling
# modelFit1 <- with(miceData,multinom(damage_grade ~ AP_int+A_int+CF_int+HP_int))
# summary(modelFit1)
# pool_mice <- pool(modelFit1)






