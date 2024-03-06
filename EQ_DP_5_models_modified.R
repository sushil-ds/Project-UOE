#Clear Environment
rm(list = ls())

# library(pacman) install pacman if not installed

sessionInfo() #This tells you all the whereabouts regarding your environment and installed version.
options(max.print=100000) #Displays full output

##Setting up the working Directory 
setwd("C:/Users/44777/Documents/Earthquake_dmg_Prediction/")
getwd()

# install.packages(c("zoo","xts","quantmod"))
# install.packages( "C:/Users/44777/Downloads/DMwR_0.4.1.tar.gz", repos=NULL, type="source")

#Install required libraries:
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr, ggpubr, missForest, Amelia, DataExplorer,
               caret,intervals,gstat,UBL,smotefamily,rpart,nnet,funModeling,
               xgboost,Ckmeans.1d.dp,scutr,ranger,MASS,randomForest,kernlab,mlbench)


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
# Compute the frequency

EQ_P_df <- df_new %>%
  group_by(damage_grade) %>%
  summarise(counts = n())

library(RColorBrewer)
# Create the bar plot. Use theme_pubclean()
cbPalette <- c("#999999", "#E69F00", "#56B4E9")
p<-ggplot(EQ_P_df, aes(x = damage_grade, y = counts,fill= damage_grade)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(label = counts), vjust = -0.6) +
  labs(title = "Distribution of damage grades in Population")

p_custom<-p+scale_fill_brewer(palette="Greens") + theme_minimal()
  



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


# Lets factorize categorical variable

strat_sample$land_surface_condition  <- as.factor(strat_sample$land_surface_condition)
strat_sample$foundation_type  <- as.factor(strat_sample$foundation_type)
strat_sample$roof_type  <- as.factor(strat_sample$roof_type)
strat_sample$ground_floor_type  <- as.factor(strat_sample$ground_floor_type)
strat_sample$other_floor_type  <- as.factor(strat_sample$other_floor_type)
strat_sample$position  <- as.factor(strat_sample$position)
strat_sample$plan_configuration  <- as.factor(strat_sample$plan_configuration)

# Lets work with hypothesis:

# 1.What are the sites that have an influence on the building's damage level?

#Plot Histogram to determine the distribution Frequency of Target variable in sample

ggplot(strat_sample, aes(Town_City)) +
  geom_bar(fill = "#FF6666") + labs(title = "Distribution of Geo_id_1")+
  theme_pubclean()
ggplot(strat_sample, aes(Village)) +
  geom_bar(fill = "#FF6666") + labs(title = "Distribution of Geo_id_2")+
  theme_pubclean()
ggplot(strat_sample, aes(Streets)) +
  geom_bar(fill = "#FF6666") + labs(title = "Distribution of Geo_id_3")+
  theme_pubclean()
  
# 2.What is the minimum age or age group of a severely damaged building?

ggplot(strat_sample, aes(age)) +
  geom_bar(fill = "#FF6666") + labs(title = "Distribution of Age")+
  theme_pubclean()

# 3.What portion of families live in severely damaged structures?

ggplot(strat_sample, aes(count_families)) +
  geom_bar(fill = "#FF6666") + labs(title = "Distribution of families")+
  theme_pubclean()

# 4.Does the percentage of area have an effect on extreme damage?

ggplot(strat_sample, aes(AP_int)) +
  geom_bar(fill = "#FF6666") + labs(title = "Distribution of Area percentage")+
  theme_pubclean()

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
impdata<-data.frame(strat_sample$Town_City,strat_sample$Village,strat_sample$Streets,
                    strat_sample$foundation_type,strat_sample$roof_type,
                    strat_sample$ground_floor_type,strat_sample$other_floor_type,
                    strat_sample$position,strat_sample$plan_configuration,
                    strat_sample$count_families,CF_int,A_int,AP_int,HP_int,
                    strat_sample$building_id,strat_sample$land_surface_condition,
                    strat_sample$damage_grade_fac)

data_to_imp<-rename(impdata,
                    "Town_City" = "strat_sample.Town_City",
                    "Village" = "strat_sample.Village",
                    "Streets" = "strat_sample.Streets",
                    "Roof_type" = "strat_sample.roof_type",
                    "Ground_floor" = "strat_sample.ground_floor_type",
                    "Foundation" = "strat_sample.foundation_type",
                    "Other_floor" = "strat_sample.other_floor_type",
                    "Position" = "strat_sample.position",
                    "Plan" = "strat_sample.plan_configuration",
                    "Land_surface_condition" = "strat_sample.land_surface_condition",
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
mice_plot <- aggr(data_to_imp, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(data_to_imp), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

#Use MICE to impute missing values.
miceData <- mice(data_to_imp,m=5,maxit=50,meth='cart',seed=500)
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

set.seed(1234)
EQ_shuffle_index <- EQ_Data[sample(1:nrow(EQ_Data)),]
head(EQ_shuffle_index)

dv <- caret::dummyVars(" ~Foundation+Roof_type+Ground_floor+Other_floor+Position+Plan+Land_surface_condition ", data = EQ_shuffle_index)
new_df <- data.frame(predict(dv,EQ_shuffle_index))
# head(new_df[, 3:9])
dim(new_df)
str(new_df)
head(new_df)

EQ_end<-cbind(EQ_shuffle_index$Town_City,EQ_shuffle_index$Village,EQ_shuffle_index$Streets,new_df,
              EQ_shuffle_index$damage_grade,EQ_shuffle_index$CF_int,EQ_shuffle_index$A_int,
              EQ_shuffle_index$count_families,EQ_shuffle_index$AP_int,EQ_shuffle_index$HP_int)

EQ_ML<-rename(EQ_end,
                    "Town_City" = "EQ_shuffle_index$Town_City",
                    "Village" = "EQ_shuffle_index$Village",
                    "Streets" = "EQ_shuffle_index$Streets",
                    "CF_int" = "EQ_shuffle_index$CF_int",
                    "A_int" = "EQ_shuffle_index$A_int",
                    "AP_int" = "EQ_shuffle_index$AP_int",
                    "HP_int" = "EQ_shuffle_index$HP_int",
                    "count_families" = "EQ_shuffle_index$count_families",
                    "damage_grade" = "EQ_shuffle_index$damage_grade"
                   )


EQ <- EQ_ML

trainIndex <- createDataPartition(EQ$damage_grade, p = .75, list = FALSE, times = 1)
train <- EQ[ trainIndex,]
val  <- EQ[-trainIndex,]
dim(train)
dim(val)

# Correlation for all numerical Variables
EQ_cor = cor(EQ %>%
             select_if(is.numeric))

corrplot(EQ_cor)

library("Hmisc")
EQ_rcorr <- rcorr(as.matrix(EQ))

EQ_coeff = EQ_rcorr$r
EQ_p = EQ_rcorr$P

# GGally::ggcorr(x %>%
#                select_if(is.numeric),
#                label = "Correlation_Matrix")

x <- EQ[,-10]
y <- EQ[,10]
dim(x)
length(y)

# # Multivariate plots
# featurePlot(x=x, y=y, plot="box")
# 
# #Density Plots by class value
# scales <- list(x=list(relation="free"), y=list(relation="free"))
# featurePlot(x=x, y=y, plot="density", scales=scales)

#Test Harness:
#Run algorithms using 5-fold cross validation
control <- trainControl(method="cv", number=5)
metric <- "Accuracy"
metric

# Build Models

# linear Discrimant Analysis
set.seed(1234)
lda_fit <- train(damage_grade~., data=EQ, method="lda", metric=metric, trControl=control)

# CART
set.seed(1234)
cart_fit <- train(damage_grade~., data=EQ, method="rpart", metric=metric, trControl=control)

# KNN
set.seed(1234)
knn_fit <- train(damage_grade~., data=EQ, method="knn", metric=metric, trControl=control)

# SVM
set.seed(1234)
svm_fit <- train(damage_grade~., data=EQ, method="svmRadial", metric=metric, trControl=control)

# RF
set.seed(1234)
rf_fit <- train(damage_grade~., data=EQ, method="rf", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(lda=lda_fit, cart=cart_fit, knn=knn_fit, svm=svm_fit, rf=rf_fit))
summary(results)

# compare accuracy of models
dotplot(results)

# estimate skill of RF on the validation dataset
predictions <- predict(rf_fit, val)
confusionMatrix(predictions, val$damage)


# NEED FOR SMOTE:
table(train$damage_grade)
train_EQ_dmg_grade_imb <- train[train$damage_grade %in% c(1,2,3), ]
str(train_EQ_dmg_grade_imb)
train_EQ_dmg_grade_imb$damage_grade <- as.numeric(train_EQ_dmg_grade_imb$damage_grade)
str(train_EQ_dmg_grade_imb)
table(train_EQ_dmg_grade_imb$damage_grade)

train_scutted <- SCUT(train_EQ_dmg_grade_imb, "damage_grade", undersample = undersample_kmeans,
                      usamp_opts = list(k=5))
str(train_scutted)
table(train_scutted$damage_grade)

# _______________________________________

# X <- c("low", "medium", "extreme") #character objects need quotes

# Compute the frequency

EQ_df <- train_scutted %>%
  group_by(damage_grade) %>%
  summarise(counts = n())
damage_grade

# Create the bar plot. Use theme_pubclean()
ggplot(EQ_df, aes(x = damage_grade, y = counts)) +
  geom_bar(fill = "#FF6666", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

# ______________________________________________

# SMOTED Build model
train_scutted$damage_grade<-as.factor(train_scutted$damage_grade)
val$damage_grade<-as.factor(val$damage_grade)
rf_fit_sm<-train(damage_grade~.,data = train_scutted,method="rf",metric = metric ,trControl=control,summaryFunction=multiClassSummary)
predictors_sm<-names(train_scutted)[names(train_scutted)!='damage_grade']
# val$damage_grade <- as.numeric(val$damage_grade)

# estimate skill of RF after smote on the validation dataset
predictions_sm <- predict(rf_fit_sm, val)
str(predictions_sm)

confusionMatrix(predictions_sm, val$damage_grade, mode = "everything")


# To find ROC
library(pROC)

predictions_sm<-as.numeric(predictions_sm)
val$damage_grade<-as.numeric(val$damage_grade)
auc<-multiclass.roc(predictions_sm, val$damage_grade)
auc

rs <- auc[['rocs']]
plot.roc(rs[[1]])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))

# Clear packages
# p_unload(all)  # Remove all add-ons
# detach("package:", unload = TRUE)

