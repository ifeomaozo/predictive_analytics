######################################################################
# setup and read files
#######################################################################

#load packages 
rm(list = ls())
list.of.packages <- c("caret", "AppliedPredictiveModeling", "tidyverse", "ggplot2", "purrr",  "stringr", "sp", "rgdal", "raster", "hablar", 
                      "lubridate", "RColorBrewer", "ggpubr", "gridExtra", "data.table",  "nngeo", "reshape2", "doParallel", "caretEnsemble", "corrplot",
                      "wesanderson", "car", "GGally")
lapply(list.of.packages, library, character.only = TRUE) #applying the library function to packages


#set up directories 
WorkDir <-"C:/Users/ido0493/Documents/civis"
ifelse(!dir.exists(file.path(WorkDir, "results")), 
       dir.create(file.path(WorkDir, "results")), FALSE)
ResultDir <- file.path(WorkDir, "results")




#read in files 
files <- list.files(path = WorkDir, pattern = "*.csv", full.names = TRUE)
df <- sapply(files, read_csv, simplify = F)

data_dictionary <- read.delim(file.path(WorkDir, "data_dictionary.txt"))




######################################################################
# Exploratory data analysis 
#######################################################################
#list names
names(df)

#1
weekly_hours_worked_by_sex <- df[[2]] %>%  mutate(sex = ifelse(sex %in% c("F", "female", "FEMALE"), "female",
                                                               ifelse(sex %in% c("M", "male", "MALE"), "male",
                                                                      sex)),
                                                  sex = as.factor(sex)) %>% 
group_by(sex) %>%  summarise(mean_weekly_hours_worked = mean(weekly_hours_worked))
head(weekly_hours_worked_by_sex)

#2
hh_highest_income <- df[[1]] %>% filter(variable =="hh_income") 

hh_highest_per_person_income <- df[[2]] %>%  left_join(hh_income_df) %>%  
  mutate(total_income =  hh_income_pct * hh_income) %>%  group_by(household_id) %>% 
  summarise(per_person_income = sum(total_income)/n()) %>% 
  arrange(-per_person_income) %>% 
  slice(1:3) 
head(hh_highest_per_person_income)


#3 
#create hh income df 
hh_income_df <- df[[1]] %>% filter(variable =="hh_income") 
colnames(hh_income_df)[3]<- "hh_income"
person_highest_income <- df[[2]] %>%  left_join(hh_income_df) %>%  
  mutate(total_income =  hh_income_pct * hh_income) %>%  arrange(-total_income) %>% 
  slice(1:3) %>%  dplyr::select(person_id, total_income)
head(person_highest_income)


#4 
percentile_age_marital_cat <- df[[2]] %>% group_by(marital_status) %>% 
  summarise(percentile_80 = quantile(age, .80))
head(percentile_age_marital_cat)



######################################################################
# Data visualization
#######################################################################

#list names
names(df)
summary(df[[3]])

visual_df<- df[[3]] %>%  dplyr::select(age, total_income, uninsured) %>%  
  mutate(uninsured = case_when(as.factor(uninsured) == "0" ~ "Insured",
                               as.factor(uninsured) == "1" ~ "Uninsured"),
         uninsured = as.factor(uninsured))

summary(visual_df)

p <- ggpairs(visual_df, aes(color = uninsured))+ theme_bw()
# Change color manually.
# Loop through each plot changing relevant scales
for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07")) +
      scale_color_manual(values=c("#00AFBB", "#E7B800", "#FC4E07"))  
  }
}
p
ggsave(paste0(ResultDir, '/', 'scatter_plot_matrix_group.png'), p, width=13, height=13)



######################################################################
# Modeling
#######################################################################

######################################################################
# Data splitting and summary 
#######################################################################

# list of 80% of rows selected for training
validation_index <- createDataPartition(df[[3]]$uninsured, p=0.80, list=FALSE)

#20% for validation
validation <- df[[3]][-validation_index,]


# use the remaining 80% of data to training and testing the models
dataset <- df[[3]][validation_index,]



#recode and summarise data
df_factor <- dataset%>%  mutate_if(is.character, as.factor) %>% 
  mutate_at(vars(starts_with("race")), funs(as.factor)) %>% 
  mutate(sex = ifelse(sex %in% c("F", "female", "FEMALE"), "female",
                      ifelse(sex %in% c("M", "male", "MALE"), "male",
                             sex)),
         sex = as.factor(sex)) %>% 
  mutate(language = case_when(language == "English" ~ "English",
                              language == "Spanish" ~ "Spanish",
                              TRUE ~ "other"),
         uninsured = case_when(as.factor(uninsured) == "0" ~ "Insured",
                               as.factor(uninsured) == "1" ~ "Uninsured"),
         uninsured = as.factor(uninsured),
         language = as.factor(language)) 

summary(df_factor)
sapply(df_factor, class)

#summarise outcome (outcome is unbalan)
percentage <- prop.table(table(df_factor$uninsured)) * 100
cbind(freq=table(df_factor$uninsured), percentage=percentage)


######################################################################
# Data visualization  
#######################################################################
x <- df_factor[,c(2, 5, 6, 7, 8, 9, 10)]
y <- df_factor %>%  dplyr::select_if(is.factor)



# boxplot for each attribute on one image
pdf(file=paste0(ResultDir, "/", "boxplot_var_explore.pdf"))
par(mfrow=c(2,4))
for(i in 1:7) {
  boxplot(x[,i], main=names(x)[i], col ="gold")
}
dev.off()

library(wesanderson)
pdf(file=paste0(ResultDir, "/", "bar_factor_explore.pdf"))
par(mfrow=c(3,2))
for(i in 1:6) {
  plot(y[,i], main=names(y)[i], col=wes_palette(n=5, name="Darjeeling1"))
}
dev.off()

pdf(file=paste0(ResultDir, "/", "bar_factor_explore_2.pdf"))
par(mfrow=c(3,2))
for(i in 7:12) {
  plot(y[,i], main=names(y)[i],  col=wes_palette(n=5, name="Darjeeling1"))
}
dev.off()


pdf(file=paste0(ResultDir, "/", "bar_factor_explore_3.pdf"))
par(mfrow=c(2,2))
for(i in 13:16) {
  plot(y[,i], main=names(y)[i], col=wes_palette(n=5, name="Darjeeling1"))
}
dev.off()


# boxplots 

pdf(file=paste0(ResultDir, "/", "bivariate_continuous_boxplot.pdf"))
featurePlot(x = x[, 1:4], 
            y = y$uninsured, 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(2,2 ), 
            auto.key = list(columns = 4))
dev.off()


pdf(file=paste0(ResultDir, "/", "bivariate_continuous_boxplot_2.pdf"))
featurePlot(x = x[, 5:7], 
            y = y$uninsured, 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(2,2 ), 
            auto.key = list(columns = 4))
dev.off()



# density plots for each attribute by class value
pdf(file=paste0(ResultDir, "/", "bivariate_continuous_density_var_3.pdf"))
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x[, 1:4], y=y$uninsured, plot="density", scales=scales, auto.key = list(columns = 2))
dev.off()


pdf(file=paste0(ResultDir, "/", "bivariate_continuous_density_var_4.pdf"))
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x[, 5:7], y=y$uninsured, plot="density", scales=scales, auto.key = list(columns = 2))
dev.off()


######################################################################
# Other Data Manipulations  
#######################################################################

#create training dataset 
train_df <- df_factor %>% mutate(race = case_when(race_white == "1" ~ "_white",
                                                  race_black == "1" ~ "_black",
                                                  TRUE ~ "other")) %>% 
  mutate(marital_status = case_when(marital_status == "divorced" ~ "divorced_or_seperated",
                                    marital_status == "separated" ~ "divorced_or_seperated", 
                                    TRUE ~ as.character(marital_status))) %>% 
  mutate(school_status  = case_when(school_status == "public_school" ~ "private or public",
                                    school_status == "private_school" ~ "private or public",
                                    TRUE ~ as.character(school_status))) %>% 
  dplyr::select(-contains("race_"))

#dummify factors 
train_df_ <- dummyVars(" ~.", data =train_df)
train_df_ <- data.frame(predict(train_df_, newdata = train_df)) 

train_df_ <- train_df_ %>% dplyr::select(-c(uninsured.Insured, uninsured.Uninsured, household_id,  person_id, nativity_status.foreign_born, sex.male,
                                            citizen_status.citizen_birth,marital_statusmarried, school_statusnot_student,
                                            when_last_worked.in_last_year, worked_last_week.worked, language.English, race_white)) #removed nativity status and other income since it can be predicted by other variables

train_df_$uninsured <- df_factor$uninsured #add back the uninsured outcome 


nZrv <-nearZeroVar(train_df_) #check for zero or near zero variance predictors 

# check missing 
p_missing <- unlist(lapply(dataset, function(x) sum(x == "Missing")))/nrow(train_df_)  
v <- data.frame(sort(p_missing[p_missing > 0], decreasing = TRUE))# no missing data


#calculate cookd's distance 

cooksd <- cooks.distance(glm(uninsured ~ ., 
                             family = "binomial", 
                             data = train_df_))


plot(cooksd, 
     pch="*", 
     cex=2, 
     main="Influential Obs by Cooks distance") 
abline(h = 4*mean(cooksd, na.rm=T), col="red")

#check and remove
outliers <- rownames(train_df_[cooksd > 4*mean(cooksd, na.rm=T), ])
print(outliers)
out <- as.numeric(outliers)
df_sensitivity <- train_df_[-out, ]


#check correlations
corr_df <- train_df_ %>% dplyr::select(where(is.numeric))
uninsuredCor <-  cor(corr_df)
pdf(file=paste0(ResultDir, "/", "correlation_all_cont_var.pdf"))
corrplot(uninsuredCor, order = "hclust")
dev.off()
sum((uninsuredCor > 0.75 | uninsuredCor < -0.75) & uninsuredCor < 1) / (25*25)# zero correlations greater than 50%


# dataset without wage_income (colinear with total income ) and race_recoded 
train_df_glm<- train_df_ %>%dplyr::select(-c(wage_income, nativity_status.native_born)) #removed nativity status and wage income due to colinearity/multicolinearity

train_df_glm$uninsured <- factor(train_df_glm$uninsured, levels =c("Uninsured", "Insured"))

######################################################################
# Modeling
#######################################################################

# initial simple glm 
summary(train_df_glm)
sapply(train_df_glm, class)

myModel <- glm(uninsured~.,data = train_df_glm, family = "binomial")
summary(myModel)
car::vif(myModel)
alias(glm(uninsured~.,data = train_df_glm, family = "binomial"))

#drop colinear variables, rerun and check colinearity 

# add interaction?
# dmy <- dummyVars(" ~total_income:race", data = dataset_v3)
# trsf <- data.frame(predict(dmy, newdata = dataset_v3)) 
# summary(trsf)
# dataset_v3<-cbind(dataset_v3, trsf)

#prediction model
registerDoParallel(3)
getDoParWorkers()
set.seed(123)
my_ctrl <- trainControl(method = "cv", 
                        number = 5,
                        classProbs = TRUE,
                        savePredictions = "final",
                        index = 
                          createResample(train_df_glm$uninsured, 3),
                        sampling = "up",
                        allowParallel = TRUE)

model_list <- caretList(uninsured ~ .,
                        data = train_df_glm,
                        methodList = c("glm", "nb"),
                        metric = "Kappa",
                        tuneList = NULL,
                        continue_on_fail = FALSE,  
                        preProcess = c("BoxCox", "center", "scale"),
                        trControl = my_ctrl)

summary(model_list$glm)
summary(model_list$nb)

# model comparisons 

resamp<-resamples(list(logistic = model_list$glm, NB = model_list$nb))
summary(resamp)
?xyplot.resamples

ggplot(resamp, models = resamp$models[1], metric = resamp$metrics[1])
resamp$models
modelDifferences<- diff(resamp)
summary(modelDifferences) # p-value suggests a small difference in performance with logistic model doing better 




######################################################################
# validation 
#######################################################################

valid_dataset <- validation %>%  mutate_if(is.character, as.factor) %>% 
  #mutate_at(vars(starts_with("race")), funs(as.factor)) %>% 
  mutate(sex = ifelse(sex %in% c("F", "female", "FEMALE"), "female",
                      ifelse(sex %in% c("M", "male", "MALE"), "male",
                             sex)),
         sex = as.factor(sex)) %>% 
  mutate(language = case_when(language == "English" ~ "English",
                              language == "Spanish" ~ "Spanish",
                              TRUE ~ "other"),
         uninsured = case_when(as.factor(uninsured) == "0" ~ "Insured",
                               as.factor(uninsured) == "1" ~ "Uninsured"),
         uninsured = as.factor(uninsured),
         language = as.factor(language)) %>% 
  mutate(race = case_when(race_white == "1" ~ "_white",
                          race_black == "1" ~ "_black",
                          TRUE ~ "other")) %>% 
  mutate(marital_status = case_when(marital_status == "divorced" ~ "divorced_or_seperated",
                                    marital_status == "separated" ~ "divorced_or_seperated", 
                                    TRUE ~ as.character(marital_status))) %>% 
  mutate(school_status  = case_when(school_status == "public_school" ~ "private or public",
                                    school_status == "private_school" ~ "private or public",
                                    TRUE ~ as.character(school_status))) %>% 
  dplyr::select(-contains("race_")) 



#dummify factors 
valid_dataset_ <- dummyVars(" ~.", data =valid_dataset)
valid_dataset_ <- data.frame(predict(valid_dataset_, newdata = valid_dataset)) 

valid_dataset_ <- valid_dataset_ %>% dplyr::select(-c(uninsured.Insured, uninsured.Uninsured, household_id,  person_id, nativity_status.foreign_born, sex.male,
                                                      citizen_status.citizen_birth,marital_statusmarried, school_statusnot_student,
                                                      when_last_worked.in_last_year, worked_last_week.worked, language.English, race_white, wage_income,
                                                      nativity_status.native_born)) 

valid_dataset_$uninsured <- valid_dataset$uninsured #add back the uninsured outcome 

valid_dataset_$uninsured <- factor(valid_dataset_$uninsured, levels =c("Uninsured", "Insured"))
nZrv <-nearZeroVar(valid_dataset_) #check for zero or near zero variance predictors 


######################################################################
# Model performance 
#######################################################################
names(valid_dataset_)
names(train_df_glm)
confusionMatrix(predict(model_list$glm,valid_dataset_, type = "raw"), valid_dataset_$uninsured)
confusionMatrix(predict(model_list$nb,valid_dataset_, type = "raw"), valid_dataset_$uninsured)

valid_dataset_$uninsured <- NULL #remove the uninsured outcome 

pred_prob <-predict(model_list$glm,valid_dataset_, type = "prob")

valid_dataset_prob <- cbind(valid_dataset_, pred_prob) %>%  filter(uninsured == "Uninsured")

uninsured_plot <- ggplot(valid_dataset_prob, aes(x=valid_dataset_prob$pred_prob$Uninsured))+
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  theme_minimal()+
  labs(title = "True Outcome: Uninsured")+
  xlab("Predicted Probabilities of being Uninsured from Logistic Model")

valid_dataset_prob_2 <- cbind(valid_dataset_, pred_prob) %>%  filter(uninsured == "Insured")
insured_plot <- ggplot(valid_dataset_prob_2, aes(x =valid_dataset_prob_2$pred_prob$Insured))+
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  theme_minimal()+
  labs(title = "True Outcome: Insured")+
  xlab("Predicted Probabilities of being Ininsured from Logistic Model")

logistic_perform <-  ggarrange(uninsured_plot, insured_plot, ncol = 1, nrow = 2)



######################################################################
# unlabeled data 
#######################################################################

test_dataset <- df[[4]]%>%  mutate_if(is.character, as.factor) %>% 
  mutate(sex = ifelse(sex %in% c("F", "female", "FEMALE"), "female",
                      ifelse(sex %in% c("M", "male", "MALE"), "male",
                             sex)),
         sex = as.factor(sex)) %>% 
  mutate(language = case_when(language == "English" ~ "English",
                              language == "Spanish" ~ "Spanish",
                              TRUE ~ "other"),
         language = as.factor(language)) %>% 
  mutate(race = case_when(race_white == "1" ~ "_white",
                          race_black == "1" ~ "_black",
                          TRUE ~ "other")) %>% 
  mutate(marital_status = case_when(marital_status == "divorced" ~ "divorced_or_seperated",
                                    marital_status == "separated" ~ "divorced_or_seperated", 
                                    TRUE ~ as.character(marital_status))) %>% 
  mutate(school_status  = case_when(school_status == "public_school" ~ "private or public",
                                    school_status == "private_school" ~ "private or public",
                                    TRUE ~ as.character(school_status))) %>% 
  dplyr::select(-contains("race_")) 

summary(test_dataset)

#dummify factors 
test_dataset_ <- dummyVars(" ~.", data =test_dataset)
test_dataset_ <- data.frame(predict(test_dataset_, newdata = test_dataset)) 

test_dataset_ <- test_dataset_ %>% dplyr::select(-c(household_id,  person_id, nativity_status.foreign_born, sex.male,
                                                      citizen_status.citizen_birth,marital_statusmarried, school_statusnot_student,
                                                      when_last_worked.in_last_year, worked_last_week.worked, language.English, race_white, wage_income,
                                                      nativity_status.native_born)) 




nZrv <-nearZeroVar(test_dataset_) #check for zero or near zero variance predictors 


pred_prob_test <-predict(model_list$glm,test_dataset_, type = "prob")

pred_prob_test$person_id <- test_dataset$person_id
colnames(pred_prob_test)[1]<- "score_uninsured"
colnames(pred_prob_test)[2]<- "score_insured"

write.csv(pred_prob_test, file.path(ResultDir, "part3_scores.csv"))
































