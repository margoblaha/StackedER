library(mlr3)
library(mlr3pipelines)
library(mlr3tuning)
library(mlr3viz)
library(mlr3extralearners) # remotes::install_github("mlr-org/mlr3extralearners@*release")
library(mlr3learners)
library(mlr3fselect)
library(paradox)
library(data.table)
library(dplyr)
library(lubridate)
library(skimr)
library(Cubist)

setwd("~/Dati/")
out.dir <- "~/Dati/sensitivity_analysis_outputs/"

# ---- 1. Load observations ----
bio.matrix <- readRDS("~/Dati/Output/bio_matrix.RDS")
bio.matrix <- bio.matrix %>%
  mutate(externalVal = ifelse(year > 2022, 1, 0), 
         internalVal = ifelse(year < 2022, 1, 0)) %>% 
  arrange(ID) %>% 
  as_tibble() %>% 
  group_by(ID) %>% 
  mutate(value.lag1 = dplyr::lag(value, n=1, order_by = ID), # add lagged eggs value as proxy of auto regressive behaviour
         year = lubridate::year(date))

head(bio.matrix)
names(bio.matrix)
range(bio.matrix$year)

table(bio.matrix$year, bio.matrix$externalVal)

# ---- 2. Modelling ----
Autoregr_model = 0 # add an autoregressive component to the model (1: Yes; 0: No)

if(Autoregr_model == 1) {
  vars <- unique(make.names(unlist(sapply(c("value.lag1", 
                                            "med_TAVG.lag2", "med_TAVG.lag3",
                                            "sum_PREC.lag2", "sum_PREC.lag3",
                                            # "med_RHAVG.lag2", "med_RHAVG.lag3",
                                            "medianPhotoweek.lag2", "medianPhotoweek.lag3",
                                            "S1.52", "C1.52", "S2.52", "C2.52", "LC_Value"), function(i){names(bio.matrix)[grep(i, names(bio.matrix))]}))))
} else {
  vars <- unique(make.names(unlist(sapply(c("med_TAVG.lag2", "med_TAVG.lag3",
                                            "sum_PREC.lag2", "sum_PREC.lag3",
                                            # "med_RHAVG.lag2", "med_RHAVG.lag3",
                                            "medianPhotoweek.lag2", "medianPhotoweek.lag3",
                                            "S1.52", "C1.52", "S2.52", "C2.52", "LC_Value"), function(i){names(bio.matrix)[grep(i, names(bio.matrix))]}))))
}

formula <- as.formula(paste('value ~ ', paste(vars, collapse = "+")))
r.sel <- stats::complete.cases(bio.matrix[,all.vars(formula)])

df.s <- bio.matrix[which(r.sel),c(all.vars(formula),"ID", "year", "date",  "Country", "Region", "internalVal")]

# define external validationd dataset
test.exdf <- df.s %>% 
  filter(year==2023)
date.extest <- test.exdf[,c("ID", "Country", "Region", "date", "internalVal")]
id.extest <- test.exdf$ID
test.exdf <- test.exdf %>%
  select(-ID, -year, -date, -Region, -Country, -internalVal) 
test.exdf <- test.exdf[,-1] 

#define dataframe from which subset are taken from
df.s <- df.s %>% 
  filter(year<2023)

#define different levels
myYears<-list(2011:2022#,
              # 2019:2022,
              # 2015:2022,
              # 2011:2022
              )

myFrac<- c(
  #0.1, 0.25, 0.50, 0.75, 
  0.9)

myReps <- 5

# start looping ...
for(y in 1:length(myYears)){
   # y=1
  
   df.y <- df.s %>% 
     filter(year %in% myYears[[y]])
   # range(df.y$year)
   
   for(f in 1:length(myFrac)){
     # f=1
     
     for(i in 1:myReps){
       # i=1
       message("Processing years ", paste(range(myYears[[y]]), collapse = "-"), " with data fraction ", myFrac[f], " and iter ", i   )
       ID <- unique(df.y$ID)
       # set.seed(123)
       sample <- sample(ID, size = myFrac[f] * length(ID))
       # subsample <- df.y[df.y$ID %in% sample, ]
       
       # ---- 3. Subset the dataset into train and test df ----
       ## train df
       train.df <-  df.y[df.y$ID %in% sample, ] 
       date.train <- train.df[,c("ID", "Country", "Region", "date", "internalVal")]  
       id <- train.df$ID
       train.df <- train.df %>% 
         select(-ID, -year, -date, -Region, -Country, -internalVal)
       #train.df <- train.df[,-1] # ugly but functional solution to avoid the grouping variable
       
       
       ## internal test df
       test.df <-  df.y[!df.y$ID %in% sample, ] 
       date.test <- test.df[,c("ID", "Country", "Region", "date", "internalVal")]
       id.test <- test.df$ID
       test.df <- test.df %>%
         select(-ID, -year, -date, -Region, -Country, -internalVal) 
       #test.df <- test.df[,-1] 
       
       if(nrow(test.df) + nrow(train.df) == nrow(df.y)){message("Matrix dimensions check: Allright")}else{stop()} # check
       
       # ---- 4. Create task + Codice (ID) for grouping ----
       #define response variable
       #tv <- all.vars(formula)[1]
       task <- mlr3::as_task_regr(train.df, target = "value",  id = "eggs_regr")
       
       # Associate vector with blocking factor
       task$cbind(data.table("ID" = id))
       
       # Tell the task about the groups
       task$set_col_roles("ID", roles = "group")
       
       ##---- 4.1 Tuning  ----
       fts<-names(train.df[,2:ncol(train.df)])
       
       stacked_graph <- gunion(
         list(po("learner_cv", lrn("regr.xgboost", predict_type = "response", nthread = 5)), #importance for xgboost?
              po("learner_cv", lrn('regr.ranger', predict_type = "response", num.threads = 5, importance="permutation")),
              po("learner_cv", lrn('regr.gbm', predict_type = "response")), # Error: Element with key 'regr.gbm' not found in DictionaryLearner! Did you mean 'regr.glmnet'
              po("learner_cv", lrn('regr.cubist', predict_type = "response"))
         )) %>>%
         po("featureunion") %>>%
         lrn("regr.lm", id = "lm_ensemble", predict_type = "se")
       # lrn("regr.glmnet", id = "glm_ensemble", predict_type = "response", family="poisson")
       
       # stacked_graph$plot(html = FALSE)
       stacked_graph$keep_results <- T
       stacked_learner <- as_learner(stacked_graph)
       
       # stacked.search_space <- paradox::ps(
       #   regr.ranger.num.trees =  p_int(lower = 200, upper = 500),
       #   regr.ranger.mtry =  p_int(lower = round((length(fts)-1) * 0.5), upper = (length(fts)-3) ),
       #   regr.ranger.min.node.size = p_int(lower = 5, upper = 10),
       #   regr.xgboost.nrounds = p_int(10, 30),
       #   regr.xgboost.max_depth = p_int(4, 10),
       #   regr.xgboost.eta = p_dbl(0.2, 0.4),
       #   regr.xgboost.subsample = p_dbl(0.9, 1),
       #   regr.xgboost.min_child_weight = p_int(1, 4),
       #   regr.xgboost.colsample_bytree = p_dbl(0.5, 0.6),
       #   regr.gbm.n.trees =p_int(lower = 200, upper = 10000),
       #   # regr.gbm.shrinkage =p_dbl(lower = 0.001, upper = 5),
       #   regr.gbm.interaction.depth =p_int(lower = 1, upper = 5),
       #   regr.gbm.cv.folds = p_int(lower = 5, upper = 5), 
       #   #regr.cubist.neighbors = p_int(lower = 0, upper = 2), 
       #   regr.cubist.committees = p_int(lower = 1, upper = 10)
       # )
       # 
       # tune.eml <- TuningInstanceSingleCrit$new(
       #   task = task,
       #   learner = stacked_learner,
       #   resampling = rsmp("cv", folds = 10),
       #   measure = msr("regr.rmse"),
       #   search_space = stacked.search_space,
       #   terminator = trm("evals", n_evals = 10)
       # )
       # 
       #        set.seed(255)
       # tuner <- tnr("random_search")
       # future::plan("multisession", workers = 5) # workers metti 5, possono essere di più con una macchina potente
       # tuner$optimize(tune.eml) #run tuning 
       
       # save tuned hyperparameters
       # if(Autoregr_model == 1) {
       #   outname <-  paste0(out.dir, "/TunedHyperParam_autoregrModel_", paste0(range(myYears[[y]]), collapse = "_"), "_Frac",  myFrac[f], "_iter", i, "_" , Sys.Date(), ".RDS")
       #   saveRDS(tune.eml, outname)
       # } else {
       #   outname<-  paste0(out.dir, "/TunedHyperParam_Model_Years", paste0(range(myYears[[y]]), collapse = "_"), "_Frac",  myFrac[f], "_iter", i, "_" , Sys.Date(), ".RDS")
       #   saveRDS(tune.eml, outname)
       # }
       
       tune.eml <- readRDS("~/Dati/Output/TunedHyperParam_Model_2024-02-12.RDS")
       # tune.eml
       
       #create new learner just to separate the two learners
       best.learner <- stacked_learner
       best.learner$param_set$values <- tune.eml$result$learner_param_vals[[1]]
       # Example for assigning valid parameters to the XGBoost learner
       best.learner$state$graph$regr.xgboost$param_set$values <- tune.eml$result$learner_param_vals[[1]]
       
       ## 4.2 Stacking ----
       # Create task + Codice for groupin = "value",  id = "eggs_regr")
       task <-  as_task_regr(train.df, target = "value",  id = "eggs_regr")
       task$cbind(data.table("ID" = id))
       
       # Tell the task about the groups
       task$set_col_roles("ID", roles = "group")
       
       # train the best model
       best.learner$train(task)
       # summary(best.learner$model$lm_ensemble$model)
       # AIC(best.learner$model$lm_ensemble$model)
       
       if(Autoregr_model == 1) {
         outname<-  paste0(out.dir, "/stacked_autoregrModel_train_", paste0(range(myYears[[y]]), collapse = "_"), "_Frac",  myFrac[f], "_iter", i, "_" , Sys.Date(), ".RDS")
         saveRDS(best.learner, outname)
       } else {
         outname<-  paste0(out.dir, "/stacked_Model_train_", paste0(range(myYears[[y]]), collapse = "_"), "_Frac",  myFrac[f], "_iter", i, "_" , Sys.Date(), ".RDS")
         saveRDS(best.learner, outname)
       }
       
       # model diagnostics
       # car::vif(best.learner$model$lm_ensemble$model)
       # car::marginalModelPlots(best.learner$model$lm_ensemble$model)
       # car::residualPlots(best.learner$model$lm_ensemble$model)
       
       ##----4.3 internal cross validation ----
       
       if(myFrac[f]!=0.1){
         myresmpl <- rsmp("cv",folds=10L)
         
         cv_predictions <- resample(
           task = task,
           learner = best.learner,
           resampling = myresmpl,
         )
         
         # as.list(cv_predictions$predictions()) #extract prediction from each iteration in a list, we can the tile id by joining back the column from the original task
         # cv_predictions$aggregate(msr("regr.rmse"))
         # cv_predictions$data$response
         # cv_predictions$score(msr("regr.rmse"))
         
         if(Autoregr_model == 1) {
           outname<-  paste0(out.dir, "/stacked_autoregrModel_train_internalCV_", paste0(range(myYears[[y]]), collapse = "_"), "_Frac",  myFrac[f], "_iter", i, "_" , Sys.Date(), ".RDS")
           saveRDS(cv_predictions, outname)
         } else {
           outname<-  paste0(out.dir, "/stacked_Model_train_internalCV_", paste0(range(myYears[[y]]), collapse = "_"), "_Frac",  myFrac[f], "_iter", i, "_" , Sys.Date(), ".RDS")
           saveRDS(cv_predictions, outname)
         }
      } # end of frac if statement
       
    
       ## 4.4  Predict on the training dataset ----
       pred.train <- best.learner$predict(task)
       # predict on the internal testing dataset
       pred.test = predict(best.learner, newdata = test.df, predict_type = "<Prediction>")
       # predict on the external testing dataset
       pred.test_Ext <- predict(best.learner, newdata = test.exdf, predict_type = "<Prediction>")
       
       fitted.stack <- data.frame("ID"= date.train$ID,
                                  "Region"= date.train$Region,
                                  "Country"= date.train$Country,
                                  "date" = date.train$date,
                                  "value"= round(pred.train$response, 3),
                                  "se" = round(pred.train$se, 3),
                                  "model" = "stacked", 
                                  "partition"="Train")
       
       test.stack <- data.frame(  "ID"= date.test$ID,
                                  "Region"= date.test$Region,
                                  "Country"= date.test$Country,
                                  "date" = date.test$date,
                                  "value"= round(pred.test$response, 3),
                                  "se" = round(pred.test$se, 3),
                                  "model" = "stacked", 
                                  "partition"="Test_int")
       
       test.stack_Ext <- data.frame("ID"= date.extest$ID,
                                    "Region"= date.extest$Region,
                                    "Country"= date.extest$Country,
                                    "value"= round(pred.test_Ext$response, 3),
                                    "se" = round(pred.test_Ext$se, 3),
                                    "date"= date.extest$date,
                                    "model" = "stacked",
                                    "partition"="Test_ext")
       
       myPred <- rbind(fitted.stack, test.stack, test.stack_Ext)
       
       if(Autoregr_model == 1) {
         outname<-  paste0(out.dir, "/mlr3_stack_AutoRegrPred_", paste0(range(myYears[[y]]), collapse = "_"), "_Frac",  myFrac[f], "_iter", i, "_" , Sys.Date(), ".RDS")
         saveRDS(myPred, outname)
       } else {
         outname <-  paste0(out.dir, "/mlr3_stack_Pred_", paste0(range(myYears[[y]]), collapse = "_"), "_Frac",  myFrac[f], "_iter", i, "_" , Sys.Date(), ".RDS")
         saveRDS(myPred, outname)
       }
       
       # export also observations for comparison
       myObs<-bind_rows(cbind(date.train, value=train.df$value, partition="Train", model="Observations"), 
                        cbind(date.test, value=test.df$value, partition="Test_int", model="Observations")) %>% mutate(ID = as.factor(ID))
       
       if(Autoregr_model == 1) {
         outname<-  paste0(out.dir, "/mlr3_stack_AutoRegrObs_", paste0(range(myYears[[y]]), collapse = "_"), "_Frac",  myFrac[f], "_iter", i, "_" , Sys.Date(), ".RDS")
         saveRDS(myObs, outname)
       } else {
         outname<-  paste0(out.dir, "/mlr3_stack_Obs_", paste0(range(myYears[[y]]), collapse = "_"), "_Frac",  myFrac[f], "_iter", i, "_" , Sys.Date(), ".RDS")
         saveRDS(myObs, outname)
       }
       
     } # end of myReps loop
    
   } # end of myFrac loop

} # end of myYear loop
