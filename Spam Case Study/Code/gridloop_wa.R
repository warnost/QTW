setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load(file="emailDF.Rda")

library(MLmetrics)
library(caret)
library(rpart)
library(naivebayes)
library(e1071)
################
# pick up here #
################


# Ok so first of all our data is in T/F 'factors'.  
# We need to change it to numbers.  And as it turns out, there are quite a few NANs as well.  Let's set those to zero.
setupRnum = function(data) {
  logicalVars = which(sapply(data, is.logical))
  facVars = lapply(data[ , logicalVars], function(x) {
    x = as.numeric(x)
  })
  cbind(facVars, data[ , - logicalVars])
}

emailDFnum = setupRnum(emailDF)

emailDFnum[is.na(emailDFnum)]<-0

# Because our authors prefer Type I/II errors, but the cool kids know that precision/recall/F1 is where its at, while the default of caret is accuracy and kappa.  
# To get us all on the same page, I create a function that returns the metrics we want.  
# However, rather than re-invent the wheel, I just install a package.  I am not sure if it had Type I/II errors so those I made my self.

library(MLmetrics)
f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[1])
  acc <- Accuracy(y_pred = data$pred, y_true = data$obs)
  p <- Precision(y_pred = data$pred, y_true = data$obs, positive = lev[1])
  r <- Recall(y_pred = data$pred, y_true = data$obs, positive = lev[1])
  fp <-sum(data$pred==0 & data$obs==1)/length(data$pred)  
  
  fn <-sum(data$pred==1 & data$obs==0)/length(data$pred)
  c(F1 = f1_val,
    Accuracy = acc,
    prec = p,
    rec = r,
    Type_I_err=fp,
    Type_II_err=fn)
}

library(naivebayes)
library(e1071)
library(rpart)

# make a dataframe of all the parameters to check
# https://topepo.github.io/caret/available-models.html
#nb_grid<-expand.grid(laplace=c(0,0.1,0.3,0.5,1), usekernel=c(T,F), adjust=c(T,F))

# Then we create a trainControl object.  It tells caret how to train--using a cross-validation ('cv') with 3 folds in this case (number = 3).  
# We want the final predictions of the best model and our summary is the custom function from above.
#train_control<-trainControl(method="cv", number=3, savePredictions = 'final',summaryFunction = f1)

# Then we create our model: "model_nb".  We use the caret::train method.  We make 'isSpam' a factor because R is dumb and can't figure out that 1 and 0 are classes.  
#model_nb<-caret::train(as.factor(isSpam) ~ .,data=emailDFnum, trControl = train_control, method='naive_bayes',tuneGrid = nb_grid, na.action = na.omit)
#model_nb

#Did the boss fool us with the folds?  Nope.
#table(model_nb$pred['Resample'])


### rpart ###
results_combined <- data.frame(minsplit=double(),
                               maxdepth=double(),
                               cp=double(),
                               F1=double(),
                               Accuracy=double(),
                               prec=double(),
                               rec=double(),
                               Type_I_err=double(),
                               Type_II_err=double(),
                               stringsAsFactors = FALSE)

control_grid <- expand.grid(minsplit=seq(5,25,1),
                            maxdepth=seq(15,30,1))

cart_grid<-expand.grid(cp = seq(from = 0, to=0.1, by=0.001))

train_control<-trainControl(method="cv", number =5, savePredictions = 'final',summaryFunction = f1)

for(i in 1:nrow(control_grid)) {
  set.seed(1234)
  model_rpart<-caret::train(as.factor(isSpam) ~ .,data=emailDFnum, trControl = train_control, method='rpart',
                            control=rpart.control(minsplit=control_grid$minsplit[i],
                                                  maxdepth=control_grid$maxdepth[i]
                            ),
                            tuneGrid = cart_grid, na.action = na.omit)
  
  results=as.data.frame(model_rpart$results)
  
  
  results$minsplit = control_grid$minsplit[i]
  results$maxdepth = control_grid$maxdepth[i]
  
  if (i!=1) { final_result <- rbind(final_result,results) } else {final_result <- results}

  print(paste0(round(i/nrow(control_grid),4)*100,"% complete",sep=""))
}

library(dplyr)
save(results_combined,file="gridsearch_result2.rda")
#model_rpart
#plot(model_rpart)

library(rattle)
fancyRpartPlot(model_rpart$finalModel)
