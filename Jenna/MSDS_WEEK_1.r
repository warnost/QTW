library(caret)
library(tidyverse)
library(magrittr)
library("pls")

txt_offline = readLines("C:/Users/b007224/Documents/masters_in_data_science/quantifying_the_world/case_study_2/offline.final.trace.txt")

#######
# This is taken from the R text book.  
# Basically, read in the line, split it by tokens and then assemle it into a data matrix.
#######
processLine = function(x)
{
tokens = strsplit(x, "[;=,]")[[1]] #split the line into tokens
if (length(tokens) == 10)
    return(NULL)
tmp = matrix(tokens[ - (1:10) ], ncol = 4, byrow = TRUE)  #build a 4 column matrix mac,signal,scan,type
cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow(tmp), 6, #builds a 6 column matrix t,scanMac,x,y,z,angle
byrow = TRUE), tmp) #binds them together
}

# create a function that will round off to the nearest major angle
roundOrientation = function(angles) {
  refs = seq(0, by = 45, length  = 9)
  q = sapply(angles, function(o) which.min(abs(o - refs)))
  c(refs[1:8], 0)[q]
}

#process the whole datafile
lines = txt_offline[ substr(txt_offline, 1, 1) != "#" ]
tmp_offline = lapply(lines, processLine)
offline = as.data.frame(do.call("rbind", tmp_offline),stringsAsFactors = FALSE)

#process the online data file
txt_online = readLines("C:/Users/b007224/Documents/masters_in_data_science/quantifying_the_world/case_study_2/online.final.trace.txt")
lines = txt_online[ substr(txt_online, 1, 1) != "#" ]
tmp_online = lapply(lines, processLine)
online = as.data.frame(do.call("rbind", tmp_online),stringsAsFactors = FALSE)

names(offline) = c("time", "scanMac", "posX", "posY", "posZ","orientation", "mac", "signal","channel", "type")
numVars = c("time", "orientation", "signal", "posX", "posY", "posZ")
offline[ numVars ] =  lapply(offline[ numVars ], as.numeric)
offline$posXY = paste(offline$posX, offline$posY, sep = "-")
offline$posXY <- as.factor(offline$posXY)
offline$angle = roundOrientation(offline$orientation)

names(online) = c("time", "scanMac", "posX", "posY", "posZ","orientation", "mac", "signal","channel", "type")
online[ numVars ] =  lapply(online[ numVars ], as.numeric)
online$posXY = paste(online$posX, online$posY, sep = "-")
online$posXY_round = paste(round(online$posX,digits=0), round(online$posY,digits=0), sep = "-")
online$posXY <- as.factor(online$posXY)
online$posXY_round <- as.factor(online$posXY_round)

online$angle = roundOrientation(online$orientation)

offline = offline[offline$type==3,]
head(offline)

online = online[online['type']==3,]
head(online)

offline[offline['posZ']!='0.0',]
subMacs = names(sort(table(offline$mac), decreasing = TRUE))[1:7]

###################################
# adjust which subMacs are in the dataset
subMacs
###################################

vals = data.frame(table(offline['mac']))
vals[order(-vals$Freq),]

offline$signal %<>% as.integer
offline = offline[ offline$mac %in% subMacs, ]
offline_pivot<-select(offline, -c(channel,scanMac)) %>% pivot_wider(names_from = mac,values_from = signal, values_fn = list(signal=mean))
offline_pivot_x_y = offline_pivot
offline_pivot = select(offline_pivot,-c(posX, posY, posZ,type,time, orientation))
offline_pivot_x_y = select(offline_pivot_x_y,-c(posXY, posZ,type,time, orientation))
head(offline_pivot)

vals = data.frame(table(online['mac']))
vals[order(-vals$Freq),]

online$signal %<>% as.integer
online = online[ online$mac %in% subMacs, ]
online_pivot<-select(online, -c(channel,scanMac)) %>% pivot_wider(names_from = mac,values_from = signal, values_fn = list(signal=mean))
online_pivot_x_y = online_pivot
online_pivot = select(online_pivot,-c(posX, posY, posZ,type,time,orientation))
online_pivot_x_y = select(online_pivot_x_y,-c(posXY, posZ,type,time,orientation))
head(online_pivot)

# all 7 subMacs
set.seed(123)
trControl <- trainControl(method  = "cv", number  = 5)
fit <- train(posXY ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = seq(3,11,2)),
             trControl  = trControl,
             data       = offline_pivot,
             na.action  = na.omit,
             preProcess = c("center","scale")
             )
fit
#online_pivot_round = select(online_pivot,-c(posXY))
#online_pivot_round %>% rename(posXY = posXY_round)
#predict = predict(fit, newdata=online_pivot_round, type="class")
#confusionMatrix(predict,online_pivot$posXY)

# 6 subMacs, including 00:0f:a3:39:e1:c0
offline_pivot_6 = select(offline_pivot,-c(`00:0f:a3:39:dd:cd`))
oline_pivot_6 = select(online_pivot,-c(`00:0f:a3:39:dd:cd`))
set.seed(123)
trControl <- trainControl(method  = "cv", number  = 5)
fit <- train(posXY ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = seq(3,11,2)),
             trControl  = trControl,
             data       = offline_pivot_6,
             na.action  = na.omit,
             preProcess = c("center","scale")
)
fit
#online_pivot_round = select(oline_pivot_6,-c(posXY))
#online_pivot_round %>% rename(posXY = posXY_round)
#predict = predict(fit, newdata=online_pivot_round, type="class")


# 6 subMacs, including 00:0f:a3:39:dd:cd
offline_pivot_6 = select(offline_pivot,-c(`00:0f:a3:39:e1:c0`))
oline_pivot_6 = select(online_pivot,-c(`00:0f:a3:39:e1:c0`))
set.seed(123)
trControl <- trainControl(method  = "cv", number  = 5)
fit <- train(posXY ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = seq(3,11,2)),
             trControl  = trControl,
             data       = offline_pivot_6,
             na.action  = na.omit,
             preProcess = c("center","scale")
)
fit
#online_pivot_round = select(oline_pivot_6,-c(posXY))
#online_pivot_round %>% rename(posXY = posXY_round)
#predict = predict(fit, newdata=online_pivot_round, type="class")


# multi-target regression

# all 7 subMacs
offline_pivot_x_y1 <- na.omit(offline_pivot_x_y)
X <- cbind(offline_pivot_x_y1$`00:14:bf:b1:97:8a`,offline_pivot_x_y1$`00:14:bf:b1:97:90`,offline_pivot_x_y1$`00:0f:a3:39:e1:c0`,offline_pivot_x_y1$`00:14:bf:b1:97:8d`,
           offline_pivot_x_y1$`00:14:bf:b1:97:81`,offline_pivot_x_y1$`00:14:bf:3b:c7:c6`,offline_pivot_x_y1$`00:0f:a3:39:dd:cd`,offline_pivot_x_y1$angle)
fit <- plsr(cbind(posX,posY) ~ X, ncomp=8, data=offline_pivot_x_y1, scale=TRUE, validation="CV", segments=5)
ase <- mean(fit$residuals[1]^2+fit$residuals[2]^2)
ase

online_pivot_x_y1 <- na.omit(online_pivot_x_y)
newX <- cbind(online_pivot_x_y1$`00:14:bf:b1:97:8a`,online_pivot_x_y1$`00:14:bf:b1:97:90`,online_pivot_x_y1$`00:0f:a3:39:e1:c0`,online_pivot_x_y1$`00:14:bf:b1:97:8d`,
              online_pivot_x_y1$`00:14:bf:b1:97:81`,online_pivot_x_y1$`00:14:bf:3b:c7:c6`,online_pivot_x_y1$`00:0f:a3:39:dd:cd`,online_pivot_x_y1$angle)
predict <- predict(fit, ncomp=8, newdata=newX)
predict_df <- as.data.frame(predict)
names(predict_df) = c("posX","posY")
ase <- mean((online_pivot_x_y1$posX - predict_df$posX)^2+(online_pivot_x_y1$posY - predict_df$posY)^2)
ase

# 6 subMacs, including 00:0f:a3:39:e1:c0
offline_pivot_x_y1 <- na.omit(offline_pivot_x_y)
X <- cbind(offline_pivot_x_y1$`00:14:bf:b1:97:8a`,offline_pivot_x_y1$`00:14:bf:b1:97:90`,offline_pivot_x_y1$`00:0f:a3:39:e1:c0`,offline_pivot_x_y1$`00:14:bf:b1:97:8d`,
           offline_pivot_x_y1$`00:14:bf:b1:97:81`,offline_pivot_x_y1$`00:14:bf:3b:c7:c6`,offline_pivot_x_y1$angle)
fit <- plsr(cbind(posX,posY) ~ X, ncomp=7, data=offline_pivot_x_y1, scale=TRUE, validation="CV", segments=5)
ase <- mean(fit$residuals[1]^2+fit$residuals[2]^2)
ase

online_pivot_x_y1 <- na.omit(online_pivot_x_y)
newX <- cbind(online_pivot_x_y1$`00:14:bf:b1:97:8a`,online_pivot_x_y1$`00:14:bf:b1:97:90`,online_pivot_x_y1$`00:0f:a3:39:e1:c0`,online_pivot_x_y1$`00:14:bf:b1:97:8d`,
              online_pivot_x_y1$`00:14:bf:b1:97:81`,online_pivot_x_y1$`00:14:bf:3b:c7:c6`,online_pivot_x_y1$angle)
predict <- predict(fit, ncomp=7, newdata=newX)
predict_df <- as.data.frame(predict)
names(predict_df) = c("posX","posY")
ase <- mean((online_pivot_x_y1$posX - predict_df$posX)^2+(online_pivot_x_y1$posY - predict_df$posY)^2)
ase


# 6 subMacs, including 00:0f:a3:39:dd:cd
offline_pivot_x_y1 <- na.omit(offline_pivot_x_y)
X <- cbind(offline_pivot_x_y1$`00:14:bf:b1:97:8a`,offline_pivot_x_y1$`00:14:bf:b1:97:90`,offline_pivot_x_y1$`00:14:bf:b1:97:8d`,
           offline_pivot_x_y1$`00:14:bf:b1:97:81`,offline_pivot_x_y1$`00:14:bf:3b:c7:c6`,offline_pivot_x_y1$`00:0f:a3:39:dd:cd`,offline_pivot_x_y1$angle)
fit <- plsr(cbind(posX,posY) ~ X, ncomp=7, data=offline_pivot_x_y1, scale=TRUE, validation="CV", segments=5)
ase <- mean(fit$residuals[1]^2+fit$residuals[2]^2)
ase

online_pivot_x_y1 <- na.omit(online_pivot_x_y)
newX <- cbind(online_pivot_x_y1$`00:14:bf:b1:97:8a`,online_pivot_x_y1$`00:14:bf:b1:97:90`,online_pivot_x_y1$`00:14:bf:b1:97:8d`,
              online_pivot_x_y1$`00:14:bf:b1:97:81`,online_pivot_x_y1$`00:14:bf:3b:c7:c6`,online_pivot_x_y1$`00:0f:a3:39:dd:cd`,online_pivot_x_y1$angle)
predict <- predict(fit, ncomp=7, newdata=newX)
predict_df <- as.data.frame(predict)
names(predict_df) = c("posX","posY")
ase <- mean((online_pivot_x_y1$posX - predict_df$posX)^2+(online_pivot_x_y1$posY - predict_df$posY)^2)
ase

