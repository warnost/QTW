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

names(online) = c("time", "scanMac", "posX", "posY", "posZ","orientation", "mac", "signal","channel", "type")
online[ numVars ] =  lapply(online[ numVars ], as.numeric)

offline = offline[offline$type==3,]
head(offline)

online = online[online['type']==3,]
head(online)

library(tidyverse)
library(magrittr)

select(offline,-c(scanMac,channel,type))

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
#offline_pivot$posXY = paste(offline_pivot$posX, offline_pivot$posY, sep = "-")
#offline_pivot = select(offline_pivot,-c(posX, posY, posZ,type,time))
offline_pivot = select(offline_pivot,-c(posZ,type,time))

head(offline_pivot)
str(offline_pivot)


vals = data.frame(table(online['mac']))
vals[order(-vals$Freq),]

online$signal %<>% as.integer
online = online[ online$mac %in% subMacs, ]
online_pivot<-select(online, -c(channel,scanMac)) %>% pivot_wider(names_from = mac,values_from = signal, values_fn = list(signal=mean))
#online_pivot$posXY = paste(online_pivot$posX, online_pivot$posY, sep = "-")
#online_pivot = select(online_pivot,-c(posX, posY, posZ,type,time))
online_pivot = select(online_pivot,-c(posZ,type,time))

head(online_pivot)
str(online_pivot)


library("caret")

set.seed(123)

trControl <- trainControl(method  = "cv", number  = 5)
fit <- train(posXY ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = seq(1,11,2)),
             trControl  = trControl,
             data       = offline_pivot,
             na.action  = na.omit,
             preProcess = c("center","scale")
             )
fit
predict(fit, newdata=online_pivot)


set.seed(123)

trControl <- trainControl(method  = "cv", number  = 5)
fit <- train(posXY ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 3),
             trControl  = trControl,
             data       = offline_pivot,
             na.action  = na.omit,
             preProcess = c("center","scale")
)
fit
predict = predict(fit, newdata=online_pivot)

library("pls")
offline_pivot1 <- na.omit(offline_pivot)
X <- cbind(offline_pivot1$`00:14:bf:b1:97:8a`,offline_pivot1$`00:14:bf:b1:97:90`,offline_pivot1$`00:0f:a3:39:e1:c0`,offline_pivot1$`00:14:bf:b1:97:8d`,
           offline_pivot1$`00:14:bf:b1:97:81`,offline_pivot1$`00:14:bf:3b:c7:c6`,offline_pivot1$`00:0f:a3:39:dd:cd`,offline_pivot1$orientation)
fit <- plsr(cbind(posX,posY) ~ X, ncomp=8, data=offline_pivot1, scale=TRUE, validation="CV", segments=5)

online_pivot1 <- na.omit(online_pivot)
newX <- cbind(online_pivot1$`00:14:bf:b1:97:8a`,online_pivot1$`00:14:bf:b1:97:90`,online_pivot1$`00:0f:a3:39:e1:c0`,online_pivot1$`00:14:bf:b1:97:8d`,
              online_pivot1$`00:14:bf:b1:97:81`,online_pivot1$`00:14:bf:3b:c7:c6`,online_pivot1$`00:0f:a3:39:dd:cd`,online_pivot1$orientation)
predict <- predict(fit, ncomp=8, newdata=newX)

offline_pivot$posXY = paste(offline_pivot$posX, offline_pivot$posY, sep = "-")