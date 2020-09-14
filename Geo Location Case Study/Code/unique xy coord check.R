## Code assumes you have the online/offline data and pivots in memory.

## Looking at the levels
sort(levels(as.factor(offline_pivot$posXY)))
levels(as.factor(online_pivot$posXY_round))

## plot data
## I want the unique xy coordinates in each set
online_xy <- round(online_pivot[,c("posX","posY")])
online_xy$name = "online"
offline_xy <- offline_pivot[,c("posX","posY")]
offline_xy$name = "offline"
new <- rbind(online_xy,offline_xy)
new <- unique(new)

## plot unique coordinates to look for mismatch
ggplot(new,aes(x=posX,y=posY,color=name)) + geom_jitter(width = 0.3,height = 0)

## final check with antijoins
off_unique <- unique(offline_pivot[,"posXY"])
on_unique <- unique(online_pivot[,"posXY_round"])
names(on_unique) <- "posXY"
on_unique %>% anti_join(off_unique)
off_unique %>% anti_join(on_unique)
