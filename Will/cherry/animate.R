library(gganimate)
setwd("C:/Users/William/OneDrive/MSDS_7331_QTW/QTW/Will/cherry")

## reduce to complete cases
plt.data <- combined[complete.cases(combined),]

## Calculate Medians and annotation text
medians <- plt.data %>% select(year,age) %>% group_by(year) %>% summarise(median.age=median(age))
medians$text <- paste(medians$year, "Median Age:",medians$median.age)

## I tried to make my own color palette. I failed.
library(RColorBrewer)
colourCount = length(unique(plt.data$year))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))


## ggplot object
anim<-ggplot(plt.data, aes(x=age, fill=as.factor(year))) + geom_density() + 
  geom_vline(data=medians, aes(xintercept=median.age),
             linetype="dashed") +
  geom_text(data=medians, mapping=aes(x=median.age, y=0, label=text), size=4, angle=90, vjust=-0.4, hjust=0) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=16),
        legend.title = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_fill_manual(values=wes_palette(n=14, name="FantasticFox1", type = "continuous")) + 
  transition_time(year) +
  labs(title = "Age Distribution of Cherry Blossom Runners (1999 - 2012)", subtitle = "Year: {frame_time}")

## animate it
anim2 <- animate(anim, duration = 40, end_pause = 20, height = 400, width = 600)

## Save the gif
anim_save(file='density_v3.gif',animation = anim2)
