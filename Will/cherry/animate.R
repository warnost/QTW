library(gganimate)
library(wesanderson)
setwd("C:/Users/William/OneDrive/MSDS_7331_QTW/QTW/Will/cherry")

## reduce to complete cases
plt.data <- combined[complete.cases(combined),]

## Calculate Medians and annotation text
medians <- plt.data %>% select(year,age) %>% group_by(year) %>% summarise(median.age=median(age))
medians$text <- paste(medians$year, "Median Age:",medians$median.age)

only1999 <- plt.data %>% filter(year == 1999) %>% select(age)
med99 <-medians%>% filter(year == 1999) %>% select(median.age, text)

## ggplot object
anim<-ggplot(plt.data) + geom_density(aes(x=age, fill=as.factor(year))) + 
  geom_density(data = only1999, aes(x=age)) +
  geom_vline(data=medians, aes(xintercept=median.age), linetype="dashed") +
  geom_vline(data=med99, aes(xintercept=median.age), linetype="dashed") +
  geom_text(data=medians, mapping=aes(x=median.age, y=0, label=text), size=4, angle=90, vjust=-0.4, hjust=0) +
  geom_text(data=med99, mapping=aes(x=median.age, y=0, label=text), size=4, angle=90, vjust=1.1, hjust=0) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=16),
        legend.title = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_fill_manual(values=wes_palette(n=15, name="FantasticFox1", type = "continuous")) + 
  transition_time(year) +
  labs(title = "Age Distribution of Cherry Blossom Runners (1999 - 2012)", subtitle = "Year: {frame_time}")

## animate it
anim2 <- animate(anim, duration = 40, end_pause = 20, height = 400, width = 600)
anim2
## Save the gif
anim_save(file='density_v3.gif',animation = anim2)
