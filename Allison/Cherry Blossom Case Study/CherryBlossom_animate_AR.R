
library(dplyr)
library(ggplot2)
library(wesanderson)
library(gganimate)
library(gifski)
library(transformr)
library(tidyr)
library(RColorBrewer)

# Quickly recreating menDF
menResMat = sapply(menTables, extractVariables)
menDF = mapply(createDF, menResMat, year = 1999:2012,
               sex = rep("M", 14), SIMPLIFY = FALSE)


combined = bind_rows(menDF)

## reduce to complete cases
plt.data <- combined[complete.cases(combined),]

# This is the palette I used to attempt to make colors as distinct as possible while still maintaining some gradation
new_pal = c(rev(brewer.pal(9,"YlGn")[3:7]), brewer.pal(9,"PuBu")[3:7], brewer.pal(9,"PuRd")[3:7])

# Static dataset for comparing to 2012
plt.2012 = plt.data[which(plt.data$year %in% c(2012)), ]
plt.2012$year_2012 = plt.2012$year
plt.2012$year = NULL

# QQ Plots comparing all years to 2012 to show that more recent years have lower age
plot <- ggplot() +
  stat_qq(aes(sample = age, color = as.factor(year_2012)), data = plt.2012) +
  stat_qq(aes(sample = age, color = as.factor(year)), data = plt.data) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=16),
        legend.title = element_blank(),
        axis.line = element_line(colour = "black")) + 
  scale_colour_manual(values=new_pal)+
  transition_time(year) + 
  labs(title = "Q-Q Plot of Cherry Blossom Runners (as compared to 2012)", subtitle = "Year: {frame_time}")

my_anim = animate(last_plot(), duration = 40, end_pause = 20, height = 400, width = 600, renderer = gifski_renderer())
anim_save(file='qq_v4.gif',animation = my_anim)
