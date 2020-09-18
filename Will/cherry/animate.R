library(gganimate)
install.packages("gganimate")

plt.data <- combined[complete.cases(combined),]

ggplot(plt.data, aes(x=age, fill=as.factor(year))) + geom_density() + transition_time(year) +
  labs(title = "Year: {frame_time}")
#file_renderer(dir = ".", prefix = "gganim_plot", overwrite = FALSE)
animate(anim)
