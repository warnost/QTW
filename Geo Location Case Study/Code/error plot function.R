## this is built step by step to get the methodology correct

## using these data frames
# online_pivot_x_y1
# predict_df

actual <- online_pivot_x_y1[,c("posX","posY")]
actual$name <- "actual"
actual$obs <- seq(1,length(actual$name),1)
predicted <- predict_df[,c("posX","posY")]
predicted$name <- "predicted"
predicted$obs <- seq(1,length(predicted$name),1)

plot_data <- rbind(actual,predicted)
set.seed(314)
sample_index<- sample(seq(1,length(actual$name),1),100)
plot_data_sample <- plot_data %>% filter(obs %in% sample_index)
ggplot(plot_data_sample,aes(x=posX,y=posY)) + geom_point(aes(color=name)) + geom_line(aes(group=obs))

## This is a function to plot the actual vs predicted pairs, joining them with a line to easily see errors
## 100 sample points are used so as clearly see the errors.
## actual should be a two column data frame with columns "posX" and "posY"
## predicted should be a two column data frame with columns "posX" and "posY"
## sample is the number of pairs to display in the graph
## seed is used when sampling the data
## the function with return a ggplot object
plot_distances <- function(actual_orig, predicted_orig, sample = 100, seed = 314) {

  actual <- actual_orig
  predicted <- predicted_orig
  
  actual$name <- "actual"
  actual$obs <- seq(1,dim(actual)[1],1)
  predicted$name <- "predicted"
  predicted$obs <- seq(1,dim(predicted)[1],1)

  plot_data <- rbind(actual,predicted)
  set.seed(seed)
  sample_index <- sample(seq(1,length(actual$name),1),sample)
  plot_data_sample <- plot_data %>% filter(obs %in% sample_index)
  
  p <- ggplot(plot_data_sample,aes(x=posX,y=posY)) + geom_point(aes(color=name)) + geom_line(aes(group=obs))
  return(p)
}
p <- plot_distances(online_pivot_x_y1[,c("posX","posY")],predict_df[,c("posX","posY")])

caption <- "Figure Z: This plot shows the differences between actual and predicted 
values for model XYZ. We can see the model mostly predicts in the middle region,  
and the larger errors are associated with observations with high or low Y values"
caption <- paste0(strwrap(caption, 80), sep="", collapse="\n")

p + labs(title="Figure Z: Error Plot for Model XYZ", 
         caption= caption,
         colow = "Group") +
         xlab("X Position") +
         ylab("Y Position") +
  theme(plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot",
        legend.title = element_blank())
caption

