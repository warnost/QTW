#load(file="gridsearch_result2.rda")
load(file="final_result_knit.rda")
library(plotly)
str(results_combined)

df.list <- list(cp = results_combined$cp,
                maxdepth = results_combined$maxdepth,
                minsplit = results_combined$minsplit,
                F1 = results_combined$F1)

plot_data <- final_result
# volcano is a numeric matrix that ships with R
fig <- plot_ly(z=plot_data$prec, x=plot_data$cp, y=plot_data$minsplit, type = 'mesh3d', opacity = 0.7)
fig <- fig %>% layout(
  title = "Change in F1 score for different values of CP and Minsplit",
  scene = list(
    xaxis = list(title = "Complexity"),
    yaxis = list(title = "Minsplit"),
    zaxis = list(title = "F1")
  ))
fig
fig2 <- plot_ly(z=plot_data$prec, x=plot_data$cp, y=plot_data$maxdepth, type = 'mesh3d', opacity = 0.7)
fig2 <- fig2 %>% layout(
  title = "Change in F1 score for different values of CP and Minsplit",
  scene = list(
    xaxis = list(title = "Complexity"),
    yaxis = list(title = "depth"),
    zaxis = list(title = "prec")
  ))
fig2

fig3 <- plot_ly(z=plot_data$prec, x=plot_data$minsplit, y=plot_data$maxdepth, type = 'mesh3d', opacity = 0.7)
fig3 <- fig3 %>% layout(
  title = "Change in F1 score for different values of CP and Minsplit",
  scene = list(
    xaxis = list(title = "split"),
    yaxis = list(title = "depth"),
    zaxis = list(title = "prec")
  ))
fig3


library(ggplot2)
results_combined$reF1 <- (100-0)*(results_combined$F1 - min(results_combined$F1)) / (max(results_combined$F1)-min(results_combined$F1)) + 0
ggplot(results_combined, aes(x=cp, y=minsplit, z = F1)) + geom_density_2d() + geom_density2d_filled(alpha=0.5)

?geom_density2d_filled


ggplot(results_combined, aes(x=cp, y=minsplit, z = reF1))+ geom_contour_filled(bins = 10)

ggplot(results_combined, aes(x=cp, y=minsplit, z = reF1)) + geom_hex()

ggplot(results_combined, aes(x=cp, y=minsplit, z = F1)) + 
  geom_tile()
p

summary(results_combined$cp)
