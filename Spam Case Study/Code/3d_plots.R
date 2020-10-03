load(file="gridsearch_result.rda")
library(plotly)
str(results_combined)

df.list <- list(cp = results_combined$cp,
                maxdepth = results_combined$maxdepth,
                minsplit = results_combined$minsplit,
                F1 = results_combined$F1)

plot_data <- results_combined %>% select(cp, minsplit, F1) %>% as.matrix
# volcano is a numeric matrix that ships with R
fig <- plot_ly(z=results_combined$F1, x=results_combined$cp, y=results_combined$minsplit, type = 'mesh3d', opacity = 0.7)
fig

fig2 <- plot_ly(z=results_combined$F1, x=results_combined$cp, y=results_combined$maxdepth, type = 'mesh3d', opacity = 0.7)
fig2

fig3 <- plot_ly(z=results_combined$F1, x=results_combined$minsplit, y=results_combined$maxdepth, type = 'mesh3d', opacity = 0.7)
fig3
