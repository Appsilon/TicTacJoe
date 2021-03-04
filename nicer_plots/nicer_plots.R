load("check_prob_1.RData")
LengthOfTraining = 10000
NiceColorTTJ = rgb(165, 81, 184, max=255)  # nice shade of purple


plot(check_prob_1, type='l', xlim=c(0,LengthOfTraining), ylim=c(0,1), xlab="", ylab="Probability of TTJs first move when he starts", main="Pick corner")
# points(val$training_step, tail(val$check_prob_1, n=1), col=NiceColorTTJ, cex=3, pch=19)


library(plotly)
x = c(1:(LengthOfTraining-1))
y = check_prob_1 * 100
data <- data.frame(x, y)


              
fig <- plot_ly(data, x = ~x) 
fig <- fig %>% add_trace(y = ~y, type = 'scatter', mode = 'lines', line = list(color = 'rgba(160,160,160,1)'))
fig <- fig %>% add_trace(x = ~c(x[LengthOfTraining-1]), y = ~c(y[LengthOfTraining-1]), type = 'scatter', mode = 'markers', marker = list(color = NiceColorTTJ, size = 20)) 

# avg_window = 500
# seq = seq(1, length(x), avg_window)
# avg_x = x[seq]
# avg_y <- sapply(seq, function(i) {mean(y[i:(i+avg_window-1)])})
# fig <- fig %>% add_trace(x = ~avg_x, y = ~avg_y, type = 'scatter', mode = "lines", line= list(color = NiceColorTTJ))


# margin <- list(autoexpand = FALSE,
#                l = 100,
#                r = 100,
#                t = 110)
xaxis <- list(title = "Games trained",
              range = c(1, LengthOfTraining+300))
yaxis <- list(title = "Probability in first move",
              range = c(0-5, 100))

fig <- fig %>% layout(title = "Pick corner", xaxis = xaxis, yaxis = yaxis, 
                      # margin = margin,
                      # autosize = FALSE,
                      showlegend = FALSE)

# Annotations
blob_start <- list(
  xref = 'x',
  yref = 'y',
  x = 100,
  y = 33,
  xanchor = 'left',
  yanchor = 'middle',
  text = ~paste0(33, '%'),
  font = list(family = 'Arial',
              size = 16,
              color = NiceColorTTJ),
  showarrow = FALSE)

blob_current <- list(
  xref = 'x',
  yref = 'y',
  x = LengthOfTraining + 100, # should be training step
  y = y[LengthOfTraining-1],
  xanchor = 'left',
  yanchor = 'middle',
  text = ~paste0(round(y[LengthOfTraining-1]), '%'),
  font = list(family = 'Arial',
              size = 16,
              color = NiceColorTTJ),
  showarrow = FALSE)

fig <- fig %>% layout(annotations = blob_start) 
fig <- fig %>% layout(annotations = blob_current) 

fig
