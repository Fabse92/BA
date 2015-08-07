library("foreach")
library("doParallel")
library("RSNNS")
registerDoParallel(cores=4)


# Funktion zum normalisieren von Werten
normalize <- function(x,min = NULL,max = NULL){
  if(missing(min)) {
    min = min(x)
  }
  if(missing(max)) {
    max = max(x)
  }
  (x - min) / (max - min)
}

train_rnn_net = function(layers,rnn_type) {
  # Einsatz des Neuronalen Netzes
  rnn_net = NULL
  if (rnn_type == "elman") {
    rnn_net <- elman(d.rnn_input,d.rnn_target,size = layers, learnFuncParams = c(0.1), maxit = 300, linOut = TRUE)
  } else if(rnn_type == "jordan") {
    rnn_net <- jordan(d.rnn_input,d.rnn_target,size = layers, learnFuncParams = c(0.1), maxit = 300, linOut = TRUE)
  }
    
  if (is.null(net)) {
    return("error")
  }
  rnn_results = data.frame(predict(rnn_net, head(d.rnn_input_test,100)))
  names(rnn_results) = c("X1")
  d.rnn_results = data.frame(Duration = head(d.rnn_target_test$Duration, 100), pred_Duration = rnn_results$X1)
  
  d.rnn_results$pred_Duration = d.rnn_results$pred_Duration * (max(d.first_prediction$Duration) - min(d.first_prediction$Duration)) + min(d.first_prediction$Duration)
  d.rnn_results$Duration =  d.rnn_results$Duration * (max(d.first_prediction$Duration) - min(d.first_prediction$Duration)) + min(d.first_prediction$Duration)
  
  d.rnn_results$error= d.rnn_results$pred_Duration - d.rnn_results$Duration
  d.rnn_results$percent_error = (d.rnn_results$error / d.rnn_results$Duration)*100
  d.rnn_results$abspercent_error = abs(d.rnn_results$percent_error)

  return (mean(d.rnn_results$abspercent_error))
}

#################################################################################################

source("get_good_general_NN.R")

net = best_net

#visualizing the network OpTyp+DeltaOffset+Size
plot(best_net)

par(mfrow=c(2,2))
gwplot(best_net, selected.covariate = "OpTyp", min =-2, max = 2)
gwplot(best_net, selected.covariate = "DeltaOffset", min =-20, max = 20)
gwplot(best_net, selected.covariate = "Size", min =-200, max = 200)

test_count = nrow(d)
temp_test = head(test_data, test_count)

results = data.frame(compute(net, temp_test)$net.result)
d.results = data.frame(Duration = head(d$Duration, test_count), pred_Quantile_0.1 = results$X1, pred_Quantile_0.5 = results$X2, pred_Quantile_0.9 = results$X3)

# Resklaierung des Ergebnisses
d.results$pred_Quantile_0.1 = d.results$pred_Quantile_0.1 * (longest_duration - shortest_duration) + shortest_duration
d.results$pred_Quantile_0.5 = d.results$pred_Quantile_0.5 * (longest_duration - shortest_duration) + shortest_duration
d.results$pred_Quantile_0.9 = d.results$pred_Quantile_0.9 * (longest_duration - shortest_duration) + shortest_duration

d.results$error= d.results$pred_Quantile_0.5 - d.results$Duration
d.results$percent_error = (d.results$error / d.results$Duration)*100
d.results$abspercent_error = abs(d.results$percent_error)

mean(d.results$abspercent_error)

# plot(d.results$Duration, col="blue", ylim=c(-0.0001, 0.0004))
# points(d.results$pred_Quantile_0.5, col="red")
# 
# d.results$in_range = 0
# d.results$in_range[(d.results$Duration > d.results$pred_Quantile_0.1) & (d.results$Duration < d.results$pred_Quantile_0.9)] = 1
# summary(d.results$in_range)
# plot(d.results$in_range)
############################################################################################

learn_count = 50000
test_count = nrow(d) - learn_count

d.first_prediction = head(d,test_count)
d.first_prediction$first_pred = head(d.results$pred_Quantile_0.5, nrow(d.first_prediction))
last_errors = d.first_prediction$first_pred - d.first_prediction$Duration
last_errors = c(0,last_errors)
d.first_prediction$last_error = head(last_errors,nrow(d.first_prediction))

d.rnn_input <- head(data.frame(apply(d.first_prediction[c("OpTyp","DeltaOffset", "Size","last_error", "first_pred")],2,normalize)),learn_count)
d.rnn_target <- head(data.frame(apply(d.first_prediction["Duration"],2,normalize)),learn_count)
d.rnn_input_test <- tail(data.frame(apply(d.first_prediction[c("OpTyp","DeltaOffset", "Size","last_error", "first_pred")],2,normalize)),test_count)
d.rnn_target_test <- tail(data.frame(apply(d.first_prediction["Duration"],2,normalize)),test_count)


rnn_parameter_test = data.frame(1,2,3,4,5,6,7)
names(rnn_parameter_test) = c("layers","neurons","rnn_type","mean_error","best_error","avg. training duration / sec","nof errors occured")
idx = 1
nof_iterations = 20
for (nof_layers in 1:1) { # Anzahl hidden-layers
  for (nof_neurons in 10:10) { # Anzahl Neuronen pro hidden-layer
    layers = c(nof_neurons)
    for (idx_layer in 2:nof_layers) {
      layers = c(layers, nof_neurons)
    }
    for (rnn_type in 1:2) {
      if (rnn_type == 1) {
        algo = "elman"
      } else if (rnn_type == 2) {
        algo = "jordan"
      }
      mean_error = 0
      best_error = 10000
      stime <- system.time({
        return_values <- foreach(icount(nof_iterations), .combine='c', .inorder=FALSE) %dopar% {
          train_rnn_net(layers,algo)
        }
      })
      error_count = sum(return_values == "error")
      if (error_count != length(return_values)) {
        mean_errors = as.numeric(return_values[return_values != "error"])
        mean_error = mean(mean_errors)
        best_error = min(mean_errors)
      } else {
        mean_error = 999999
        best_error = 999999
      }
      avg_duration = stime[3] / nof_iterations
      rnn_parameter_test[idx,] = c(as.numeric(nof_layers),as.numeric(nof_neurons),algo,as.numeric(mean_error), as.numeric(best_error), as.numeric(avg_duration),as.numeric(error_count))
      idx = idx + 1
    }
  }
}


rnn_net
plotIterativeError(rnn_net)
summary(rnn_net)

par(mfrow=c(1,1))
plot(d.rnn_target$Duration, col="blue", ylim=c(-0.0004, 0.0004))
points(rnn_net$fitted.values,col ="green")

plot(d.rnn_results$Duration, col="blue", ylim=c(-0.0001, 0.0004))
points(d.rnn_results$pred_Duration, col="red")
