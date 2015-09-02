library("neuralnet")

source("preparation_of_data.R")

idx = 1
nof_iterations = 10
best_error = 10000
best_net = neuralnet(Quantile_0.1+Quantile_0.5+Quantile_0.9~OpTyp+DeltaOffset+Size, d.norm, hidden = c(1), threshold = 0.0001, algorithm = "rprop-", rep = 1)
for (nof_neurons in 13:13) {
  for (count in 1:nof_iterations) {
    # Einsatz des Neuronalen Netzes
    net = neuralnet(Quantile_0.1+Quantile_0.5+Quantile_0.9~OpTyp+DeltaOffset+Size, d.norm, hidden = rep(nof_neurons,4), threshold = 0.0001, algorithm = "rprop-", rep = 1)
    
    results = data.frame(compute(net, temp_test)$net.result)
    d.results = data.frame(Duration = head(d$Duration, test_count), pred_Quantile_0.1 = results$X1, pred_Quantile_0.5 = results$X2, pred_Quantile_0.9 = results$X3)
    
    # Resklaierung des Ergebnisses
    d.results$pred_Quantile_0.1 = d.results$pred_Quantile_0.1 * (longest_duration - shortest_duration) + shortest_duration
    d.results$pred_Quantile_0.5 = d.results$pred_Quantile_0.5 * (longest_duration - shortest_duration) + shortest_duration
    d.results$pred_Quantile_0.9 = d.results$pred_Quantile_0.9 * (longest_duration - shortest_duration) + shortest_duration
    
    d.results$error= d.results$pred_Quantile_0.5 - d.results$Duration
    d.results$percent_error = (d.results$error / d.results$Duration)*100
    d.results$abspercent_error = abs(d.results$percent_error)
    
    if (mean(d.results$abspercent_error) < best_error) {
      best_error = mean(d.results$abspercent_error)
      best_net = net
    }
  }
}