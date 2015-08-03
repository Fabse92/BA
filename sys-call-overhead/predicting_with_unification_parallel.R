library("neuralnet")
library("foreach")
library("doParallel")
registerDoParallel(cores=4)

d = read.table("data.csv", header = T, sep=",")
d$OpTyp = as.numeric(d$OpTyp)

train_net = function(nof_layers, nof_neurons, test_threshold, nof_reps, algo,error_fct,act_fct) {
  # Einsatz des Neuronalen Netzes
  net = NULL
  net = neuralnet(Quantile_0.1+Quantile_0.5+Quantile_0.9~OpTyp+DeltaOffset+Size, data = d.norm, hidden = layers, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
  
  if (is.null(net)) {
    return("error")
  }
  results = data.frame(compute(net, temp_test)$net.result)
  d.results = data.frame(Duration = head(d$Duration, test_count), pred_Quantile_0.1 = results$X1, pred_Quantile_0.5 = results$X2, pred_Quantile_0.9 = results$X3)
  
  # Resklaierung des Ergebnisses
  d.results$pred_Quantile_0.1 = d.results$pred_Quantile_0.1 * (longest_duration - shortest_duration) + shortest_duration
  d.results$pred_Quantile_0.5 = d.results$pred_Quantile_0.5 * (longest_duration - shortest_duration) + shortest_duration
  d.results$pred_Quantile_0.9 = d.results$pred_Quantile_0.9 * (longest_duration - shortest_duration) + shortest_duration
  
  d.results$error= d.results$pred_Quantile_0.5 - d.results$Duration
  d.results$percent_error = (d.results$error / d.results$Duration)*100
  d.results$abspercent_error = abs(d.results$percent_error)
  
  return (mean(d.results$abspercent_error))
}

# funktioniert nur mit Liste Länge 3
# Gibt ein dara.frame mit einem Eintrag für jede Kombination von einzigartigen Werten aus param_list, die in den Daten vorkommt. Jeder Eintrag enthält
# die drei Werte aus param_list und die Quantile 0.1, 0.5 und 0.9
unify = function(param_list){  
  unique_list = list(unique(param_list[[1]]), unique(param_list[[2]]), unique(param_list[[3]]))
  
  d.unified = data.frame(1,2,3,4,5,6,7,8)
  names(d.unified) = c("OpTyp","DeltaOffset","Size", "Quantile_0.1","Quantile_0.5", "Quantile_0.9", "min_Duration", "max_Duration")
  idx = 1
  comb_idx = 1
  for (idx1 in 1:length(unique_list[[1]])) {
    for (idx2 in 1:length(unique_list[[2]])) {
      for (idx3 in 1:length(unique_list[[3]])) {
        # zunächst werden alle möglichen Kombinationen von einzigartigen Werten gebildet
        pos_combination = c(unique_list[[1]][idx1],unique_list[[2]][idx2],unique_list[[3]][idx3])
        test = d[d$OpTyp == pos_combination[1] & d$DeltaOffset== pos_combination[2] & d$Size == pos_combination[3],]
        # wennn diese Werte-Kombination im Datensatz vorkommt, wird hierfür ein Eintrag erstellt
        if  (nrow(test) > 0) {
          d.unified[comb_idx,] = c(pos_combination[1],pos_combination[2],pos_combination[3],quantile(test$Duration, 0.1),quantile(test$Duration, 0.5),quantile(test$Duration, 0.9), min(test$Duration), max(test$Duration))
          comb_idx = comb_idx + 1
        }
        idx = idx + 1
      }
    }
  }
  return (d.unified)  
}

d.unified = unify(list(d$OpTyp, d$DeltaOffset, d$Size))

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

shortest_duration = min(d.unified$min_Duration)
longest_duration = max(d.unified$max_Duration)

d.norm <- data.frame(apply(d.unified[c("OpTyp","DeltaOffset", "Size")],2,normalize))
#d.norm <- data.frame(apply(d.unified[c("OpTyp","DeltaOffset", "Size","Quantile_0.1","Quantile_0.5", "Quantile_0.9")],2,normalize))
d.norm$Quantile_0.1 = normalize(d.unified$Quantile_0.1, shortest_duration, longest_duration) 
d.norm$Quantile_0.5 = normalize(d.unified$Quantile_0.5, shortest_duration, longest_duration) 
d.norm$Quantile_0.9 = normalize(d.unified$Quantile_0.9, shortest_duration, longest_duration) 

# Test des Netzes auf dem Originalen Datenset
test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset", "Size")],2,normalize))
test_count = 100000
temp_test = head(test_data, test_count)

parameter_test = data.frame(1,2,3,4,5,6,7,8,9,10,11)
names(parameter_test) = c("layers","neurons","reps","threshold","algo","error function","activation function","mean_error","best_error","avg. training duration / sec","nof errors occured")
idx = 1
nof_iterations = 50
for (nof_layers in 1:3) { # Anzahl hidden-layers
  for (nof_neurons in 12:14) { # Anzahl Neuronen pro hidden-layer
    layers = c(nof_neurons)
    for (idx_layer in 2:nof_layers) {
      layers = c(layers, nof_neurons)
    }
    for (nof_reps in 1:1) { # Anzahl Wiederholungen des Trainingsprozesses
      for (threshold_exponent in 5:5) { #  über 5 selten konvergent; über 4 Error
        for (threshold_summand in 9:9) { # threshold des Algorithmus ist 10^(-threshold_exponent) + threshold_summand * 10^(-threshold_exponent);
          test_threshold = 10^(-threshold_exponent) + threshold_summand * 10^(-threshold_exponent)
          for (learning_algo in 2:3) { # 1 - backprop, 2 - rprop+, 3- rprop-, 4 - sag, 5 - slr
            if (learning_algo == 1) {
              algo = "backprop"
            } else if (learning_algo == 2) {
              algo = "rprop+"
            } else if (learning_algo == 3) {
              algo = "rprop-"
            } else if (learning_algo == 4) {
              algo = "sag"
            } else if (learning_algo == 5) {
              algo = "slr"
            }
            for (error_fct_idx in 1:1) { # 1 - sse (sum of squared errors), 2 - ce (cross entropy)
              if (error_fct_idx == 1) {
                error_fct = "sse" 
              } else if (error_fct_idx == 2) {
                error_fct = "ce"
              }
              for (act_fct_idx in 1:1) { # 1 - logistic, 2 - tanh
                if (act_fct_idx == 1) {
                  act_fct = "logistic" 
                } else if (act_fct_idx == 2) {
                  act_fct = "tanh"
                }
                mean_error = 0
                best_error = 10000
                stime <- system.time({
                  return_values <- foreach(icount(nof_iterations), .combine='c', .inorder=FALSE) %dopar% {
                    train_net(nof_layers,nof_neurons,test_threshold,nof_reps,algo,error_fct,act_fct)
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
                #test_type = c(nof_layers,nof_neurons,nof_reps,test_threshold,algo)
                parameter_test[idx,] = c(nof_layers,nof_neurons,nof_reps,test_threshold,algo,error_fct,act_fct,mean_error, best_error, avg_duration,error_count)
                idx = idx + 1
              }
            }
          }
        }
      }
    }
  }
}

# Plot der echten Duration gegenüber der vorhergesagten
plot(d.results$Duration, col="blue", ylim=c(-0.0001, 0.0004))
points(d.results$pred_Quantile_0.5, col="red")

d.results$in_range = 0
d.results$in_range[(d.results$Duration > d.results$pred_Quantile_0.1) & (d.results$Duration < d.results$pred_Quantile_0.9)] = 1
summary(d.results$in_range)
plot(d.results$in_range)


