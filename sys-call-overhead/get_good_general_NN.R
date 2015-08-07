library("neuralnet")

d = read.table("data.csv", header = T, sep=",")
d$OpTyp = as.numeric(d$OpTyp)

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
test_count = nrow(d)
temp_test = head(test_data, test_count)

idx = 1
nof_iterations = 30
best_error = 10000
best_net = neuralnet(Quantile_0.1+Quantile_0.5+Quantile_0.9~OpTyp+DeltaOffset+Size, d.norm, hidden = c(1), threshold = 0.0001, algorithm = "rprop-", rep = 1)
for (nof_neurons in 13:13) {
  for (count in 1:nof_iterations) {
    # Einsatz des Neuronalen Netzes
    net = neuralnet(Quantile_0.1+Quantile_0.5+Quantile_0.9~OpTyp+DeltaOffset+Size, d.norm, hidden = c(nof_neurons), threshold = 0.0001, algorithm = "rprop-", rep = 1)
    
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