
d = read.table("data.csv", header = T, sep=",")
d$OpTyp = as.numeric(d$OpTyp)

d$throughput = d$Size / d$Duration

d$index = 1:nrow(d)
d$outlier = 0

detect_outlier = function(relevant_cols){
  for(relevant_col in relevant_cols){
    sigma = sd(d[,relevant_col])
    µ = mean(d[,relevant_col])
    d[,"outlier"][d[,relevant_col] < µ - 2 * sigma] = 1
    d[,"outlier"][d[,relevant_col] > µ + 2 * sigma] = 1
  }
  return (d)
}

relevant_cols = list("Size","DeltaOffset")
d = detect_outlier(relevant_cols)

# funktioniert nur mit Liste Länge 3
# Gibt ein dara.frame mit einem Eintrag für jede Kombination von einzigartigen Werten aus param_list, die in den Daten vorkommt. Jeder Eintrag enthält
# die drei Werte aus param_list und die Quantile 0.1, 0.5 und 0.9
aggregate_without_outlier = function(param_list, relevant_col){  
  unique_list = list(unique(param_list[[1]]), unique(param_list[[2]]), unique(param_list[[3]]))
  
  aggregated = data.frame(1,2,3,4,5,6,7,8,9)
  names(aggregated) = c("OpTyp","DeltaOffset","Size", "Quantile_0.1","Quantile_0.5", "Quantile_0.9", "min_Duration", "max_Duration","Count")
  idx = 1
  comb_idx = 1
  for (idx1 in 1:length(unique_list[[1]])) {
    for (idx2 in 1:length(unique_list[[2]])) {
      for (idx3 in 1:length(unique_list[[3]])) {
        # zunächst werden alle möglichen Kombinationen von einzigartigen Werten gebildet
        pos_combination = c(unique_list[[1]][idx1],unique_list[[2]][idx2],unique_list[[3]][idx3])
        combo = d[d$OpTyp == pos_combination[1] & d$DeltaOffset== pos_combination[2] & d$Size == pos_combination[3],]
        # wennn diese Werte-Kombination im Datensatz vorkommt, wird hierfür ein Eintrag erstellt
        if  (nrow(combo) > 0) {
          if  (nrow(combo) > 1) {
            sigma = sd(combo[,relevant_col])
            µ = mean(combo[,relevant_col])
            d[d$OpTyp == pos_combination[1] & d$DeltaOffset== pos_combination[2] & d$Size == pos_combination[3],"outlier"][d[d$OpTyp == pos_combination[1] & d$DeltaOffset== pos_combination[2] & d$Size == pos_combination[3],relevant_col] < µ - 2 * sigma] = 1
            d[d$OpTyp == pos_combination[1] & d$DeltaOffset== pos_combination[2] & d$Size == pos_combination[3],"outlier"][d[d$OpTyp == pos_combination[1] & d$DeltaOffset== pos_combination[2] & d$Size == pos_combination[3],relevant_col] > µ + 2 * sigma] = 1
            # d[d$OpTyp == pos_combination[1] & d$DeltaOffset== pos_combination[2] & d$Size == pos_combination[3] & d[relevant_col] > µ + 2 * sigma, "outlier"] = 1
          }
          combo = d[d$OpTyp == pos_combination[1] & d$DeltaOffset== pos_combination[2] & d$Size == pos_combination[3] & d$outlier == 0,]
          if  (nrow(combo) > 0) {
            aggregated[comb_idx,] = c(pos_combination[1],pos_combination[2],pos_combination[3],quantile(combo$Duration, 0.1),quantile(combo$Duration, 0.5),quantile(combo$Duration, 0.9), min(combo$Duration), max(combo$Duration), nrow(combo))
            comb_idx = comb_idx + 1
          }
        }
        idx = idx + 1
      }
    }
  }
  return (list(aggregated, d))  
}



relevant_col = "throughput"
return_value = aggregate_without_outlier(list(d$OpTyp, d$DeltaOffset, d$Size),relevant_col)
d.aggregated_wo_outlier = data.frame(return_value[1])
d = data.frame(return_value[2])

d.without_outlier = d[d$outlier == 0,]




relevant_cols = list("Size","DeltaOffset")
d = detect_outlier(relevant_cols)

# funktioniert nur mit Liste Länge 3
# Gibt ein dara.frame mit einem Eintrag für jede Kombination von einzigartigen Werten aus param_list, die in den Daten vorkommt. Jeder Eintrag enthält
# die drei Werte aus param_list und die Quantile 0.1, 0.5 und 0.9
aggregate = function(param_list){  
  unique_list = list(unique(param_list[[1]]), unique(param_list[[2]]), unique(param_list[[3]]))
  
  aggregated = data.frame(1,2,3,4,5,6,7,8,9)
  names(aggregated) = c("OpTyp","DeltaOffset","Size", "Quantile_0.1","Quantile_0.5", "Quantile_0.9", "min_Duration", "max_Duration","Count")
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
          aggregated[comb_idx,] = c(pos_combination[1],pos_combination[2],pos_combination[3],quantile(test$Duration, 0.1),quantile(test$Duration, 0.5),quantile(test$Duration, 0.9), min(test$Duration), max(test$Duration), nrow(test))
          comb_idx = comb_idx + 1
        }
        idx = idx + 1
      }
    }
  }
  return (aggregated)  
}

d.aggregated = aggregate(list(d$OpTyp, d$DeltaOffset, d$Size))

# plot(d$index[d$outlier == 0 & d$OpTyp == 1], d$throughput[d$outlier == 0 & d$OpTyp == 1], col="blue", ylim = c(min(d$throughput),max(d$throughput)), xlim = c(0,nrow(d)), xlab = "index", ylab = "throughput")
# points(d$index[d$outlier == 1 & d$OpTyp == 1], d$throughput[d$outlier == 1 & d$OpTyp == 1], col="red")
# points(d$index[d$outlier == 0 & d$OpTyp == 2], d$throughput[d$outlier == 0 & d$OpTyp == 2], col="green")
# points(d$index[d$outlier == 1 & d$OpTyp == 2], d$throughput[d$outlier == 1 & d$OpTyp == 2], col="yellow")

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

shortest_duration = min(d.aggregated$min_Duration)
longest_duration = max(d.aggregated$max_Duration)

d.aggregated_norm <- data.frame(apply(d.aggregated[c("OpTyp","DeltaOffset", "Size")],2,normalize))
d.aggregated_norm$Quantile_0.1 = normalize(d.aggregated$Quantile_0.1, shortest_duration, longest_duration) 
d.aggregated_norm$Quantile_0.5 = normalize(d.aggregated$Quantile_0.5, shortest_duration, longest_duration) 
d.aggregated_norm$Quantile_0.9 = normalize(d.aggregated$Quantile_0.9, shortest_duration, longest_duration) 


d.aggregated_wo_outlier_norm <- data.frame(apply(d.aggregated_wo_outlier[c("OpTyp","DeltaOffset", "Size")],2,normalize))
d.aggregated_wo_outlier_norm$Quantile_0.1 = normalize(d.aggregated_wo_outlier$Quantile_0.1, shortest_duration, longest_duration) 
d.aggregated_wo_outlier_norm$Quantile_0.5 = normalize(d.aggregated_wo_outlier$Quantile_0.5, shortest_duration, longest_duration) 
d.aggregated_wo_outlier_norm$Quantile_0.9 = normalize(d.aggregated_wo_outlier$Quantile_0.9, shortest_duration, longest_duration) 
#test <- data.frame(apply(d.unified[c("Quantile_0.1","Quantile_0.5", "Quantile_0.9")],2,normalize, shortest_duration, longest_duration))

# first_row = 0
# for(col_idx in 1:(ncol(d)-1)) {
#   first_row = c(first_row,0)
# }
# last_rows = head(d,nrow(d) - 1)
# last_rows = rbind(first_row,last_rows)

d.tuple = data.frame(tail(d,nrow(d) - 1), head(d,nrow(d) - 1))
d.triple = data.frame(tail(d.tuple,nrow(d) - 2), head(d,nrow(d) - 2))

for(col_idx in 1:ncol(d)) {
  if(col_idx == 1) {
    d.tuple_norm = data.frame(apply(d.tuple[c(col_idx,col_idx+ncol(d))],2,normalize,min(d[col_idx]),max(d[col_idx])))
  }
  else {
    d.tuple_norm = cbind(d.tuple_norm, apply(d.tuple[c(col_idx,col_idx+ncol(d))],2,normalize,min(d[col_idx]),max(d[col_idx])))
  }
}

for(col_idx in 1:ncol(d)) {
  if(col_idx == 1) {
    d.triple_norm = data.frame(apply(d.triple[c(col_idx,col_idx+ncol(d),col_idx+2*ncol(d))],2,normalize,min(d[col_idx]),max(d[col_idx])))
  }
  else {
    d.triple_norm = cbind(d.triple_norm, apply(d.triple[c(col_idx,col_idx+ncol(d),col_idx+2*ncol(d))],2,normalize,min(d[col_idx]),max(d[col_idx])))
  }
}

library("TTR")

d.ema = d
d.ema$throughput_ema = c(0,head(EMA(d$throughput),-1))

d.ema = tail(d.ema,-10)

d.ema_norm <- data.frame(apply(d.ema[c("OpTyp","DeltaOffset", "Size", "throughput_ema")],2,normalize))
d.ema_norm$Duration = normalize(d.ema$Duration, shortest_duration, longest_duration) 

# d.ema = d
# #faktor = 0.5
# avg_duration = d.ema$Duration[1]
# d.ema$ema_throughput[1] = d.ema$throughput[1]
# for (ridx in 2:nrow(d)) {
#   summand = log(d.ema$Duration[ridx] / avg_duration, 10) / 2
#   if (faktor < -0.4) {faktor = -0.4}
#   if (faktor > 0.4) {faktor = 0.4}
#   faktor = 0.5 + summand
#   d.ema$ema_throughput[ridx] = faktor * d.ema$throughput[ridx] + (1 - faktor) * d.ema$ema_throughput[ridx - 1] 
#   avg_duration = (avg_duration + d.ema$Duration[ridx]) / ridx
# }


# for (col in 1:ncol(d.ema)) {
#   message(cor(d.ema$Duration,d.ema[col]))
# }
# 
# for (col in 1:ncol(d.without_outlier)) {
#   message(cor(d.without_outlier$Duration,d.without_outlier[col]))
# }


