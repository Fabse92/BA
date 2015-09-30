library("TTR")

# d.read = read.table("../data-cached-off0-seq-R.csv", header = T, sep=",")#, nrows = 1000000)
# names(d.read)[2] = "Size"
# d.read$OpTyp = 1
#  
# d.write = read.table("../data-cached-off0-seq-W.csv", header = T, sep=",")#, nrows = 1000000)
# names(d.write)[2] = "Size"
# d.write$OpTyp = 2
#  
# d = rbind(d.read,d.write)
# 
# last_Offset = head(d$Offset,nrow(d)-1)
# last_Offset = c(0,last_Offset)
# d$DeltaOffset = d$Offset - last_Offset   ##### !!! ACHTUNG !!! #####

d = read.table("../data.csv", header = T, sep=",")

if(typeof(d$OpTyp) == "integer") {
  d$OpTyp = as.numeric(d$OpTyp)
}
  
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
d$known_outlier = d$outlier

##############################################################################################################################################################################

d.agg_throughput_sigma = aggregate(d$throughput, by = list(d$OpTyp,d$DeltaOffset,d$Size), FUN = sd,na.rm =FALSE)
d.agg_throughput_sigma$x[is.na(d.agg_throughput_sigma$x)] = 0
d.agg_mean = aggregate(d$throughput, by = list(d$OpTyp,d$DeltaOffset,d$Size), FUN = mean)

d.agg_Count = aggregate(cbind(Count = 1,d)$Count, by = list(d$OpTyp,d$DeltaOffset,d$Size), FUN = length)

d.aggregated = data.frame(OpTyp = d.agg_mean$Group.1,DeltaOffset = d.agg_mean$Group.2,Size = d.agg_mean$Group.3, sigma =d.agg_throughput_sigma$x, mean =d.agg_mean$x, Count = d.agg_Count$x)

d.merged = merge(d,d.aggregated,by=c("OpTyp","DeltaOffset","Size"),all = TRUE,sort = FALSE)

d$outlier[d.merged$throughput < d.merged$mean - 2 * d.merged$sigma & d.merged$Count > 1] = 1
d$outlier[d.merged$throughput > d.merged$mean + 2 * d.merged$sigma & d.merged$Count > 1] = 1


d.without_outlier = d[d$outlier == 0,]

##############################################################################################################################################################################

d.agg_max = aggregate(d.without_outlier$Duration, by = list(d.without_outlier$OpTyp,d.without_outlier$DeltaOffset,d.without_outlier$Size), FUN = max, na.rm =FALSE)
d.agg_min = aggregate(d.without_outlier$Duration, by = list(d.without_outlier$OpTyp,d.without_outlier$DeltaOffset,d.without_outlier$Size), FUN = min, na.rm =FALSE)

d.agg_quantile_0.1 = aggregate(d.without_outlier$Duration, by = list(d.without_outlier$OpTyp,d.without_outlier$DeltaOffset,d.without_outlier$Size), FUN = quantile, na.rm =FALSE, probs = 0.1)
d.agg_quantile_0.5 = aggregate(d.without_outlier$Duration, by = list(d.without_outlier$OpTyp,d.without_outlier$DeltaOffset,d.without_outlier$Size), FUN = quantile, na.rm =FALSE, probs = 0.5)
d.agg_quantile_0.9 = aggregate(d.without_outlier$Duration, by = list(d.without_outlier$OpTyp,d.without_outlier$DeltaOffset,d.without_outlier$Size), FUN = quantile, na.rm =FALSE, probs = 0.9)

d.agg_dur_mean = aggregate(d.without_outlier$Duration, by = list(d.without_outlier$OpTyp,d.without_outlier$DeltaOffset,d.without_outlier$Size), FUN = mean, na.rm =FALSE)

d.agg_Count = aggregate(cbind(Count = 1,d.without_outlier)$Count, by = list(d.without_outlier$OpTyp,d.without_outlier$DeltaOffset,d.without_outlier$Size), FUN = length)

d.aggregated_wo_outlier = data.frame(OpTyp = d.agg_max$Group.1,DeltaOffset = d.agg_max$Group.2,Size = d.agg_max$Group.3,Quantile_0.1 = d.agg_quantile_0.1$x,Quantile_0.5 = d.agg_quantile_0.5$x,Quantile_0.9 = d.agg_quantile_0.9$x,mean_Duration = d.agg_dur_mean$x,min_Duration = d.agg_min$x,max_Duration = d.agg_max$x,Count = d.agg_Count$x)

##############################################################################################################################################################################

d.agg_dur_mean = aggregate(d$Duration, by = list(d$OpTyp,d$DeltaOffset,d$Size), FUN = mean, na.rm =FALSE)
d.agg_max = aggregate(d$Duration, by = list(d$OpTyp,d$DeltaOffset,d$Size), FUN = max, na.rm =FALSE)
d.agg_min = aggregate(d$Duration, by = list(d$OpTyp,d$DeltaOffset,d$Size), FUN = min, na.rm =FALSE)

d.agg_quantile_0.1 = aggregate(d$Duration, by = list(d$OpTyp,d$DeltaOffset,d$Size), FUN = quantile, na.rm =FALSE, probs = 0.1)
d.agg_quantile_0.5 = aggregate(d$Duration, by = list(d$OpTyp,d$DeltaOffset,d$Size), FUN = quantile, na.rm =FALSE, probs = 0.5)
d.agg_quantile_0.9 = aggregate(d$Duration, by = list(d$OpTyp,d$DeltaOffset,d$Size), FUN = quantile, na.rm =FALSE, probs = 0.9)

d.agg_Count = aggregate(cbind(Count = 1,d)$Count, by = list(d$OpTyp,d$DeltaOffset,d$Size), FUN = length)

d.aggregated = data.frame(OpTyp = d.agg_dur_mean$Group.1,DeltaOffset = d.agg_dur_mean$Group.2,Size = d.agg_dur_mean$Group.3,Quantile_0.1 = d.agg_quantile_0.1$x,Quantile_0.5 = d.agg_quantile_0.5$x,Quantile_0.9 = d.agg_quantile_0.9$x,mean_Duration = d.agg_dur_mean$x, min_Duration = d.agg_min$x,max_Duration = d.agg_max$x,Count = d.agg_Count$x)

##############################################################################################################################################################################

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
  if(min ==  max) {
    return(rep(0,length(x)))
  } else {
    (x - min) / (max - min)
  }
}

shortest_duration = min(d.aggregated$min_Duration)
longest_duration = max(d.aggregated$max_Duration)

d.aggregated_norm <- data.frame(apply(d.aggregated[c("OpTyp","DeltaOffset", "Size")],2,normalize))
d.aggregated_norm$Quantile_0.1 = normalize(d.aggregated$Quantile_0.1, shortest_duration, longest_duration) 
d.aggregated_norm$Quantile_0.5 = normalize(d.aggregated$Quantile_0.5, shortest_duration, longest_duration) 
d.aggregated_norm$Quantile_0.9 = normalize(d.aggregated$Quantile_0.9, shortest_duration, longest_duration)
d.aggregated_norm$mean_Duration = normalize(d.aggregated$mean_Duration, shortest_duration, longest_duration) 

d.aggregated_wo_outlier_norm <- data.frame(apply(d.aggregated_wo_outlier[c("OpTyp","DeltaOffset", "Size")],2,normalize))
d.aggregated_wo_outlier_norm$Quantile_0.1 = normalize(d.aggregated_wo_outlier$Quantile_0.1, shortest_duration, longest_duration) 
d.aggregated_wo_outlier_norm$Quantile_0.5 = normalize(d.aggregated_wo_outlier$Quantile_0.5, shortest_duration, longest_duration) 
d.aggregated_wo_outlier_norm$Quantile_0.9 = normalize(d.aggregated_wo_outlier$Quantile_0.9, shortest_duration, longest_duration)
d.aggregated_wo_outlier_norm$mean_Duration = normalize(d.aggregated_wo_outlier$mean_Duration, shortest_duration, longest_duration)
#test <- data.frame(apply(d.unified[c("Quantile_0.1","Quantile_0.5", "Quantile_0.9")],2,normalize, shortest_duration, longest_duration))

# first_row = 0
# for(col_idx in 1:(ncol(d)-1)) {
#   first_row = c(first_row,0)
# }
# last_rows = head(d,nrow(d) - 1)
# last_rows = rbind(first_row,last_rows)
##############################################################################################################################################################################
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

##############################################################################################################################################################################
d.general_pred = merge(d,d.aggregated,by=c("OpTyp","DeltaOffset","Size"),all = TRUE,sort = FALSE)
d.errors = head(data.frame(last_error = d.general_pred$Duration - d.general_pred$mean_Duration, rel_correction = (d.general_pred$Duration - d.general_pred$mean_Duration) / d.general_pred$mean_Duration),nrow(d)-1)
d.general_pred = tail(d.general_pred,nrow(d)-1)

d.general_pred$last_error = 0
d.general_pred$last_error = d.errors[,"last_error"]
d.general_pred$rel_correction = d.errors[,"rel_correction"]

d.tuple1_with_general_pred = d.general_pred
d.general_pred = d.general_pred[c("Duration","mean_Duration","last_error","rel_correction")]


d.general_pred_norm <- data.frame(apply(d.general_pred[c("Duration")],2,normalize))
d.general_pred_norm$Duration = normalize(d.general_pred$Duration, shortest_duration, longest_duration)
d.general_pred_norm$last_error = normalize(d.general_pred$last_error, shortest_duration, longest_duration) 
d.general_pred_norm$rel_correction = normalize(d.general_pred$rel_correction, shortest_duration, longest_duration) 
d.general_pred_norm$mean_Duration = normalize(d.general_pred$mean_Duration, shortest_duration, longest_duration) 
##############################################################################################################################################################################
d.tuple1_with_general_pred = d.tuple1_with_general_pred[c("OpTyp","DeltaOffset","Size","Duration","mean_Duration","rel_correction","last_error")]


d.tuple1_with_general_pred_norm <- data.frame(apply(d.tuple1_with_general_pred[c("OpTyp","DeltaOffset", "Size")],2,normalize))
d.tuple1_with_general_pred_norm$Duration = normalize(d.tuple1_with_general_pred$Duration, shortest_duration, longest_duration) 
d.tuple1_with_general_pred_norm$last_error = normalize(d.tuple1_with_general_pred$last_error, shortest_duration, longest_duration)
d.tuple1_with_general_pred_norm$rel_correction = normalize(d.tuple1_with_general_pred$rel_correction, shortest_duration, longest_duration) 
d.tuple1_with_general_pred_norm$mean_Duration = normalize(d.tuple1_with_general_pred$mean_Duration, shortest_duration, longest_duration) 

row.names(d.tuple1_with_general_pred_norm) = row.names(d.tuple1_with_general_pred)
##############################################################################################################################################################################

d.ema = d
d.ema$throughput_ema = c(0,head(EMA(d$throughput),-1))

d.ema = tail(d.ema,-10)

d.ema_norm <- data.frame(apply(d.ema[,c("Duration","OpTyp","DeltaOffset","Size", "throughput_ema")],2,normalize))
d.ema_norm$Duration = normalize(d.ema$Duration, shortest_duration, longest_duration) 
row.names(d.ema_norm) = row.names(d.ema)

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

save.image("../images/prepared_data")

message("data prepared")

