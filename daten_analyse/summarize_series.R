load("../images/prepared_data")

first

for(this_series in sum_series){
  source(paste("../evaluation/evaluate_tests_",this_series,".R",sep=""))
}

pfad = "../best_parameters/"
para_pfad = "../parameter_tests/"
results_pfad = "../results/"

parameter_test = NULL
for(this_series in sum_series){
  if (is.null(parameter_test)){
    parameter_test = read.table(paste("../parameter_tests/parameter_test_",this_series,sep=""), header = T, sep=",", stringsAsFactors = FALSE)
  } else {
    parameter_test = rbind(parameter_test,read.table(paste("../parameter_tests/parameter_test_",this_series,sep=""), header = T, sep=",", stringsAsFactors = FALSE))
  }
}
write.table(parameter_test, paste("../results/parameter_test_all"), sep=",", row.names = FALSE)

# test_data1 = read.table(paste(pfad,"models_data_",series[1],sep=""), header = T, sep=",", stringsAsFactors = FALSE)
# test_data2 = read.table(paste(pfad,"models_data_",series[2],sep=""), header = T, sep=",", stringsAsFactors = FALSE)
# test_results1 = read.table(paste(pfad,"models_results_",series[1],sep=""), header = T, sep=",", stringsAsFactors = FALSE)
# test_results2 = read.table(paste(pfad,"models_results_",series[2],sep=""), header = T, sep=",", stringsAsFactors = FALSE)

best_data = read.table(paste(pfad,"models_data_",series[1],sep=""), header = T, sep=",", stringsAsFactors = FALSE)
best_results = read.table(paste(pfad,"models_results_",series[1],sep=""), header = T, sep=",", stringsAsFactors = FALSE)

for(this_series in sum_series){
  this_results = read.table(paste(pfad,"models_results_",this_series,sep=""), header = T, sep=",", stringsAsFactors = FALSE)
  this_data = read.table(paste(pfad,"models_data_",this_series,sep=""), header = T, sep=",", stringsAsFactors = FALSE)
  for(row_idx in 1:nrow(this_results)){
    if(this_results[row_idx,"model"] %in% best_results$model){
      if(this_results[row_idx,"rel_rms"] < best_results[best_results$model == this_results[row_idx,"model"],"rel_rms"]) {
        best_results[best_results$model == this_results[row_idx,"model"],] = this_results[row_idx,]
        best_data[,names(best_data) == paste(this_results[row_idx,"model"],"_Duration",sep = "")] = this_data[,names(this_data) == paste(this_results[row_idx,"model"],"_Duration",sep = "")]
        best_data[,names(best_data) == paste(this_results[row_idx,"model"],"_q0.1",sep = "")] = this_data[,names(this_data) == paste(this_results[row_idx,"model"],"_q0.1",sep = "")]
        best_data[,names(best_data) == paste(this_results[row_idx,"model"],"_q0.9",sep = "")] = this_data[,names(this_data) == paste(this_results[row_idx,"model"],"_q0.9",sep = "")]
      }
    } else {
      best_results[nrow(best_results)+1,] = this_results[row_idx,]
      best_data = cbind(best_data,this_data[,names(this_data) == paste(this_results[row_idx,"model"],"_Duration",sep = "")])
      names(best_data)[ncol(best_data)] = paste(this_results[row_idx,"model"],"_Duration",sep = "")
      best_data = cbind(best_data,this_data[,names(this_data) == paste(this_results[row_idx,"model"],"_q0.1",sep = "")])
      names(best_data)[ncol(best_data)] = paste(this_results[row_idx,"model"],"_q0.1",sep = "")
      best_data = cbind(best_data,this_data[,names(this_data) == paste(this_results[row_idx,"model"],"_q0.9",sep = "")])
      names(best_data)[ncol(best_data)] = paste(this_results[row_idx,"model"],"_q0.9",sep = "")
    }
  }
}

write.table(best_data, paste("../best_parameters/best_data", sep = ""), sep=",", row.names = FALSE)
write.table(best_results, paste("../best_parameters/best_results", sep = ""), sep=",", row.names = FALSE)

# Plot der echten Duration gegenüber der vorhergesagten
# for(col_idx in seq(from = 2, to = ncol(best_data), by = 3)) {
#   png(filename=paste(pfad,"plot_",names(best_data)[col_idx],sep = ""))
#   plot(best_data$Duration, col="blue",pch = c(20, rep(NA, 9)),ylim = c(quantile(best_data$Duration,0.01,names = FALSE),quantile(best_data$Duration,0.99,names = FALSE)))
#   points(best_data[,col_idx], col="red",pch = c(20, rep(NA, 9)))
#   dev.off()
# }
for(col_idx in seq(from = 2, to = ncol(best_data), by = 3)) {
  png(filename=paste(results_pfad,"plot_",names(best_data)[col_idx],sep = ""))
  plot(best_data$Duration, col="blue",pch = c(20, rep(NA, 9)),ylim = c(quantile(best_data$Duration,0.01,names = FALSE),quantile(best_data$Duration,0.99,names = FALSE)))
  points(best_data[,col_idx], col="yellow",pch = c(20, rep(NA, 9)))
  points(best_data[,col_idx+1], col="red",pch = c(20, rep(NA, 9)))
  points(best_data[,col_idx+2], col="green",pch = c(20, rep(NA, 9)))
  dev.off()
}

all_data = cbind(d,best_data[!names(best_data) %in% c("Duration")])

for(col_idx in seq(from = 2, to = ncol(best_data), by = 3)) {
  all_data$error = abs(best_data[,col_idx] - best_data$Duration)
  
  mean_error = aggregate(all_data$error, by = list(d$OpTyp,d$DeltaOffset,d$Size), FUN = mean)
  names(mean_error) = c("OpTyp","DeltaOffset","Size","mean_error")
  
  agg_Count = aggregate(cbind(Count = 1,d)$Count, by = list(d$OpTyp,d$DeltaOffset,d$Size), FUN = length)
  names(agg_Count) = c("OpTyp","DeltaOffset","Size","Count")
  mean_error = merge(mean_error,agg_Count,by=c("OpTyp","DeltaOffset","Size"),all = TRUE,sort = FALSE)
  mean_error = mean_error[mean_error$Count > 1,]
  
  min_features = mean_error[mean_error$mean_error == min(mean_error$mean_error),]
  max_features = mean_error[mean_error$mean_error == max(mean_error$mean_error),]
  median_features = mean_error[mean_error$mean_error == median(mean_error$mean_error),]
  
  if (nrow(median_features) == 0) {
    median_features = mean_error[abs(mean_error$mean_error - median(mean_error$mean_error)) == min(abs(mean_error$mean_error - median(mean_error$mean_error))),]
  }
  # es darf nur einen geben
  min_features = min_features[1,]
  max_features = max_features[1,]
  median_features = median_features[1,]
  
  min_data = all_data[all_data$Size == min_features[,"Size"] & all_data$OpTyp == min_features[,"OpTyp"] & all_data$DeltaOffset == min_features[,"DeltaOffset"],]
  max_data = all_data[all_data$Size == max_features[,"Size"] & all_data$OpTyp == max_features[,"OpTyp"] & all_data$DeltaOffset == max_features[,"DeltaOffset"],]
  median_data = all_data[all_data$Size == median_features[,"Size"] & all_data$OpTyp == median_features[,"OpTyp"] & all_data$DeltaOffset == median_features[,"DeltaOffset"],]
    
  png(filename=paste(results_pfad,"plot_density_",best_results[(col_idx+1)/3,"model"],sep = ""))
  par(mfrow = c(2,2))
  plot(density(min_data$Duration), main ="best error", col = "blue", lwd = 1.5)#, xlim = c(quantile(min_data$Duration,0.01,names = FALSE),quantile(min_data$Duration,0.99,names = FALSE)))
  abline(v = mean(min_data$q0.1), untf = FALSE, col = "black",lwd = 2)
  abline(v = mean(min_data$q0.9), untf = FALSE, col = "black", lwd = 2)
  abline(v = mean(min_data[,names(min_data) == names(best_data)[col_idx]]), untf = FALSE, col = "yellow",lwd = 2)
  if (mean(min_data[,names(min_data) == names(best_data)[col_idx + 1]] != -1)) {
    abline(v = mean(min_data[,names(min_data) == names(best_data)[col_idx + 1]]), untf = FALSE, col = "green",lwd = 2)
    abline(v = mean(min_data[,names(min_data) == names(best_data)[col_idx + 2]]), untf = FALSE, col = "red",lwd = 2)
  }
  
  plot(density(median_data$Duration), main ="median error", col = "blue", lwd = 1.5)#, xlim = c(quantile(median_data$Duration,0.01,names = FALSE),quantile(median_data$Duration,0.99,names = FALSE)))
  abline(v = mean(median_data$q0.1), untf = FALSE, col = "black",lwd = 2)
  abline(v = mean(median_data$q0.9), untf = FALSE, col = "black", lwd = 2)
  abline(v = mean(median_data[,names(median_data) == names(best_data)[col_idx]]), untf = FALSE, col = "yellow",lwd = 2)
  if (mean(min_data[,names(min_data) == names(best_data)[col_idx + 1]] != -1)) {
    abline(v = mean(median_data[,names(median_data) == names(best_data)[col_idx + 1]]), untf = FALSE, col = "green",lwd = 2)
    abline(v = mean(median_data[,names(median_data) == names(best_data)[col_idx + 2]]), untf = FALSE, col = "red",lwd = 2)
  }
  
  plot(density(max_data$Duration), main ="worst error", col = "blue", lwd = 1.5)#, xlim = c(quantile(max_data$Duration,0.01,names = FALSE),quantile(max_data$Duration,0.99,names = FALSE)))
  abline(v = mean(max_data$q0.1), untf = FALSE, col = "black",lwd = 2)
  abline(v = mean(max_data$q0.9), untf = FALSE, col = "black", lwd = 2)
  abline(v = mean(max_data[,names(max_data) == names(best_data)[col_idx]]), untf = FALSE, col = "yellow",lwd = 2)
  if (mean(min_data[,names(min_data) == names(best_data)[col_idx + 1]] != -1)) {
    abline(v = mean(max_data[,names(max_data) == names(best_data)[col_idx + 1]]), untf = FALSE, col = "green",lwd = 2)
    abline(v = mean(max_data[,names(max_data) == names(best_data)[col_idx + 2]]), untf = FALSE, col = "red",lwd = 2)
  }
  
  dev.off()
}




















