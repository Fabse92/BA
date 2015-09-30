
first

pfad = "../best_parameters/"

for(this_series in sum_series){
  source(paste("../evaluation/evaluate_tests_",this_series,".R",sep=""))
}

parameter_test = NULL
for(this_series in sum_series){
  if (is.null(parameter_test)){
    parameter_test = read.table(paste("../parameter_tests/parameter_test_",this_series,sep=""), header = T, sep=",", stringsAsFactors = FALSE)
  } else {
    parameter_test = rbind(parameter_test,read.table(paste("../parameter_tests/parameter_test_",this_series,sep=""), header = T, sep=",", stringsAsFactors = FALSE))
  }
}
write.table(parameter_test, paste("../results/parameter_test_all"), sep=",", row.names = FALSE)


best_data = read.table(paste(pfad,"models_data_",series[1],sep=""), header = T, sep=",", stringsAsFactors = FALSE)
best_results = read.table(paste(pfad,"models_results_",series[1],sep=""), header = T, sep=",", stringsAsFactors = FALSE)

for(this_series in series){
  this_results = read.table(paste(pfad,"models_results_",this_series,sep=""), header = T, sep=",", stringsAsFactors = FALSE)
  this_data = read.table(paste(pfad,"models_data_",this_series,sep=""), header = T, sep=",", stringsAsFactors = FALSE)
  for(row_idx in 1:nrow(this_results)){
    if(this_results[row_idx,"model"] %in% best_results$model){
      if(this_results[row_idx,"rel_rms"] < best_results[best_results$model == this_results[row_idx,"model"],"rel_rms"]) {
        best_results[best_results$model == this_results[row_idx,"model"],] = this_results[row_idx,]
        best_data[,names(best_data) == this_results[row_idx,"model"]] = this_data[,names(this_data) == this_results[row_idx,"model"]]
      }
    } else {
      best_results[nrow(best_results)+1,] = this_results[row_idx,]
      best_data = cbind(best_data,this_data[,names(this_data) == this_results[row_idx,"model"]])
      names(best_data)[ncol(best_data)] = this_results[row_idx,"model"]
    }
  }
}

# Plot der echten Duration gegenüber der vorhergesagten
for(col_idx in 2:ncol(best_data)) {
  png(filename=paste(pfad,"plot_",names(best_data)[col_idx],sep = ""))
  plot(best_data$Duration, col="blue",pch = c(20, rep(NA, 9)),ylim = c(quantile(best_data$Duration,0.01,names = FALSE),quantile(best_data$Duration,0.99,names = FALSE)))
  points(best_data[,col_idx], col="red",pch = c(20, rep(NA, 9)))
  dev.off()
}




