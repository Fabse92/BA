load("../images/prepared_data")

results_pfad = "../results/"

best_data = read.table(paste("../parameter_tests/data_baselines", sep = ""), sep=",",header = TRUE)

# Plot der echten Duration gegenüber der vorhergesagten
# for(col_idx in seq(from = 2, to = ncol(best_data), by = 3)) {
#   png(filename=paste(pfad,"plot_",names(best_data)[col_idx],sep = ""))
#   plot(best_data$Duration, col="blue",pch = c(20, rep(NA, 9)),ylim = c(quantile(best_data$Duration,0.01,names = FALSE),quantile(best_data$Duration,0.99,names = FALSE)))
#   points(best_data[,col_idx], col="red",pch = c(20, rep(NA, 9)))
#   dev.off()
# }

for(col_idx in seq(from = 2, to = ncol(best_data), by = 1)) {
  png(filename=paste(results_pfad,"plot_",names(best_data)[col_idx],sep = ""))
  plot(best_data$Duration, col="blue",pch = c(20, rep(NA, 9)),ylim = c(quantile(best_data$Duration,0.01,names = FALSE),quantile(best_data$Duration,0.99,names = FALSE)))
  points(best_data[,col_idx], col="yellow",pch = c(20, rep(NA, 9)))
  dev.off()
}

all_data = cbind(d,best_data[!names(best_data) %in% c("Duration")])

for(col_idx in seq(from = 2, to = ncol(best_data), by = 1)) {
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
  
  png(filename=paste(results_pfad,"plot_density_",names(best_data)[col_idx],sep = ""))
  par(mfrow = c(2,2))
  plot(density(min_data$Duration), main ="best error", col = "blue", lwd = 1.5)#, xlim = c(quantile(min_data$Duration,0.01,names = FALSE),quantile(min_data$Duration,0.99,names = FALSE)))
  abline(v = mean(min_data$q0.1), untf = FALSE, col = "black",lwd = 2)
  abline(v = mean(min_data$q0.9), untf = FALSE, col = "black", lwd = 2)
  abline(v = mean(min_data[,names(min_data) == names(best_data)[col_idx]]), untf = FALSE, col = "yellow",lwd = 2)
  
  plot(density(median_data$Duration), main ="median error", col = "blue", lwd = 1.5)#, xlim = c(quantile(median_data$Duration,0.01,names = FALSE),quantile(median_data$Duration,0.99,names = FALSE)))
  abline(v = mean(median_data$q0.1), untf = FALSE, col = "black",lwd = 2)
  abline(v = mean(median_data$q0.9), untf = FALSE, col = "black", lwd = 2)
  abline(v = mean(median_data[,names(median_data) == names(best_data)[col_idx]]), untf = FALSE, col = "yellow",lwd = 2)
  
  plot(density(max_data$Duration), main ="worst error", col = "blue", lwd = 1.5)#, xlim = c(quantile(max_data$Duration,0.01,names = FALSE),quantile(max_data$Duration,0.99,names = FALSE)))
  abline(v = mean(max_data$q0.1), untf = FALSE, col = "black",lwd = 2)
  abline(v = mean(max_data$q0.9), untf = FALSE, col = "black", lwd = 2)
  abline(v = mean(max_data[,names(max_data) == names(best_data)[col_idx]]), untf = FALSE, col = "yellow",lwd = 2)
  
  dev.off()
}



