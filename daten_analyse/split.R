# 
# test = read.table("results/best5", header = T, sep=",")

# parameter_test1 = read.table("parameter_test", header = T, sep=",")
# parameter_test2 = read.table("parameter_test_all_new", header = T, sep=",")
# parameter_test3 = read.table("parameter_test_all_rnn", header = T, sep=",")
# 
# parameter_test = rbind(parameter_test1,parameter_test2,parameter_test3)

parameter_test = read.table("../results/parameter_test_all", header = T, sep=",")
parameter_test_wo_errors = parameter_test[parameter_test$nof.errors == 0,]
baselines = parameter_test[parameter_test$nof.nets == -1,]

######################################################################################################################################################

# aggregated_all = parameter_test_wo_errors[parameter_test_wo_errors$model %in% c("aggregated","aggregated_wo_outlier"),]
# aggregated = parameter_test_wo_errors[parameter_test_wo_errors$model %in% c("aggregated"),]
# aggregated_wo_outlier = parameter_test_wo_errors[parameter_test_wo_errors$model %in% c("aggregated_wo_outlier"),]
# tuple_all = parameter_test_wo_errors[parameter_test_wo_errors$model %in% c("1tuple","2tuple","triple"),]
# tuple1 = parameter_test_wo_errors[parameter_test_wo_errors$model %in% c("1tuple"),]
# tuple2 = parameter_test_wo_errors[parameter_test_wo_errors$model %in% c("2tuple"),]
# triple = parameter_test_wo_errors[parameter_test_wo_errors$model %in% c("triple"),]
# tuple1_all = parameter_test_wo_errors[parameter_test_wo_errors$model %in% c("1tuple","1tuple_without_outlier"),]
# tuple1_without_outlier = parameter_test_wo_errors[parameter_test_wo_errors$model %in% c("1tuple_without_outlier"),]
# throughput_ema = parameter_test_wo_errors[parameter_test_wo_errors$model %in% c("throughput_ema"),]
# ordered_param = parameter_test_wo_errors[order(parameter_test_wo_errors$mean_rel_rms),]

######################################################################################################################################################

best5 = NULL
for(model in unique(parameter_test_wo_errors$model)){
  this_wo = parameter_test_wo_errors[parameter_test_wo_errors$model == model,]
  if (is.null(best5)){
    best5 = head(this_wo[order(this_wo$mean_rel_rms),],5)
  } else {
    best5 = rbind(best5,head(this_wo[order(this_wo$mean_rel_rms),],5))
  }
}

for (idx in 2:5) {
  best5[,idx] = as.numeric(best5[,idx])
}
for (idx in 9:27) {
  best5[,idx] = as.numeric(best5[,idx])
}

write.table(best5, paste("../results/best5", sep = ""), sep=",", row.names = FALSE)

######################################################################################################################################################

all_layers = NULL
all_neurons = NULL
for(model in unique(parameter_test_wo_errors$model)){
  this_wo = parameter_test_wo_errors[parameter_test_wo_errors$model == model,]
  
  this_layers = aggregate(cbind(this_wo$mean_rel_rms,this_wo$best_rel_rms)~this_wo$layers+this_wo$model, FUN = mean, na.rm =FALSE)
  names(this_layers) = c("layers","model","mean_mean_rel_rms","mean_best_rel_rms")
  mins = aggregate(cbind(this_wo$mean_rel_rms,this_wo$best_rel_rms)~this_wo$layers+this_wo$model, FUN = min, na.rm =FALSE)
  names(mins) = c("layers","model","min_mean_rel_rms","min_best_rel_rms")
  this_layers$min_mean_rel_rms = mins$min_mean_rel_rms
  this_layers$min_best_rel_rms = mins$min_best_rel_rms
  min_layer = this_layers[this_layers$min_mean_rel_rms == min(this_layers$min_mean_rel_rms),"layers"]
  
  this_neurons = aggregate(cbind(mean_rel_rms,best_rel_rms)~neurons+model, data = this_wo[this_wo$layers == min_layer,], FUN = mean, na.rm =FALSE)
  names(this_neurons) = c("layers","model","mean_mean_rel_rms","mean_best_rel_rms")
  mins = aggregate(cbind(mean_rel_rms,best_rel_rms)~neurons+model, data = this_wo[this_wo$layers == min_layer,], FUN = min, na.rm =FALSE)
  names(mins) = c("layers","model","min_mean_rel_rms","min_best_rel_rms")
  this_neurons$min_mean_rel_rms = mins$min_mean_rel_rms
  this_neurons$min_best_rel_rms = mins$min_best_rel_rms
  
  if(is.null(all_layers)){
    all_layers = this_layers[order(this_layers$min_mean_rel_rms),]
  } else {
    all_layers = rbind(all_layers,this_layers[order(this_layers$min_mean_rel_rms),])
  }
  if(is.null(all_neurons)){
    all_neurons = this_neurons[order(this_neurons$min_mean_rel_rms),]
  } else {
    all_neurons = rbind(all_neurons,this_neurons[order(this_neurons$min_mean_rel_rms),])
  }
}  

######################################################################################################################################################

# tuple1_agg_layers = aggregate(cbind(tuple1$mean_rel_rms,tuple1$best_rel_rms)~tuple1$layers+tuple1$model, FUN = mean, na.rm =FALSE)
# names(tuple1_agg_layers) = c("layers","model","mean_mean_rel_rms","mean_best_rel_rms")
# mins = aggregate(cbind(tuple1$mean_rel_rms,tuple1$best_rel_rms)~tuple1$layers+tuple1$model, FUN = min, na.rm =FALSE)
# names(mins) = c("layers","model","min_mean_rel_rms","min_best_rel_rms")
# 
# tuple1_agg_layers$min_mean_rel_rms = mins$min_mean_rel_rms
# tuple1_agg_layers$min_best_rel_rms = mins$min_best_rel_rms
# 
# tuple1_agg_neurons = aggregate(cbind(mean_rel_rms,best_rel_rms)~neurons+model, data = tuple1[tuple1$layers == 3 & tuple1$model == "1tuple_without_outlier",], FUN = min, na.rm =FALSE)
# 
# 
# aggregated_agg_layers = aggregate(cbind(aggregated$mean_rel_rms,aggregated$best_rel_rms)~aggregated$layers+aggregated$model, FUN = mean, na.rm =FALSE)
# names(aggregated_agg_layers) = c("layers","model","mean_mean_rel_rms","mean_best_rel_rms")
# mins = aggregate(cbind(aggregated$mean_rel_rms,aggregated$best_rel_rms)~aggregated$layers+aggregated$model, FUN = min, na.rm =FALSE)
# names(mins) = c("layers","model","min_mean_rel_rms","min_best_rel_rms")
# 
# aggregated_agg_layers$min_mean_rel_rms = mins$min_mean_rel_rms
# aggregated_agg_layers$min_best_rel_rms = mins$min_best_rel_rms
# aggregated_agg_neurons = aggregate(cbind(mean_rel_rms,best_rel_rms)~neurons+model, data = aggregated[aggregated$layers == 7 & aggregated$model == "aggregated",], FUN = min, na.rm =FALSE)

