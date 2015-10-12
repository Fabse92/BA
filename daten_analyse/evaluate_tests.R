
layers_from = 5# 1
layers_to = 7#9# 13
layers_by = 2# 2
neurons_from = 7# 1
neurons_to = 11# 30
neurons_by = 2# 2

series = ""
para_pfad = "../parameter_tests/"
pfad = "../best_parameters/"

parameter_test = NULL
parameter_test = read.table(paste(para_pfad,"parameter_test-baselines",sep=""), header = T, sep=",", stringsAsFactors = FALSE)

for(layer in seq(layers_from,layers_to,by = layers_by)) {
  for(neuron in seq(neurons_from,neurons_to,by = neurons_by)) {
    if(is.null(parameter_test)) {
      parameter_test = read.table(paste(para_pfad,"parameter_test_",layer,"_",neuron,"_",series,sep=""), header = T, sep=",",stringsAsFactors = FALSE)
      
    } else {
      parameter_test = rbind(parameter_test,read.table(paste(para_pfad,"parameter_test_",layer,"_",neuron,"_",series,sep=""), header = T, sep=",",stringsAsFactors = FALSE))
    }
  }
}

parameter_test = data.frame(parameter_test, stringsAsFactors = FALSE)
write.table(parameter_test, paste(para_pfad,"parameter_test","_",series,sep = ""), sep=",", row.names = FALSE)


best_data = read.table(paste(pfad,"models_data_",layers_from,"_",neurons_from,"_",series,sep=""), header = T, sep=",", stringsAsFactors = FALSE)
best_results = read.table(paste(pfad,"models_results_",layers_from,"_",neurons_from,"_",series,sep=""), header = T, sep=",", stringsAsFactors = FALSE)

for(layer in seq(layers_from,layers_to,by = layers_by)) {
  for(neuron in seq(neurons_from,neurons_to,by = neurons_by)) {
    this_data = read.table(paste(pfad,"models_data_",layer,"_",neuron,"_",series,sep=""), header = T, sep=",", stringsAsFactors = FALSE)
    this_results = read.table(paste(pfad,"models_results_",layer,"_",neuron,"_",series,sep=""), header = T, sep=",", stringsAsFactors = FALSE)
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
}

write.table(best_data, paste("../best_parameters/models_data_",series, sep = ""), sep=",", row.names = FALSE)
write.table(best_results, paste("../best_parameters/models_results_",series, sep = ""), sep=",", row.names = FALSE)



for (idx in 2:5) {
  parameter_test[,idx] = as.numeric(parameter_test[,idx])
}
for (idx in 9:27) {
  parameter_test[,idx] = as.numeric(parameter_test[,idx])
}

parameter_test$model[parameter_test$model == "aggregated_wo_outlier_norm"] = "aggregated_wo_outlier"

parameter_test$model[parameter_test$model == "aggregated_wo_outlier"] = 0
parameter_test$model[parameter_test$model == "aggregated"] = 1
parameter_test$model[parameter_test$model == "tuple1"] = 2
parameter_test$model[parameter_test$model == "tuple2"] = 3
parameter_test$model[parameter_test$model == "triple"] = 4
parameter_test$model[parameter_test$model == "tuple1_without_outlier"] = 5
parameter_test$model[parameter_test$model == "throughput_ema"] = 6

parameter_test$algo[parameter_test$algo == "backprop"] = 1
parameter_test$algo[parameter_test$algo == "rprop+"] = 2
parameter_test$algo[parameter_test$algo == "rprop-"] = 3
parameter_test$algo[parameter_test$algo == "sag"] = 4
parameter_test$algo[parameter_test$algo == "slr"] = 5

parameter_test$error.function[parameter_test$error.function == "sse"] = 1
parameter_test$error.function[parameter_test$error.function == "ce"] = 2

parameter_test$activation.function[parameter_test$activation.function == "logistic"] = 1
parameter_test$activation.function[parameter_test$activation.function == "tanh"] = 2

parameter_test$fail_rate = parameter_test$nof.errors / parameter_test$nof.nets


correlation_alg = parameter_test[,c("mean_rel_rms","model","layers","neurons","algo")]

correlation_agg_wo_outlier1 = parameter_test[parameter_test$model == 0,c("mean_rel_rms","layers","neurons","algo","threshold")]
correlation_agg_wo_outlier2 = parameter_test[parameter_test$model == 0,c("mean_rel_rms","fail_rate","avg..training.duration...sec","mean.nof.steps","arith_mean_rel_error_of_best_rel_rms_on_training_data")]

correlation_agg1 = parameter_test[parameter_test$model == 1,c("mean_rel_rms","layers","neurons","algo","threshold")]
correlation_agg2 = parameter_test[parameter_test$model == 1,c("mean_rel_rms","fail_rate","avg..training.duration...sec","mean.nof.steps","arith_mean_rel_error_of_best_rel_rms_on_training_data")]

correlation_tuple11 = parameter_test[parameter_test$model == 2,c("mean_rel_rms","layers","neurons","algo","threshold")]
correlation_tuple12 = parameter_test[parameter_test$model == 2,c("mean_rel_rms","fail_rate","avg..training.duration...sec","mean.nof.steps","arith_mean_rel_error_of_best_rel_rms_on_training_data")]

correlation_tuple21 = parameter_test[parameter_test$model == 3,c("mean_rel_rms","layers","neurons","algo","threshold")]
correlation_tuple22 = parameter_test[parameter_test$model == 3,c("mean_rel_rms","fail_rate","avg..training.duration...sec","mean.nof.steps","arith_mean_rel_error_of_best_rel_rms_on_training_data")]

correlation_triple1 = parameter_test[parameter_test$model == 4,c("mean_rel_rms","layers","neurons","algo","threshold")]
correlation_triple2 = parameter_test[parameter_test$model == 4,c("mean_rel_rms","fail_rate","avg..training.duration...sec","mean.nof.steps","arith_mean_rel_error_of_best_rel_rms_on_training_data")]

correlation_tuple1_wo_outlier1 = parameter_test[parameter_test$model == 5,c("mean_rel_rms","layers","neurons","algo","threshold")]
correlation_tuple1_wo_outlier2 = parameter_test[parameter_test$model == 5,c("mean_rel_rms","fail_rate","avg..training.duration...sec","mean.nof.steps","arith_mean_rel_error_of_best_rel_rms_on_training_data")]

correlation_throughput_ema1 = parameter_test[parameter_test$model == 6,c("mean_rel_rms","layers","neurons","algo","threshold")]
correlation_throughput_ema2 = parameter_test[parameter_test$model == 6,c("mean_rel_rms","fail_rate","avg..training.duration...sec","mean.nof.steps","arith_mean_rel_error_of_best_rel_rms_on_training_data")]


param_wo =parameter_test[parameter_test$mean_rel_rms != -1,]
ordered_param = param_wo[order(param_wo$mean_rel_rms),]
write.table(head(ordered_param,nrow(ordered_param)), paste(para_pfad,"/best_of_parameter_test","_",series,sep = ""), sep=",", row.names = FALSE)


parameter_test = parameter_test[parameter_test$layers %in% 0:6,]

for (idx in 1:28) {
  parameter_test[,idx] = as.numeric(parameter_test[,idx])
}

cor_file = paste("../results/correlations","_",series)
file.create(cor_file)

write("allgemein:", file = cor_file, append = TRUE)
for (col in 1:28) {
  value = cor(parameter_test[parameter_test[col] != -1,"mean_rel_rms"],parameter_test[parameter_test[col] != -1,col])
  if(is.na(value)) {
    value = 0
  }
  write(paste("mean_rel_rms zu",names(parameter_test)[col],value),file = cor_file, append = TRUE)
}
for(model in 0:6) {
  if(model == 0) {
    write("\nagg_wo_outlier:", file = cor_file, append = TRUE)
  } else if(model == 1) {
    write("\naggr:", file = cor_file, append = TRUE)
  } else if(model == 2) {
    write("\n1tupel:", file = cor_file, append = TRUE)
  } else if(model == 3) {
    write("\n2tupel:", file = cor_file, append = TRUE)
  } else if(model == 4) {
    write("\ntriple:", file = cor_file, append = TRUE)
  } else if(model == 5) {
    write("\ntuple1 wo outlier:", file = cor_file, append = TRUE)
  } else if(model == 6) {
    write("\nthroughput ema:", file = cor_file, append = TRUE)
  } 
  
  for (col in 1:28) {
    value = cor(parameter_test$mean_rel_rms[parameter_test$model == model & parameter_test[col] != -1],parameter_test[parameter_test$model == model & parameter_test[col] != -1,col])
    if(is.na(value)) {
      value = 0
    }
    write(paste("mean_rel_rms zu",names(parameter_test)[col],value),file = cor_file, append = TRUE)
  }
}

