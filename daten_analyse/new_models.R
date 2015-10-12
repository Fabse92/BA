load("../images/prepared_data")

library("neuralnet")
library("RSNNS")
library("foreach")
library("doParallel")
nof_cores = 4
registerDoParallel(cores=nof_cores)
library("cvTools")

#source("../preparation_of_data.R")
#message("data prepared")
source("../help_functions.R")

my_number = "1"

layers_from = 5
layers_to = 5
neurons_from = 25
neurons_to = 25
neurons_by = 1
reps_from = 1
reps_to = 1
exponent_from = 1 # test_threshold = 10^(-threshold_exponent) + threshold_summand * 10^(-threshold_exponent)
exponent_to = 1
summand_from = 0
summand_to = 0
summand_by = 2
learning_algo_from = 2
learning_algo_to = 2
model_from = 16
model_to = 16
max_iter_from = 15000 ### aggregated -> 10k, general_pred -> 1k, sonst 500, rnn -> ???
max_iter_to =  15000#21000
max_iter_by = 5000
net_count_from = 1
net_count_to = 1
net_count_by = 1
all_qs_value = FALSE

### mindestens 2
nof_iterations = 4
nof_folds_full = 10

overall_job_duration = 0
# best_rel_rms = rep(Inf,model_to+1-model_from)mean_q0.1_rel_arith_mean = mean(as.numeric(single_values[15]))

model_names = c("aggregated","aggregated_wo_outlier","aggregated_only_mean","aggregated_only_mean_wo_outlier","tuple1","tuple1_single","tuple1_without_outlier","tuple1_with_outlier_information")
model_names = c(model_names,"tuple1_with_general_pred","tuple1_with_general_pred_with_correction","tuple2","triple","throughput_ema","general_pred_with_correction","rnn_tuple1")
model_names = c(model_names,"rnn_tuple1_with_general_pred","rnn_tuple1_with_general_pred_with_correction","rnn_general_pred_with_correction")

best_models_results = data.frame(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
names(best_models_results) = c("model","layers","neurons","reps","threshold","algo","error function","activation function",
                               "avg. training duration / sec","nof nets","nof errors","nof steps","rel_harmonic_error",
                               "arith_error","rel_arith_error","rel_rms","median_rel_error","first_quartile_rel_error",
                               "third_quartile_rel_error","max_rel_error","arith_mean_rel_error_on_training_data",
                               "weigthed_arith_mean_test_rel_error","mean_in_range","q0.1_rel_arith_mean","q0.1_rel_rms","q0.9_rel_arith_mean",
                               "q0.9_rel_rms","outlier_correct_percent","net_count","nof_bagged_nets")
for (idx in model_from:model_to) {
  best_models_results[idx-model_from+1,]  = c(model_names[idx],rep(Inf,length(best_models_results)-1))
}

best_models_data = data.frame(Duration = d$Duration)
for(col_idx in seq(from = 1,to = (model_to+1-model_from)*3, by = 1)) {
  best_models_data = cbind(best_models_data,data.frame(col_idx))
}

data_names = c("Duration")
for (model_idx in model_from:model_to) {
  data_names = c(data_names,paste(model_names[model_idx],"_Duration",sep =""),paste(model_names[model_idx],"_q0.1",sep =""),paste(model_names[model_idx],"_q0.9",sep =""))
}
names(best_models_data) = data_names

parameter_test = data.frame(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34)
names(parameter_test) = c("model","layers","neurons","reps","threshold","algo","error function","activation function","avg. training duration / sec",
                          "nof nets","nof errors","mean nof steps","mean_rel_harmonic_error","best_rel_harmonic_error","mean_arith_error",
                          "mean_rel_arith_error","best_rel_arith_error","mean_rel_rms","best_rel_rms","mean_median_rel_error",
                          "mean_first_quartile_rel_error","mean_third_quartile_rel_error","max_max_rel_error",
                          "mean_arith_mean_rel_error_on_training_data","arith_mean_rel_error_of_best_rel_rms_on_training_data",
                          "mean_weigthed_arith_mean_test_rel_error","mean_mean_in_range","mean_q0.1_rel_arith_mean","mean_q0.1_rel_rms","mean_q0.9_rel_arith_mean",
                          "mean_q0.9_rel_rms","mean_outlier_correct_percent","mean_net_count","mean_nof_bagged_nets")

idx = 1
for (nof_layers in layers_from:layers_to) { ### Anzahl hidden-layers
  for (nof_neurons in seq(neurons_from,neurons_to, by = neurons_by)) { ### Anzahl Neuronen pro hidden-layer
    layers = c(nof_neurons)
    for (idx_layer in 2:nof_layers) {
      layers = c(layers, nof_neurons)
    }
    for (model_idx in model_from:model_to) {
      if (model_idx == 1) {
        model = "aggregated"
        nof_folds = 1 ### 
        train_data <- d.aggregated_norm[c("OpTyp","DeltaOffset", "Size","Quantile_0.1","Quantile_0.5","Quantile_0.9")]
        train_test_data = d.aggregated_norm[c("OpTyp","DeltaOffset", "Size")]
        test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset", "Size")],2,normalize))
        test_target_data <- d[c("Duration","q0.1","q0.9","outlier")]
        Count = d.aggregated$Count
        all_qs = TRUE
      } else if (model_idx == 2) {
        model = "aggregated_wo_outlier"
        nof_folds = 1 ### 
        train_data <- d.aggregated_wo_outlier_norm[c("OpTyp","DeltaOffset", "Size","Quantile_0.1","Quantile_0.5","Quantile_0.9")]
        train_test_data = d.aggregated_wo_outlier_norm[c("OpTyp","DeltaOffset", "Size")]
        test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset", "Size")],2,normalize))
        test_target_data <- d[c("Duration","q0.1","q0.9","outlier")]
        Count = d.aggregated_wo_outlier$Count
        all_qs = TRUE
      } else if (model_idx == 3) {
        model = "aggregated_only_mean"
        nof_folds = 1 ### 
        train_data <- d.aggregated_norm[c("OpTyp","DeltaOffset","Size","mean_Duration")]
        names(train_data)[names(train_data) == "mean_Duration"] = "Duration"
        train_test_data = d.aggregated_norm[c("OpTyp","DeltaOffset", "Size")]
        test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset", "Size")],2,normalize))
        test_target_data <- d[c("Duration")]
        Count = d.aggregated$Count
        all_qs = FALSE
      } else if (model_idx == 4) {
        model = "aggregated_only_mean_wo_outlier"
        nof_folds = 1 ### 
        train_data <- d.aggregated_wo_outlier_norm[c("OpTyp","DeltaOffset","Size","mean_Duration")]
        names(train_data)[names(train_data) == "mean_Duration"] = "Duration"
        train_test_data = d.aggregated_wo_outlier_norm[c("OpTyp","DeltaOffset", "Size")]
        test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset", "Size")],2,normalize))
        test_target_data <- d[c("Duration")]
        Count = d.aggregated_wo_outlier_norm$Count
        all_qs = FALSE
      } else if (model_idx == 5) { #good parameter: threshold ~ 0.008, algo = rprop+, neurons ~ 14
        model = "tuple1"
        nof_folds = nof_folds_full
        train_data <- data.frame(apply(d[c("Duration","OpTyp","DeltaOffset","Size")],2,normalize))
        train_data$q0.1 = normalize(d$q0.1, shortest_duration, longest_duration) 
        train_data$q0.9 = normalize(d$q0.9, shortest_duration, longest_duration)
        train_data$outlier = d$outlier
        train_test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset","Size")],2,normalize))
        test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset","Size")],2,normalize))
        test_target_data <- d[c("Duration","q0.1","q0.9","outlier")]
        all_qs = all_qs_value
      } else if (model_idx == 6) { #good parameter: threshold ~ 0.008, algo = rprop+, neurons ~ 14
        model = "tuple1_single"
        nof_folds = nof_folds_full
        train_data <- data.frame(apply(d[c("Duration","OpTyp","DeltaOffset","Size")],2,normalize))
        train_test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset","Size")],2,normalize))
        test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset","Size")],2,normalize))
        test_target_data <- d[c("Duration")]
        all_qs = FALSE
      } else if (model_idx == 7) {
        model = "tuple1_without_outlier"
        nof_folds = nof_folds_full
        train_data <- data.frame(apply(d[c("Duration","OpTyp","DeltaOffset","Size","outlier")],2,normalize)) ## die outlier werden bei den folds herausgefiltert
        train_data$q0.1 = normalize(d$q0.1, shortest_duration, longest_duration) 
        train_data$q0.9 = normalize(d$q0.9, shortest_duration, longest_duration)
        train_data$outlier = d$outlier
        train_test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset","Size","outlier")],2,normalize))
        test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset","Size")],2,normalize))
        test_target_data <- d[c("Duration","q0.1","q0.9","outlier")]
        all_qs = all_qs_value
      } else if (model_idx == 8) {
        model = "tuple1_with_outlier_information"
        nof_folds = nof_folds_full
        train_data <- data.frame(apply(d[c("Duration","OpTyp","DeltaOffset","Size","known_outlier")],2,normalize))
        train_data$q0.1 = normalize(d$q0.1, shortest_duration, longest_duration) 
        train_data$q0.9 = normalize(d$q0.9, shortest_duration, longest_duration)
        train_data$outlier = d$outlier
        train_test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset","Size","known_outlier")],2,normalize))
        test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset","Size","known_outlier")],2,normalize))
        test_target_data <- d[c("Duration","q0.1","q0.9","outlier")]
        all_qs = all_qs_value
      } else if (model_idx == 9) {
        model = "tuple1_with_general_pred"
        nof_folds = nof_folds_full
        train_data <- d.tuple1_with_general_pred_norm[c("Duration","q0.1","q0.9","outlier","OpTyp","DeltaOffset","Size","mean_Duration")]
        train_test_data = d.tuple1_with_general_pred_norm[c("OpTyp","DeltaOffset","Size","mean_Duration")]
        test_data <- d.tuple1_with_general_pred_norm[c("OpTyp","DeltaOffset","Size","mean_Duration")]
        test_target_data <- d.tuple1_with_general_pred[c("Duration","q0.1","q0.9","outlier")]
        all_qs = all_qs_value
      }else if (model_idx == 10) {
        model = "tuple1_with_general_pred_with_correction"
        nof_folds = nof_folds_full
        train_data <- d.tuple1_with_general_pred_norm[c("Duration","q0.1","q0.9","outlier","OpTyp","DeltaOffset","Size","mean_Duration","rel_correction","last_error")]
        train_test_data = d.tuple1_with_general_pred_norm[c("OpTyp","DeltaOffset","Size","mean_Duration","rel_correction","last_error")]
        test_data <- d.tuple1_with_general_pred_norm[c("OpTyp","DeltaOffset","Size","mean_Duration","rel_correction","last_error")]
        test_target_data <- d.tuple1_with_general_pred[c("Duration","q0.1","q0.9","outlier")]
        all_qs = all_qs_value
      } else if (model_idx == 11) {
        model = "tuple2"
        nof_folds = nof_folds_full
        train_data <- d.tuple_norm[c("Duration","q0.1","q0.9","outlier","OpTyp","DeltaOffset","Size","Duration.1","OpTyp.1","DeltaOffset.1","Size.1")]
        train_test_data <- d.tuple_norm[c("OpTyp","DeltaOffset","Size","Duration.1","OpTyp.1","DeltaOffset.1","Size.1")]
        test_data <- d.tuple_norm[c("OpTyp","DeltaOffset","Size","Duration.1","OpTyp.1","DeltaOffset.1","Size.1")]
        test_target_data <- d.tuple[c("Duration","q0.1","q0.9","outlier")]
        all_qs = all_qs_value
      } else if (model_idx == 12) {
        model = "triple"
        nof_folds = nof_folds_full
        train_data <- d.triple_norm[c("Duration","q0.1","q0.9","outlier","OpTyp","DeltaOffset","Size","Duration.1","OpTyp.1","DeltaOffset.1","Size.1","Duration.2","OpTyp.2","DeltaOffset.2","Size.2")]
        train_test_data <- d.triple_norm[c("OpTyp","DeltaOffset","Size","Duration.1","OpTyp.1","DeltaOffset.1","Size.1","Duration.2","OpTyp.2","DeltaOffset.2","Size.2")]
        test_data <- d.triple_norm[c("OpTyp","DeltaOffset","Size","Duration.1","OpTyp.1","DeltaOffset.1","Size.1","Duration.2","OpTyp.2","DeltaOffset.2","Size.2")]
        test_target_data <- d.triple[c("Duration","q0.1","q0.9","outlier")]
        all_qs = all_qs_value
      } else if (model_idx == 13) {
        model = "throughput_ema"
        nof_folds = nof_folds_full
        train_data <- d.ema_norm[c("Duration","q0.1","q0.9","outlier","OpTyp","DeltaOffset","Size","throughput_ema")]
        train_test_data <- d.ema_norm[c("OpTyp","DeltaOffset","Size","throughput_ema")]
        test_data <- d.ema_norm[c("OpTyp","DeltaOffset","Size","throughput_ema")]
        test_target_data <- d.ema[c("Duration","q0.1","q0.9","outlier")]
        all_qs = all_qs_value
      } else if (model_idx == 14) {
        model = "general_pred_with_correction"
        nof_folds = nof_folds_full
        train_data <- d.general_pred_norm[c("Duration","q0.1","q0.9","outlier","mean_Duration","rel_correction","last_error")]
        train_test_data = d.general_pred_norm[c("mean_Duration","rel_correction","last_error")]
        test_data <- d.general_pred_norm[c("mean_Duration","rel_correction","last_error")]
        test_target_data <- d.general_pred[c("Duration","q0.1","q0.9","outlier")]
        all_qs = all_qs_value
      } else if (model_idx == 15) {
        model = "rnn_tuple1"
        nof_folds = nof_folds_full
        train_data <- data.frame(apply(d[c("Duration","OpTyp","DeltaOffset","Size")],2,normalize))
        train_test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset","Size")],2,normalize))
        test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset","Size")],2,normalize))
        test_target_data <- d[c("Duration")]
        all_qs = FALSE
      } else if (model_idx == 16) {
        model = "rnn_tuple1_with_general_pred"
        nof_folds = nof_folds_full
        train_data <- d.tuple1_with_general_pred_norm[c("Duration","OpTyp","DeltaOffset","Size","mean_Duration")]
        train_test_data = d.tuple1_with_general_pred_norm[c("OpTyp","DeltaOffset","Size","mean_Duration")]
        test_data <- d.tuple1_with_general_pred_norm[c("OpTyp","DeltaOffset","Size","mean_Duration")]
        test_target_data <- d.tuple1_with_general_pred[c("Duration")]
        all_qs = FALSE
      }else if (model_idx == 17) {
        model = "rnn_tuple1_with_general_pred_with_correction"
        nof_folds = nof_folds_full
        train_data <- d.tuple1_with_general_pred_norm[c("Duration","OpTyp","DeltaOffset","Size","mean_Duration","rel_correction","last_error")]
        train_test_data = d.tuple1_with_general_pred_norm[c("OpTyp","DeltaOffset","Size","mean_Duration","rel_correction","last_error")]
        test_data <- d.tuple1_with_general_pred_norm[c("OpTyp","DeltaOffset","Size","mean_Duration","rel_correction","last_error")]
        test_target_data <- d.tuple1_with_general_pred[c("Duration")]
        all_qs = FALSE
      } else if (model_idx == 18) {
        model = "rnn_general_pred_with_correction"
        nof_folds = nof_folds_full
        train_data <- d.general_pred_norm[c("Duration","mean_Duration","rel_correction","last_error")]
        train_test_data = d.general_pred_norm[c("mean_Duration","rel_correction","last_error")]
        test_data <- d.general_pred_norm[c("mean_Duration","rel_correction","last_error")]
        test_target_data <- d.general_pred[c("Duration")]
        all_qs = FALSE
      }
      if (model %in% c("rnn_tuple1","rnn_tuple1_with_general_pred","rnn_tuple1_with_general_pred_with_correction","rnn_general_pred_with_correction")) {
        type = "rnn"
      } else type = "nn"
      for (nof_reps in reps_from:reps_to) { ### Anzahl Wiederholungen des Trainingsprozesses
        for (threshold_exponent in exponent_from:exponent_to) { ###  über 5 selten konvergent; über 4 Error
          for (threshold_summand in seq(summand_from,summand_to, by = summand_by)) { ### threshold des Algorithmus ist 10^(-threshold_exponent) + threshold_summand * 10^(-threshold_exponent);
            test_threshold = 10^(-threshold_exponent) + threshold_summand * 10^(-threshold_exponent)
            for (learning_algo in learning_algo_from:learning_algo_to) { ### 1 - backprop, 2 - rprop+, 3- rprop-, 4 - sag, 5 - slr
              if (learning_algo == 1) {
                if (type == "nn") {
                  algo = "backprop"
                } else { 
                  algo = "jordan"
                }
              } else if (learning_algo == 2) {
                if (type == "nn") {
                  algo = "rprop+"
                } else { 
                  algo = "elman"
                }
              } else if (learning_algo == 3) {
                algo = "rprop-"
              } else if (learning_algo == 4) {
                algo = "sag"
              } else if (learning_algo == 5) {
                algo = "slr"
              }
              for (net_count in seq(net_count_from,net_count_to, by = net_count_by)) {
                for (max_iter in seq(max_iter_from,max_iter_to, by = max_iter_by)) {
                  for (error_fct_idx in 1:1) { ### 1 - sse (sum of squared errors), 2 - ce (cross entropy)
                    if (error_fct_idx == 1) {
                      error_fct = "sse" 
                    } else if (error_fct_idx == 2) {
                      error_fct = "ce"
                    }
                    if (type == "rnn") error_fct = "-1"
                    for (act_fct_idx in 1:1) { ### 1 - logistic, 2 - tanh
                      if (act_fct_idx == 1) {
                        act_fct = "logistic" 
                      } else if (act_fct_idx == 2) {
                        act_fct = "tanh"
                      }
                      if (type == "rnn") {
                        act_fct = "-1"
                        fold_mode = "consecutive"
                      } else {
                        fold_mode = "random"
                      }
                      fold_iterations = nof_iterations #/ nof_folds #### Achtung, bei kleinen nof_iterations
                      if (nof_folds > 1) {
                        folds = cvFolds(nrow(train_data), K = nof_folds, R = 1,type = fold_mode)
                      }
                      for (fold_idx in 1:1) {#nof_folds) {
                        if (nof_folds == 1) {
                          temp_train = train_data
                          temp_train_test = train_test_data
                          temp_test = test_data
                          temp_target = test_target_data
                        } else if (model == "tuple1_without_outlier") { 
                          temp_train = train_data[folds$subsets[folds$which == fold_idx], ]
                          temp_train = temp_train[temp_train$outlier == 0,]
                          temp_train_test = train_test_data[folds$subsets[folds$which == fold_idx], ]
                          temp_train_test = temp_train_test[temp_train_test$outlier == 0,]
                          temp_train_rest = train_data[folds$subsets[folds$which == fold_idx], ]
                          temp_train_rest = temp_train_rest[temp_train_rest$outlier == 1,]
                          temp_train_test_rest = train_test_data[folds$subsets[folds$which == fold_idx], ]
                          temp_train_test_rest = temp_train_test_rest[temp_train_test_rest$outlier == 1,]
                          temp_test = test_data[folds$subsets[folds$which != fold_idx], ]
                          temp_target = data.frame(test_target_data[folds$subsets[folds$which != fold_idx], ])
                          temp_test = rbind(temp_test,temp_train_test_rest[names(temp_train_test_rest) != "outlier"])
                          temp_target = rbind(temp_target[c("Duration","q0.1","q0.9","outlier")],temp_train_rest[c("Duration","q0.1","q0.9","outlier")])
                          temp_train_test = temp_train_test[names(temp_train_test) != "outlier"]
                        } else { 
                          temp_train = train_data[folds$subsets[folds$which == fold_idx], ]
                          temp_train_test = train_test_data[folds$subsets[folds$which == fold_idx], ]
                          temp_test = test_data[folds$subsets[folds$which != fold_idx], ]
                          temp_target = data.frame(test_target_data[folds$subsets[folds$which != fold_idx], ])
                        }
                        fold_time <- system.time({
                          fold_return <- foreach(icount(fold_iterations), .combine='c', .inorder=FALSE) %dopar% {
                            train_net(layers,test_threshold,nof_reps,algo,error_fct,act_fct,model)
                          }
                        })
                        fold_values = as.numeric(unlist(fold_return[1]))
                        for(value_idx in seq(from = 3,to = fold_iterations*2,by = 2)) {
                          fold_values = cbind(fold_values,as.numeric(unlist(fold_return[value_idx])))
                        }
                        for(col_idx in 1:fold_iterations) {
                          if (fold_values[6,col_idx] == min(fold_values[6,])) {
                            min_idx = col_idx
                          }
                        }
                        if (min(fold_values[6,]) < best_models_results[best_models_results$model == model,"rel_rms"]) {
                          best_models_results[best_models_results$model == model_names[model_idx],] = c(model,nof_layers,nof_neurons,nof_reps,test_threshold,algo,error_fct,act_fct,fold_time[3],nof_iterations,0,process_single(fold_values[,min_idx]))
                          min_frame = data.frame(fold_return[min_idx*2])
                          # min_frame = (fold_return[min_idx*2])
                          deficit = nrow(best_models_data) - nrow(min_frame)
                          
                          if(deficit > 0) {
                            for (row_idx in 1:deficit) {
                              if (all_qs) {
                                new_frame = data.frame(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)
                                names(new_frame) = c("Duration","pred_Duration","error","q0.1","pred_q0.1","q0.9","pred_q0.9",
                                                     "q0.1_error","q0.9_error","q0.1_percent_error","q0.1_abspercent_error","q0.9_percent_error","q0.9_abspercent_error","outlier",
                                                     "pred_outlier","outlier_error","percent_error","abspercent_error")
                              } else {
                                new_frame = data.frame(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
                                names(new_frame) = c("Duration","pred_Duration","error","pred_q0.1","pred_q0.9",
                                                     "q0.1_error","q0.9_error","q0.1_percent_error","q0.1_abspercent_error","q0.9_percent_error","q0.9_abspercent_error","outlier",
                                                     "pred_outlier","percent_error","abspercent_error")
                              }
                              new_frame[1,] = c(rep(0,ncol(new_frame)))
                              new_frame = rbind(new_frame,min_frame)
                              min_frame = new_frame
                            }
                            row.names(min_frame)[1:deficit] = c(1:deficit)
                          }
                          if (model == "aggregated" || model == "aggregated_wo_outlier") {
                            min_frame$pred_Duration = min_frame$pred_Quantile_0.5
                          }
                          best_models_data[,names(best_models_data) == paste(model_names[model_idx],"_Duration",sep = "")] = min_frame$pred_Duration
                          best_models_data[,names(best_models_data) == paste(model_names[model_idx],"_q0.1",sep = "")] = min_frame$pred_q0.1
                          best_models_data[,names(best_models_data) == paste(model_names[model_idx],"_q0.9",sep = "")] = min_frame$pred_q0.9
                        }
                        if (fold_idx == 1) {
                          return_values = fold_values
                          stime = fold_time
                        } else {
                          return_values = cbind(return_values,fold_values)
                          stime = stime + fold_time
                        }
                      }
                      error_count = sum(return_values[1,] == -1)
                      return_values = return_values[,return_values[1,] != -1]
                      values = process_return_values(return_values)
                      
                      avg_duration = stime[3] / (nof_iterations / nof_cores)
                      parameter_test[idx,]  = c(model,nof_layers,nof_neurons,nof_reps,test_threshold,algo,error_fct,act_fct,avg_duration,nof_iterations,error_count,values)
                      idx = idx + 1
                      current_model_str = paste("done with:",nof_layers,nof_neurons,nof_reps,test_threshold,algo,error_fct,act_fct,model,"; es sind:",error_count,"von",nof_iterations,"fehlgeschlagen","; dieser Test hat",stime[3],"Sekunden gebraucht")
                      message(current_model_str)
                      overall_job_duration = overall_job_duration + stime[3]
                      write.table(parameter_test, paste("../parameter_tests/parameter_test_",my_number, sep = ""), sep=",", row.names = FALSE)
                      write.table(best_models_data, paste("../best_parameters/models_data_",my_number, sep = ""), sep=",", row.names = FALSE)
                      write.table(best_models_results, paste("../best_parameters/models_results_",my_number, sep = ""), sep=",", row.names = FALSE)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

message("calculated all models")

# for (idx in 2:5) {
#   parameter_test[,idx] = as.numeric(parameter_test[,idx])
# }
# for (idx in 9:27) {
#   parameter_test[,idx] = as.numeric(parameter_test[,idx])
# }

#save.image(paste("../images/models",my_number, sep =""))
#write.table(parameter_test, paste("../parameter_tests/parameter_test",my_number, sep = ""), sep=",", row.names = FALSE)

#parameter_test$model[parameter_test$model == "aggregated"] = 0
#parameter_test$model[parameter_test$model == "tuple1"] = 1
#parameter_test$model[parameter_test$model == "tuple2"] = 2
#parameter_test$model[parameter_test$model == "triple"] = 3

#plot(mypara$model, mypara$mean_rel_rms, xlim = c(1,3), ylim = c(0,100))
#plot(parameter_test$threshold,parameter_test$mean_rel_rms, ylim = c(0,100), xlim = c(0,0.01))


# Plot der echten Duration gegenüber der vorhergesagten
#plot(d.results$Duration, col="blue", ylim=c(-0.0001, 0.0004))
#points(d.results$pred_Duration, col="red")

# sorted = d.results[order(as.numeric(row.names(d.results))),]
# plot(sorted$Duration, col="blue", ylim=c(-0.0001,0.00015), pch = c(1,rep(NA,10)))
# points(sorted$pred_Duration, col="red",  pch = c(1,rep(NA,10)))
# 
#  d.results$in_range = 0
#  d.results$in_range[(d.results$Duration > d.results$pred_Quantile_0.1) & (d.results$Duration < d.results$pred_Quantile_0.9)] = 1
#  mean(d.results$in_range)
# plot(d.results$in_range)


#for (col in 2:5) {
#  message(cor(parameter_test$mean_rel_rms,parameter_test[col]))
#}

#for (col in 9:12) {
#  message(cor(parameter_test$mean_rel_rms,parameter_test[col]))
#}
 
#for (i in seq(1,10, by = 2)) {
#  message(i)
#}

