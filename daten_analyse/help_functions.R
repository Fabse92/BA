
process_return_values = function(return_values) {
  if (error_count == nof_iterations - 1) {
    #mean_harmonic_error = mean(return_values[1])
    mean_rel_harmonic_error = mean(return_values[2])
    best_rel_harmonic_error = min(return_values[2])
    mean_arith_error = mean(return_values[3])
    mean_rel_arith_error = mean(return_values[4])
    best_rel_arith_error = min(return_values[4])
    #mean_rms = mean(return_values[5,])
    mean_rel_rms = mean(return_values[6])
    best_rel_rms = min(return_values[6])
    mean_median_rel_error = mean(return_values[7])
    mean_first_quartile_rel_error = mean(return_values[8])
    mean_third_quartile_rel_error = mean(return_values[9])
    max_max_rel_error = max(return_values[10])
    mean_arith_mean_test_rel_error = mean(return_values[11])
    mean_weigthed_arith_mean_test_rel_error = mean(return_values[12])
    mean_mean_in_range = mean(return_values[13])
    mean_steps = mean(as.numeric(return_values[14]))
    arith_mean_rel_error_of_best_rel_rms_on_training_data = return_values[11]
    mean_q0.1_rel_arith_mean = mean(as.numeric(return_values[15]))
    mean_q0.1_rel_rms = mean(as.numeric(return_values[16]))
    mean_q0.9_rel_arith_mean = mean(as.numeric(return_values[17]))
    mean_q0.9_rel_rms = mean(as.numeric(return_values[18]))
    mean_outlier_percent = mean(as.numeric(return_values[19]))
    mean_net_count = mean(as.numeric(return_values[20]))
    mean_nof_bagged_nets = mean(as.numeric(return_values[21]))
  } else if (error_count == nof_iterations) {
    #mean_harmonic_error = -1
    mean_rel_harmonic_error = -1
    best_rel_harmonic_error = -1
    mean_arith_error = -1
    mean_rel_arith_error = -1
    best_rel_arith_error = -1
    #mean_rms = -1
    mean_rel_rms = -1
    best_rel_rms = -1
    mean_median_rel_error = -1
    mean_first_quartile_rel_error = -1
    mean_third_quartile_rel_error = -1
    max_max_rel_error = -1
    mean_arith_mean_test_rel_error = -1
    mean_weigthed_arith_mean_test_rel_error = -1
    mean_mean_in_range = -1
    mean_steps = -1
    arith_mean_rel_error_of_best_rel_rms_on_training_data = -1
    mean_q0.1_rel_arith_mean = -1
    mean_q0.1_rel_rms = -1
    mean_q0.9_rel_arith_mean = -1 
    mean_q0.9_rel_rms = -1
    mean_outlier_percent = -1 
    mean_net_count = -1
    mean_nof_bagged_nets = -1
  } else {
    #mean_harmonic_error = mean(return_values[1,])
    mean_rel_harmonic_error = mean(return_values[2,])
    best_rel_harmonic_error = min(return_values[2,])
    mean_arith_error = mean(return_values[3,])
    mean_rel_arith_error = mean(return_values[4,])
    best_rel_arith_error = min(return_values[4,])
    #mean_rms = mean(return_values[5,])
    mean_rel_rms = mean(return_values[6,])
    best_rel_rms = min(return_values[6,])
    mean_median_rel_error = mean(return_values[7,])
    mean_first_quartile_rel_error = mean(return_values[8,])
    mean_third_quartile_rel_error = mean(return_values[9,])
    max_max_rel_error = max(return_values[10,])
    mean_arith_mean_test_rel_error = mean(return_values[11,])
    mean_weigthed_arith_mean_test_rel_error = mean(return_values[12,])
    mean_mean_in_range = mean(return_values[13,])
    mean_steps = mean(return_values[14,])
    arith_mean_rel_error_of_best_rel_rms_on_training_data = return_values[11,return_values[6,] == min(return_values[6,])]
    mean_q0.1_rel_arith_mean = mean(as.numeric(return_values[15,]))
    mean_q0.1_rel_rms = mean(as.numeric(return_values[16,]))
    mean_q0.9_rel_arith_mean = mean(as.numeric(return_values[17,]))
    mean_q0.9_rel_rms = mean(as.numeric(return_values[18,]))
    mean_outlier_percent = mean(as.numeric(return_values[19,]))
    mean_net_count = mean(as.numeric(return_values[20,]))
    mean_nof_bagged_nets = mean(as.numeric(return_values[21,]))
  }
  values = (c(mean_steps,mean_rel_harmonic_error,best_rel_harmonic_error,mean_arith_error,mean_rel_arith_error,best_rel_arith_error,mean_rel_rms,
              best_rel_rms,mean_median_rel_error,mean_first_quartile_rel_error,mean_third_quartile_rel_error,max_max_rel_error,
              mean_arith_mean_test_rel_error,arith_mean_rel_error_of_best_rel_rms_on_training_data,mean_weigthed_arith_mean_test_rel_error,
              mean_mean_in_range,mean_q0.1_rel_arith_mean,mean_q0.1_rel_rms,mean_q0.9_rel_arith_mean,mean_q0.9_rel_rms,mean_outlier_percent,mean_net_count,
              mean_nof_bagged_nets))
  return (values)
}

process_single = function(single_values) {
  #mean_harmonic_error = mean(return_values[1])
  mean_rel_harmonic_error = mean(single_values[2])
  mean_arith_error = mean(single_values[3])
  mean_rel_arith_error = mean(single_values[4])
  #mean_rms = mean(return_values[5,])
  mean_rel_rms = mean(single_values[6])
  mean_median_rel_error = mean(single_values[7])
  mean_first_quartile_rel_error = mean(single_values[8])
  mean_third_quartile_rel_error = mean(single_values[9])
  max_max_rel_error = max(single_values[10])
  mean_arith_mean_test_rel_error = mean(single_values[11])
  mean_weigthed_arith_mean_test_rel_error = mean(single_values[12])
  mean_mean_in_range = mean(single_values[13])
  mean_steps = mean(as.numeric(single_values[14]))
  
  mean_q0.1_rel_arith_mean = mean(as.numeric(single_values[15]))
  mean_q0.1_rel_rms = mean(as.numeric(single_values[16]))
  mean_q0.9_rel_arith_mean = mean(as.numeric(single_values[17]))
  mean_q0.9_rel_rms = mean(as.numeric(single_values[18]))
  mean_outlier_percent = mean(as.numeric(single_values[19]))
  
  mean_net_count = mean(as.numeric(single_values[20]))
  mean_nof_bagged_nets = mean(as.numeric(single_values[21]))
  
  values = (c(mean_steps,mean_rel_harmonic_error,mean_arith_error,mean_rel_arith_error,mean_rel_rms,mean_median_rel_error,mean_first_quartile_rel_error,
              mean_third_quartile_rel_error,max_max_rel_error,mean_arith_mean_test_rel_error,mean_weigthed_arith_mean_test_rel_error,mean_mean_in_range,
              mean_q0.1_rel_arith_mean,mean_q0.1_rel_rms,mean_q0.9_rel_arith_mean,mean_q0.9_rel_rms,mean_outlier_percent,mean_net_count,
              mean_nof_bagged_nets))
  return (values)
}

get_error_metrics = function(errors, test_type,test_data) {
  
  if(missing(test_data)) {
    test_data = d
  }
  d.results = errors
  
  names(d.results) = c("error")
  d.results$percent_error = (d.results$error / test_data$Duration)*100
  d.results$abspercent_error = abs(d.results$percent_error)
  
  rel_harmonic_mean = 1/mean((1/d.results$abspercent_error))
  arith_mean = mean(abs(d.results$error))
  rel_arith_mean = mean(d.results$abspercent_error)
  
  rel_rms = sqrt(mean(d.results$abspercent_error^2))
  rel_median_error = median(d.results$abspercent_error)
  rel_first_quartile_error = quantile(d.results$abspercent_error, 0.25, names = FALSE)
  rel_third_quartile_error = quantile(d.results$abspercent_error, 0.75, names = FALSE)
  max_rel_error = max(d.results$abspercent_error)
  
  arith_mean_test_rel_error = -1
  weigthed_arith_mean_test_rel_error = -1
  arith_mean_rel_error_of_best_rel_rms_on_training_data = -1

  mean_in_range = -1
  
  answer_list = c(test_type,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,
                  as.numeric(rel_harmonic_mean),as.numeric(rel_harmonic_mean),
                  as.numeric(arith_mean),as.numeric(rel_arith_mean),as.numeric(rel_arith_mean),as.numeric(rel_rms),as.numeric(rel_rms),
                  as.numeric(rel_median_error),as.numeric(rel_first_quartile_error),as.numeric(rel_third_quartile_error),as.numeric(max_rel_error),
                  as.numeric(arith_mean_test_rel_error),arith_mean_rel_error_of_best_rel_rms_on_training_data,as.numeric(weigthed_arith_mean_test_rel_error),as.numeric(mean_in_range),
                  -1,-1,-1,-1,-1,-1,-1)
  
  return (answer_list)
}

# results_frame = d.results
# target = temp_target

calc_all_qs = function(results_frame,results,target, to_calc = TRUE) {
  if (all_qs & to_calc) {
    old_names = names(results_frame)
    results_frame = data.frame(results_frame,q0.1 = target[names(target) == "q0.1"], pred_q0.1 = results[2],q0.9 = target[names(target) == "q0.9"], pred_q0.9 = results[3]) ## Achtung bei aggregated ist die falsch
    names(results_frame) = c(old_names,"q0.1","pred_q0.1","q0.9","pred_q0.9")
    
    if (model %in% c("aggregated","aggregated_wo_outlier")) {
      results_frame$pred_q0.1 = results_frame$pred_Quantile_0.1
      results_frame$pred_q0.9 = results_frame$pred_Quantile_0.9
    }
    
    # Resklaierung des Ergebnisses
    results_frame$pred_q0.1 = results_frame$pred_q0.1 * (longest_duration - shortest_duration) + shortest_duration
    results_frame$pred_q0.9 = results_frame$pred_q0.9 * (longest_duration - shortest_duration) + shortest_duration
    
    results_frame$q0.1_error= results_frame$pred_q0.1 - results_frame$q0.1
    results_frame$q0.9_error= results_frame$pred_q0.9 - results_frame$q0.9
    results_frame$q0.1_percent_error = (results_frame$q0.1_error / results_frame$q0.1)*100
    results_frame$q0.1_abspercent_error = abs(results_frame$q0.1_percent_error)
    results_frame$q0.9_percent_error = (results_frame$q0.9_error / results_frame$q0.9)*100
    results_frame$q0.9_abspercent_error = abs(results_frame$q0.9_percent_error)
    
    results_frame$outlier = target[names(target) == "outlier"]
    results_frame$pred_outlier = 0
    
    results_frame[results_frame$pred_q0.1 > results_frame$pred_Duration,"pred_outlier"] = 1
    results_frame[results_frame$pred_q0.9 < results_frame$pred_Duration,"pred_outlier"] = 1
    results_frame$outlier_error = abs(results_frame$pred_outlier - results_frame$outlier)
  } else {
    results_frame$pred_q0.1 = -1
    results_frame$pred_q0.9 = -1
    results_frame$q0.1_error= -1
    results_frame$q0.9_error= -1
    results_frame$q0.1_percent_error = -1
    results_frame$q0.1_abspercent_error = -1
    results_frame$q0.9_percent_error = -1
    results_frame$q0.9_abspercent_error = -1
    
    results_frame$outlier = -1
    results_frame$pred_outlier = -1
  }
  return (results_frame)
}

train_net = function(layers, test_threshold, nof_reps, algo,error_fct,act_fct,model) {
  # Einsatz des Neuronalen Netzes
  net_list = vector("list",net_count)
  for (net_idx in 1:net_count) {
    net = NULL
    if (model == "aggregated" || model == "aggregated_wo_outlier") {
      net = neuralnet(Quantile_0.5+Quantile_0.1+Quantile_0.9~OpTyp+DeltaOffset+Size, data = temp_train, hidden = layers, stepmax = 10000, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
    } else if(model == "aggregated_only_mean" || model == "aggregated_only_mean_wo_outlier") {
      net = neuralnet(Duration~OpTyp+DeltaOffset+Size, data = temp_train, hidden = layers, stepmax = 10000, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
    } else if(model == "tuple1" || model == "tuple1_without_outlier") {
      if (all_qs) {
        net = neuralnet(Duration+q0.1+q0.9~OpTyp+DeltaOffset+Size, data = temp_train, hidden = layers, stepmax = max_iter, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
      } else {
        net = neuralnet(Duration~OpTyp+DeltaOffset+Size, data = temp_train, hidden = layers, stepmax = max_iter, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
      }
    } else if(model == "tuple1_single") {
      net = neuralnet(Duration~OpTyp+DeltaOffset+Size, data = temp_train, hidden = layers, stepmax = max_iter, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
    } else if(model == "tuple2") {
      if (all_qs) {
        net = neuralnet(Duration+q0.1+q0.9~OpTyp+DeltaOffset+Size+Duration.1+OpTyp.1+DeltaOffset.1+Size.1, data = temp_train, hidden = layers, stepmax = max_iter, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
      } else {
        net = neuralnet(Duration~OpTyp+DeltaOffset+Size+Duration.1+OpTyp.1+DeltaOffset.1+Size.1, data = temp_train, hidden = layers, stepmax = max_iter, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
      }
    } else if(model == "triple") {
      if (all_qs) {
        net = neuralnet(Duration+q0.1+q0.9~OpTyp+DeltaOffset+Size+Duration.1+OpTyp.1+DeltaOffset.1+Size.1+Duration.2+OpTyp.2+DeltaOffset.2+Size.2, data = temp_train, hidden = layers, stepmax = max_iter, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
      } else {
        net = neuralnet(Duration~OpTyp+DeltaOffset+Size+Duration.1+OpTyp.1+DeltaOffset.1+Size.1+Duration.2+OpTyp.2+DeltaOffset.2+Size.2, data = temp_train, hidden = layers, stepmax = max_iter, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
      }
    } else if(model == "throughput_ema") {
      if (all_qs) {
        net = neuralnet(Duration+q0.1+q0.9~OpTyp+DeltaOffset+Size+throughput_ema, data = temp_train, hidden = layers, stepmax = max_iter, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
      } else {
        net = neuralnet(Duration~OpTyp+DeltaOffset+Size+throughput_ema, data = temp_train, hidden = layers, stepmax = max_iter, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
      }
    } else if(model == "general_pred_with_correction") {
      if (all_qs) {
        net = neuralnet(Duration+q0.1+q0.9~mean_Duration+rel_correction+last_error, data = temp_train, hidden = layers, stepmax = max_iter, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
      } else {
        net = neuralnet(Duration~mean_Duration+rel_correction+last_error, data = temp_train, hidden = layers, stepmax = max_iter, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
      }
    } else if(model == "tuple1_with_general_pred") {
      if (all_qs) {
        net = neuralnet(Duration+q0.1+q0.9~OpTyp+DeltaOffset+Size+mean_Duration, data = temp_train, hidden = layers, stepmax = max_iter, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
      } else {
        net = neuralnet(Duration~OpTyp+DeltaOffset+Size+mean_Duration, data = temp_train, hidden = layers, stepmax = max_iter, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
      }
    }else if(model == "tuple1_with_general_pred_with_correction") {
      if (all_qs) {
        net = neuralnet(Duration+q0.1+q0.9~OpTyp+DeltaOffset+Size+rel_correction+last_error+mean_Duration, data = temp_train, hidden = layers, stepmax = max_iter, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
      } else {
        net = neuralnet(Duration~OpTyp+DeltaOffset+Size+rel_correction+last_error+mean_Duration, data = temp_train, hidden = layers, stepmax = max_iter, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
      }
    } else if(model == "tuple1_with_outlier_information") {
      if (all_qs) {
        net = neuralnet(Duration+q0.1+q0.9~OpTyp+DeltaOffset+Size+known_outlier, data = temp_train, hidden = layers, stepmax = max_iter, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
      } else {
        net = neuralnet(Duration~OpTyp+DeltaOffset+Size+known_outlier, data = temp_train, hidden = layers, stepmax = max_iter, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
      }
    } else if(model == "rnn_tuple1") {
      if(algo == "elman") {
        net = elman(x = temp_train[c("Size","OpTyp","DeltaOffset")] , y = temp_train$Duration, size = layers, learnFuncParams = c(test_threshold), maxit = max_iter, linOut = TRUE)
      } else {
        net = jordan(x = temp_train[c("Size","OpTyp","DeltaOffset")] , y = temp_train$Duration, size = layers, learnFuncParams = c(test_threshold), maxit = max_iter, linOut = TRUE)
      }
    } else if(model == "rnn_tuple1_with_general_pred") {
      if(algo == "elman") {
        net = elman(x = temp_train[c("Size","OpTyp","DeltaOffset","mean_Duration")] , y = temp_train$Duration, size = layers, maxit = max_iter, learnFuncParams = c(test_threshold), linOut = TRUE)
      }
    }else if(model == "rnn_tuple1_with_general_pred_with_correction") {
      if(algo == "elman") {
        net = elman(x = temp_train[c("Size","OpTyp","DeltaOffset","rel_correction","last_error","mean_Duration")] , y = temp_train$Duration, size = layers, maxit = max_iter, learnFuncParams = c(test_threshold), linOut = TRUE)
      }
    } else if(model == "rnn_general_pred_with_correction") {
      if(algo == "elman") {
        net = elman(x = temp_train[c("rel_correction","last_error","mean_Duration")] , y = temp_train$Duration, learnFuncParams = c(test_threshold),size = layers, maxit = max_iter, linOut = TRUE)
      }
    }
    if ((is.null(net) & net_count == 1) || (type == "nn" & is.null(net$result.matrix) & net_count == 1)) {
      return (list(c(rep(-1,5),Inf,rep(-1,15)),-1))
    } else if (!is.null(net) || !(type == "nn" & is.null(net$result.matrix))) {
      net_list[[net_idx]] = net
    }
  }
  for (net_idx in length(net_list):1) {
    if(is.null(net_list[[net_idx]])) {
      if(length(net_list) == 1) {
        return (list(c(rep(-1,5),Inf,rep(-1,15)),-1))
      } else {
        net_list = net_list[-net_idx]
      }
    }
  }
  
  if (type == "nn") {
    results = data.frame(compute(net_list[[1]], temp_test)$net.result)
  } else {
    results = data.frame(predict(net_list[[1]], temp_test))
  }
  for (net_idx in min(2,length(net_list)):length(net_list)) {
    if (type == "nn") {
      this_results = data.frame(compute(net_list[[net_idx]], temp_test)$net.result)
    } else {
      this_results = data.frame(predict(net_list[[net_idx]], temp_test))
    }
    results = results + this_results
  }
  results = results / length(net_list)
  
  if (model == "aggregated" || model == "aggregated_wo_outlier") {
    d.results = data.frame(Duration = temp_target[1], pred_Quantile_0.1 = results$X2, pred_Quantile_0.5 = results$X1, pred_Quantile_0.9 = results$X3)
    
    # Resklaierung des Ergebnisses
    d.results$pred_Quantile_0.1 = d.results$pred_Quantile_0.1 * (longest_duration - shortest_duration) + shortest_duration
    d.results$pred_Quantile_0.5 = d.results$pred_Quantile_0.5 * (longest_duration - shortest_duration) + shortest_duration
    d.results$pred_Quantile_0.9 = d.results$pred_Quantile_0.9 * (longest_duration - shortest_duration) + shortest_duration
    
    d.results$error= d.results$pred_Quantile_0.5 - d.results$Duration
  } else {
    d.results = data.frame(Duration = temp_target[1], pred_Duration = results[1])
    names(d.results) = c("Duration","pred_Duration")
    
    # Resklaierung des Ergebnisses
    d.results$pred_Duration = d.results$pred_Duration * (longest_duration - shortest_duration) + shortest_duration
    
    d.results$error= d.results$pred_Duration - d.results$Duration
  }
  
  d.results = calc_all_qs(d.results,results,temp_target)
    
  if (all_qs) {    
    q0.1_rel_arith_mean = mean(d.results$q0.1_abspercent_error)
    q0.1_rel_rms = sqrt(mean(d.results$q0.1_abspercent_error^2))
    q0.9_rel_arith_mean = mean(d.results$q0.9_abspercent_error)
    q0.9_rel_rms = sqrt(mean(d.results$q0.9_abspercent_error^2))
    outlier_percent = 1 - sum(d.results$outlier_error) / nrow(d.results)
  } else {
    q0.1_rel_arith_mean = -1
    q0.1_rel_rms = -1
    q0.9_rel_arith_mean = -1
    q0.9_rel_rms = -1
    outlier_percent = -1
  }
  
  d.results$percent_error = (d.results$error / d.results$Duration)*100
  d.results$abspercent_error = abs(d.results$percent_error)
  
  harmonic_mean = 1/mean(abs(1/d.results$error))
  rel_harmonic_mean = 1/mean((1/d.results$abspercent_error))
  arith_mean = mean(abs(d.results$error))
  rel_arith_mean = mean(d.results$abspercent_error)
  rms = sqrt(mean(d.results$error^2)) #root mean square
  rel_rms = sqrt(mean(d.results$abspercent_error^2))
  rel_median_error = median(d.results$abspercent_error)
  rel_first_quartile_error = quantile(d.results$abspercent_error, 0.25, names = FALSE)
  rel_third_quartile_error = quantile(d.results$abspercent_error, 0.75, names = FALSE)
  max_rel_error = max(d.results$abspercent_error)
  
  # Test des Netzes auf den Trainingsdaten
  if (type == "nn") {
    results_on_training = data.frame(compute(net_list[[1]], temp_train_test)$net.result)
  } else {
    results_on_training = data.frame(predict(net_list[[1]], temp_train_test))
  }
  for (net_idx in min(2,length(net_list)):length(net_list)) {
    if (type == "nn") {
      this_results_on_training = data.frame(compute(net_list[[net_idx]], temp_train_test)$net.result)
    } else {
      this_results_on_training = data.frame(predict(net_list[[net_idx]], temp_train_test))
    }
    results_on_training = results_on_training + this_results_on_training
  }
  results_on_training = results_on_training / length(net_list)
  
  if (model == "aggregated" || model == "aggregated_wo_outlier") {
    d.results_on_training = data.frame(Duration = temp_train$Quantile_0.5, pred_Quantile_0.1 = results_on_training$X2, pred_Quantile_0.5 = results_on_training$X1, pred_Quantile_0.9 = results_on_training$X3)
    d.results_on_training$Duration = d.results_on_training$Duration * (longest_duration - shortest_duration) + shortest_duration
    d.results_on_training$pred_Quantile_0.5 = d.results_on_training$pred_Quantile_0.5 * (longest_duration - shortest_duration) + shortest_duration
    d.results_on_training$error= d.results_on_training$pred_Quantile_0.5 - d.results_on_training$Duration
  } else {
    d.results_on_training = data.frame(Duration = temp_train$Duration, pred_Duration = results_on_training[1])
    names(d.results_on_training) = c("Duration","pred_Duration")
    d.results_on_training$Duration = d.results_on_training$Duration * (longest_duration - shortest_duration) + shortest_duration
    d.results_on_training$pred_Duration = d.results_on_training$pred_Duration * (longest_duration - shortest_duration) + shortest_duration
    d.results_on_training$error= d.results_on_training$pred_Duration - d.results_on_training$Duration
  }
  
  if (model %in% c("aggregated","aggregated_wo_outlier","aggregated_only_mean","aggregated_only_mean_wo_outlier")) {
    to_calc = FALSE
  } else to_calc = TRUE
  d.results_on_training = calc_all_qs(d.results_on_training,results_on_training,temp_train,to_calc)
  
  d.results_on_training$percent_error = (d.results_on_training$error / d.results_on_training$Duration)*100
  d.results_on_training$abspercent_error = abs(d.results_on_training$percent_error)
  
  arith_mean_test_rel_error = mean(d.results_on_training$abspercent_error)  
  
  d.all_results = d.results
  
  if (model == "aggregated" || model == "aggregated_wo_outlier") {
    weigthed_arith_mean_test_rel_error = sum(d.results_on_training$abspercent_error * Count) / sum(Count)
  } else {
    weigthed_arith_mean_test_rel_error = -1
  }
  if(all_qs) {
    # liegt der vorhergesagte Wert zwischen dem tatächlichen 0.1 und 0.9 Quantil?
    results_copy = d.results
    results_copy$in_range = 0
    results_copy$in_range[results_copy$pred_Duration > results_copy$q0.1 & results_copy$pred_Duration < results_copy$q0.9] = 1
    mean_in_range = mean(results_copy$in_range)
  } else {
    mean_in_range = -1
  }
  if (!model %in% c("aggregated","aggregated_wo_outlier","aggregated_only_mean","aggregated_only_mean_wo_outlier")) {
    d.all_results = rbind(d.results,d.results_on_training)
    d.all_results = d.all_results[order(as.numeric(row.names(d.all_results))),]
  }
  if(type == "nn") {
    nof_steps = net$result.matrix[3]
  } else nof_steps = length(net$IterativeFitError)
  
  return (list(c(harmonic_mean,rel_harmonic_mean,arith_mean,rel_arith_mean,rms,rel_rms,
                 rel_median_error,rel_first_quartile_error,rel_third_quartile_error,max_rel_error,arith_mean_test_rel_error,
                 weigthed_arith_mean_test_rel_error,mean_in_range,nof_steps,
                 q0.1_rel_arith_mean,q0.1_rel_rms,q0.9_rel_arith_mean,q0.9_rel_rms,outlier_percent,net_count,length(net_list)),d.all_results))
}

          
#           #mean_harmonic_error = mean(return_values[1,])
#           mean_rel_harmonic_error = mean(return_values[2,])
#           best_rel_harmonic_error = min(return_values[2,])
#           mean_arith_error = mean(return_values[3,])
#           mean_rel_arith_error = mean(return_values[4,])
#           best_rel_arith_error = min(return_values[4,])
#           #mean_rms = mean(return_values[5,])
#           mean_rel_rms = mean(return_values[6,])
#           best_rel_rms = min(return_values[6,])
#           mean_median_rel_error = mean(return_values[7,])
#           mean_first_quartile_rel_error = mean(return_values[8,])
#           mean_third_quartile_rel_error = mean(return_values[9,])
#           max_max_rel_error = max(return_values[10,])
#           mean_arith_mean_test_rel_error = mean(return_values[11,])
#           mean_weigthed_arith_mean_test_rel_error = mean(return_values[12,])
#           mean_mean_in_range = mean(return_values[13,])
#           mean_steps = mean(return_values[14,])
#           arith_mean_rel_error_of_best_rel_rms_on_training_data = return_values[12,return_values[6,] == min(return_values[6,])]
#           
#           
#           return (c(harmonic_mean,
#           rel_harmonic_mean
#           ,arith_mean
#           ,rel_arith_mean
#           ,rms
#           ,rel_rms
#           ,rel_median_error
#           ,rel_first_quartile_error
#           ,rel_third_quartile_error
#           ,max_rel_error
#           ,arith_mean_test_rel_error
#           ,weigthed_arith_mean_test_rel_error
#           ,mean_in_range
#           ,net$result.matrix[3]))

