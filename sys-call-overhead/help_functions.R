
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
  }
  values = (c(mean_steps,mean_rel_harmonic_error,best_rel_harmonic_error,mean_arith_error,mean_rel_arith_error,best_rel_arith_error,mean_rel_rms,best_rel_rms,mean_median_rel_error,mean_first_quartile_rel_error,mean_third_quartile_rel_error,max_max_rel_error,mean_arith_mean_test_rel_error,arith_mean_rel_error_of_best_rel_rms_on_training_data,mean_weigthed_arith_mean_test_rel_error,mean_mean_in_range))
  return (values)
}

get_error_metrics = function(errors, test_type) {
  
  d.results = errors
  
  names(d.results) = c("error")
  d.results$percent_error = (d.results$error / d$Duration)*100
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
  
  # liegt der tatsächliche Wert zwischen dem vorhergesagten 0.1 und 0.9 Quantil?
  d.results$in_range = -1
  mean_in_range = mean(d.results$in_range)
  
  answer_list = c(test_type,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,
                  as.numeric(rel_harmonic_mean),as.numeric(rel_harmonic_mean),
                  as.numeric(arith_mean),as.numeric(rel_arith_mean),as.numeric(rel_arith_mean),as.numeric(rel_rms),as.numeric(rel_rms),
                  as.numeric(rel_median_error),as.numeric(rel_first_quartile_error),as.numeric(rel_third_quartile_error),as.numeric(max_rel_error),
                  as.numeric(arith_mean_test_rel_error),arith_mean_rel_error_of_best_rel_rms_on_training_data,as.numeric(weigthed_arith_mean_test_rel_error),as.numeric(mean_in_range))
  
  return (answer_list)
}

train_net = function(layers, test_threshold, nof_reps, algo,error_fct,act_fct,model) {
  # Einsatz des Neuronalen Netzes
  net = NULL
  if (model == "aggregated" || model == "aggregated_wo_outlier_norm") {
    net = neuralnet(Quantile_0.1+Quantile_0.5+Quantile_0.9~OpTyp+DeltaOffset+Size, data = temp_train, hidden = layers, stepmax = 10000, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
  } else if(model == "1tuple" || model == "1tuple_without_outlier") {
    #net = neuralnet(Duration~OpTyp+DeltaOffset+Size, data = temp_train, stepmax = 1, hidden = layers, threshold = 1, lifesign = "full")
    net = neuralnet(Duration~OpTyp+DeltaOffset+Size, data = temp_train, hidden = layers, stepmax = 500, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
  } else if(model == "2tuple") {
    #net = neuralnet(Duration~OpTyp+DeltaOffset+Size+Duration.1+OpTyp.1+DeltaOffset.1+Size.1, data = temp_train, stepmax = 1, hidden = layers, threshold = 1, lifesign = "full")
    net = neuralnet(Duration~OpTyp+DeltaOffset+Size+Duration.1+OpTyp.1+DeltaOffset.1+Size.1, data = temp_train, hidden = layers, stepmax = 500, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
  } else if(model == "triple") {
    net = neuralnet(Duration~OpTyp+DeltaOffset+Size+Duration.1+OpTyp.1+DeltaOffset.1+Size.1+Duration.2+OpTyp.2+DeltaOffset.2+Size.2, data = temp_train, hidden = layers, stepmax = 500, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
  } else if(model == "throughput_ema") {
    net = neuralnet(Duration~OpTyp+DeltaOffset+Size+throughput_ema, data = temp_train, hidden = layers, stepmax = 500, threshold = test_threshold, algorithm = algo, rep = nof_reps, err.fct = error_fct, act.fct = act_fct)
  }
  if (is.null(net) || is.null(net$result.matrix)) {
    return (rep(-1,14))
  }
  results = data.frame(compute(net, temp_test)$net.result)
  
  if (model == "aggregated" || model == "aggregated_wo_outlier_norm") {
    d.results = data.frame(Duration = temp_target[1], pred_Quantile_0.1 = results$X1, pred_Quantile_0.5 = results$X2, pred_Quantile_0.9 = results$X3)
    
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
  if (model == "aggregated" || model == "aggregated_wo_outlier_norm") {
    results_on_training = data.frame(compute(net, temp_train_test)$net.result)
    d.results_on_training = data.frame(Duration = temp_train$Quantile_0.5, pred_Quantile_0.1 = results_on_training$X1, pred_Quantile_0.5 = results_on_training$X2, pred_Quantile_0.9 = results_on_training$X3)
    d.results_on_training$Duration = d.results_on_training$Duration * (longest_duration - shortest_duration) + shortest_duration
    d.results_on_training$pred_Quantile_0.5 = d.results_on_training$pred_Quantile_0.5 * (longest_duration - shortest_duration) + shortest_duration
    d.results_on_training$error= d.results_on_training$pred_Quantile_0.5 - d.results_on_training$Duration
  } else {
    results_on_training = data.frame(compute(net, temp_train_test)$net.result)
    d.results_on_training = data.frame(Duration = temp_train$Duration, pred_Duration = results_on_training[1])
    names(d.results_on_training) = c("Duration","pred_Duration")
    d.results_on_training$Duration = d.results_on_training$Duration * (longest_duration - shortest_duration) + shortest_duration
    d.results_on_training$pred_Duration = d.results_on_training$pred_Duration * (longest_duration - shortest_duration) + shortest_duration
    d.results_on_training$error= d.results_on_training$pred_Duration - d.results_on_training$Duration
  }
  d.results_on_training$percent_error = (d.results_on_training$error / d.results_on_training$Duration)*100
  d.results_on_training$abspercent_error = abs(d.results_on_training$percent_error)
  
  arith_mean_test_rel_error = mean(d.results_on_training$abspercent_error)  
  
  if (model == "aggregated" || model == "aggregated_wo_outlier_norm") {
    weigthed_arith_mean_test_rel_error = sum(d.results_on_training$abspercent_error * Count) / sum(Count)
    # liegt der tatsächliche Wert zwischen dem vorhergesagten 0.1 und 0.9 Quantil?
    d.results$in_range = 0
    d.results$in_range[(d.results$Duration > d.results$pred_Quantile_0.1) & (d.results$Duration < d.results$pred_Quantile_0.9)] = 1
    mean_in_range = mean(d.results$in_range)
  } else {
    weigthed_arith_mean_test_rel_error = -1
    mean_in_range = -1
  }
  
  return (c(harmonic_mean,rel_harmonic_mean,arith_mean,rel_arith_mean,rms,rel_rms,rel_median_error,rel_first_quartile_error,rel_third_quartile_error,max_rel_error,arith_mean_test_rel_error,weigthed_arith_mean_test_rel_error,mean_in_range,net$result.matrix[3]))
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

