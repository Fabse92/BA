library("neuralnet")
library("foreach")
library("doParallel")
nof_cores = 6
registerDoParallel(cores=nof_cores)
library("cvTools")

source("preparation_of_data.R")
message("data prepared")
source("help_functions.R")

parameter_test = data.frame(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27)
names(parameter_test) = c("model","layers","neurons","reps","threshold","algo","error function","activation function","avg. training duration / sec","nof nets","nof errors","mean nof steps","mean_rel_harmonic_error","best_rel_harmonic_error","mean_arith_error","mean_rel_arith_error","best_rel_arith_error","mean_rel_rms","best_rel_rms","mean_median_rel_error","mean_first_quartile_rel_error","mean_third_quartile_rel_error","max_max_rel_error","mean_arith_mean_test_rel_error","arith_mean_rel_error_of_best_rel_rms_on_training_data","mean_weigthed_arith_mean_test_rel_error","mean_mean_in_range")
idx = 1

### Performance-Baselines evaluieren
#mittlere Performance als Modell
d.errors = data.frame(d$Duration - mean(d.without_outlier$Duration)) # mittlere Performance der Daten ohne Outlier
parameter_test[idx,] = get_error_metrics(d.errors, "mean performance")
idx = idx + 1

#lineare Regression nach size auf aggregierten Daten
fm = lm(formula = Quantile_0.5 ~ Size, data = d.aggregated)
#test = data.frame(predict(fm, data.frame(Size = d$Size)))
#test2 = data.frame(unname(coefficients(fm)["(Intercept)"]) + unname(coefficients(fm)[2]) * d$Size)
d.errors = data.frame(d$Duration - (unname(coefficients(fm)["(Intercept)"]) + unname(coefficients(fm)[2]) * d$Size ))
parameter_test[idx,] = get_error_metrics(d.errors, "lin reg: Size (aggregated)")
idx = idx + 1

#lineare Regression nach size
fm = lm(formula = d$Duration ~ d$Size)
d.errors = data.frame(d$Duration - fitted.values(fm))
parameter_test[idx,] = get_error_metrics(d.errors, "lin reg: Size")
idx = idx + 1

#Regression nach size + deltaoffset
fm = lm(formula = d$Duration ~ d$Size + d$DeltaOffset)
d.errors = data.frame(d$Duration - fitted.values(fm))
parameter_test[idx,] = get_error_metrics(d.errors, "lin reg: Size+Offset")
idx = idx + 1

#Regression nach size + deltaoffset + optyp
fm = lm(formula = d$Duration ~ d$Size + d$DeltaOffset +d$OpTyp)
d.errors = data.frame(d$Duration - fitted.values(fm))
parameter_test[idx,] = get_error_metrics(d.errors, "lin reg: Size+Offset+OpTyp")
idx = idx + 1

#Vorhersage der jeweiligen Durchschnittsperformance
d.errors = d
d.errors$pred_Duration = 0
for (i in 1:nrow(d.aggregated)) {
  d.errors$pred_Duration[d.errors$OpTyp == d.aggregated[i,"OpTyp"] & d.errors$DeltaOffset == d.aggregated[i,"DeltaOffset"] & d.errors$Size == d.aggregated[i,"Size"]] = d.aggregated$Quantile_0.5[i]
}
d.errors = data.frame(d.errors$Duration - d.errors$pred_Duration)
parameter_test[idx,] = get_error_metrics(d.errors, "mean Duration, aggregated")
idx = idx + 1

#Vorhersage der jeweiligen Durchschnittsperformance + Korrigierung
# d.errors = d#head(d,1000)
# d.errors$pred_Duration = 0
# for (i in 1:nrow(d.aggregated)) {
#   d.errors$pred_Duration[d.errors$OpTyp == d.aggregated[i,"OpTyp"] & d.errors$DeltaOffset == d.aggregated[i,"DeltaOffset"] & d.errors$Size == d.aggregated[i,"Size"]] = d.aggregated$Quantile_0.5[i]
# }
# d.errors$error = 0
# d.errors$normal = (d.errors$Duration - d.errors$pred_Duration)
# for (i in 1:nrow(d.errors)) {
#   if (i == 1) {
#     d.errors$error[i] = d.errors$Duration[i] - d.errors$pred_Duration[i]
#   } else {
#     correction = (d.errors$error[i-1]/d.errors$Duration[i-1]) / 2
#     d.errors$error[i] = d.errors$Duration[i] - (d.errors$pred_Duration[i] + correction * d.errors$pred_Duration[i])
#   }
# }
d.errors = d.errors2
d.errors = data.frame(d.errors$error)
parameter_test[idx,] = get_error_metrics(d.errors2, "mean Duration, aggregated + correction")
idx = idx + 1

message("baselines ready")

nof_iterations = 12 ### mindestens 2
for (nof_layers in 2:2) { ### Anzahl hidden-layers
  for (nof_neurons in 16:16) { ### Anzahl Neuronen pro hidden-layer
    layers = c(nof_neurons)
    for (idx_layer in 2:nof_layers) {
      layers = c(layers, nof_neurons)
    }
    for (nof_reps in 1:1) { ### Anzahl Wiederholungen des Trainingsprozesses
      for (threshold_exponent in 4:4) { ###  über 5 selten konvergent; über 4 Error
        for (threshold_summand in seq(5,5, by = 2)) { ### threshold des Algorithmus ist 10^(-threshold_exponent) + threshold_summand * 10^(-threshold_exponent);
          test_threshold = 10^(-threshold_exponent) + threshold_summand * 10^(-threshold_exponent)
          for (learning_algo in 2:2) { ### 1 - backprop, 2 - rprop+, 3- rprop-, 4 - sag, 5 - slr
            if (learning_algo == 1) {
              algo = "backprop"
            } else if (learning_algo == 2) {
              algo = "rprop+"
            } else if (learning_algo == 3) {
              algo = "rprop-"
            } else if (learning_algo == 4) {
              algo = "sag"
            } else if (learning_algo == 5) {
              algo = "slr"
            }
            for (error_fct_idx in 1:1) { ### 1 - sse (sum of squared errors), 2 - ce (cross entropy)
              if (error_fct_idx == 1) {
                error_fct = "sse" 
              } else if (error_fct_idx == 2) {
                error_fct = "ce"
              }
              for (act_fct_idx in 1:1) { ### 1 - logistic, 2 - tanh
                if (act_fct_idx == 1) {
                  act_fct = "logistic" 
                } else if (act_fct_idx == 2) {
                  act_fct = "tanh"
                }
                for (model_idx in 0:1) { ###
                  if (model_idx == 0) {
                    model = "aggregated_wo_outlier_norm"
                    nof_folds = 1 ### 
                    train_data <- d.aggregated_wo_outlier_norm
                    train_test_data = d.aggregated_wo_outlier_norm[c("OpTyp","DeltaOffset", "Size")]
                    test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset", "Size")],2,normalize))
                    test_target_data <- d[c("Duration")]
                    Count = d.aggregated_wo_outlier$Count
                  } else if (model_idx == 1) {
                    model = "aggregated"
                    nof_folds = 1 ### 
                    train_data <- d.aggregated_norm
                    train_test_data = d.aggregated_norm[c("OpTyp","DeltaOffset", "Size")]
                    test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset", "Size")],2,normalize))
                    test_target_data <- d[c("Duration")]
                    Count = d.aggregated$Count
                  } else if (model_idx == 2) { #good parameter: threshold ~ 0.008, algo = rprop+, neurons ~ 14
                    model = "1tuple"
                    nof_folds = 10 ###
                    train_data <- data.frame(apply(d[c("Duration","OpTyp","DeltaOffset","Size")],2,normalize))
                    train_test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset","Size")],2,normalize))
                    test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset","Size")],2,normalize))
                    test_target_data <- d[c("Duration")]
                  } else if (model_idx == 3) {
                    model = "2tuple"
                    nof_folds = 10 ###
                    train_data <- d.tuple_norm[c("Duration","OpTyp","DeltaOffset","Size","Duration.1","OpTyp.1","DeltaOffset.1","Size.1")]
                    train_test_data <- d.tuple_norm[c("OpTyp","DeltaOffset","Size","Duration.1","OpTyp.1","DeltaOffset.1","Size.1")]
                    test_data <- d.tuple_norm[c("OpTyp","DeltaOffset","Size","Duration.1","OpTyp.1","DeltaOffset.1","Size.1")]
                    test_target_data <- d.tuple[c("Duration")]
                  } else if (model_idx == 4) {
                    model = "triple"
                    nof_folds = 10 ###
                    train_data <- d.triple_norm[c("Duration","OpTyp","DeltaOffset","Size","Duration.1","OpTyp.1","DeltaOffset.1","Size.1","Duration.2","OpTyp.2","DeltaOffset.2","Size.2")]
                    train_test_data <- d.triple_norm[c("OpTyp","DeltaOffset","Size","Duration.1","OpTyp.1","DeltaOffset.1","Size.1","Duration.2","OpTyp.2","DeltaOffset.2","Size.2")]
                    test_data <- d.triple_norm[c("OpTyp","DeltaOffset","Size","Duration.1","OpTyp.1","DeltaOffset.1","Size.1","Duration.2","OpTyp.2","DeltaOffset.2","Size.2")]
                    test_target_data <- d.triple[c("Duration")]
                  } else if (model_idx == 5) {
                    model = "1tuple_without_outlier"
                    nof_folds = 10 ###
                    train_data <- data.frame(apply(d[c("Duration","OpTyp","DeltaOffset","Size")],2,normalize)) ## die outlier werden bei den folds herausgefiltert
                    train_test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset","Size")],2,normalize))
                    test_data <- data.frame(apply(d[c("OpTyp","DeltaOffset","Size")],2,normalize))
                    test_target_data <- d[c("Duration")]
                  } else if (model_idx == 6) {
                    model = "throughput_ema"
                    nof_folds = 10 ###
                    train_data <- d.ema_norm[c("Duration","OpTyp","DeltaOffset","Size","throughput_ema")]
                    train_test_data <- d.ema_norm[c("OpTyp","DeltaOffset","Size","throughput_ema")]
                    test_data <- d.ema_norm[c("OpTyp","DeltaOffset","Size","throughput_ema")]
                    test_target_data <- d.ema[c("Duration")]
                  }
                  fold_iterations = nof_iterations #/ nof_folds #### Achtung, bei kleinen nof_iterations
                  if (nof_folds > 1) {
                    folds = cvFolds(nrow(train_data), K = nof_folds, R = 1,type = "random") ### welcher type?
                  }
                  for (fold_idx in 1:1) {##### nof_folds) {
                    if (model == "aggregated" || model == "aggregated_wo_outlier_norm") {
                      temp_train = train_data
                      temp_train_test = train_test_data
                      temp_test = test_data
                      temp_target = test_target_data
                    } else if (model == "1tuple_without_outlier") { 
                      temp_train = train_data[d$outlier == 0,][folds$subsets[folds$which == fold_idx], ]
                      temp_train = temp_train[is.na(temp_train$OpTyp) == FALSE,]
                      temp_train_test = train_test_data[d$outlier == 0,][folds$subsets[folds$which == fold_idx], ]
                      temp_train_test = temp_train_test[is.na(temp_train_test$OpTyp) == FALSE,]
#                       temp_train_rest = train_data[d$outlier == 1,][folds$subsets[folds$which == fold_idx], ]
#                       temp_train_rest = temp_train_rest[is.na(temp_train_rest$OpTyp) == FALSE,]
#                       temp_train_test_rest = train_test_data[d$outlier == 1,][folds$subsets[folds$which == fold_idx], ]
#                       temp_train_test_rest = temp_train_test_rest[is.na(temp_train_test_rest$OpTyp) == FALSE,]
                      temp_test = test_data[folds$subsets[folds$which != fold_idx], ]
                      temp_target = data.frame(test_target_data[folds$subsets[folds$which != fold_idx], ])
                    } else { 
                      temp_train = train_data[folds$subsets[folds$which == fold_idx], ]
                      temp_train_test = train_test_data[folds$subsets[folds$which == fold_idx], ]
                      temp_test = test_data[folds$subsets[folds$which != fold_idx], ]
                      temp_target = data.frame(test_target_data[folds$subsets[folds$which != fold_idx], ])
                    }
                    fold_time <- system.time({
                      fold_values <- foreach(icount(fold_iterations), .combine='cbind', .inorder=FALSE) %dopar% {
                        train_net(layers,test_threshold,nof_reps,algo,error_fct,act_fct,model)
                      }
                    })
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

for (idx in 2:5) {
  parameter_test[,idx] = as.numeric(parameter_test[,idx])
}
for (idx in 9:27) {
  parameter_test[,idx] = as.numeric(parameter_test[,idx])
}



parameter_test$model[parameter_test$model == "aggregated"] = 0
parameter_test$model[parameter_test$model == "1tuple"] = 1
parameter_test$model[parameter_test$model == "2tuple"] = 2
parameter_test$model[parameter_test$model == "triple"] = 3

plot(mypara$model, mypara$mean_rel_rms, xlim = c(1,3), ylim = c(0,100))
plot(parameter_test$threshold,parameter_test$mean_rel_rms, ylim = c(0,100), xlim = c(0,0.01))


# Plot der echten Duration gegenüber der vorhergesagten
plot(d.results$Duration, col="blue", ylim=c(-0.0001, 0.0004))
points(d.results$pred_Duration, col="red")

# sorted = d.results[order(as.numeric(row.names(d.results))),]
# plot(sorted$Duration, col="blue", ylim=c(-0.0001,0.00015), pch = c(1,rep(NA,10)))
# points(sorted$pred_Duration, col="red",  pch = c(1,rep(NA,10)))
# 
#  d.results$in_range = 0
#  d.results$in_range[(d.results$Duration > d.results$pred_Quantile_0.1) & (d.results$Duration < d.results$pred_Quantile_0.9)] = 1
#  mean(d.results$in_range)
# plot(d.results$in_range)


 write.table(parameter_test, "different_models2", sep=",", row.names = FALSE)

for (col in 2:5) {
  message(cor(parameter_test$mean_rel_rms,parameter_test[col]))
}

for (col in 9:12) {
  message(cor(parameter_test$mean_rel_rms,parameter_test[col]))
}
 
for (i in seq(1,10, by = 2)) {
  message(i)
}

