load("../images/prepared_data")

# library("neuralnet")
# library("foreach")
# library("doParallel")
# nof_cores = 12
# registerDoParallel(cores=nof_cores)
# library("cvTools")

#source("preparation_of_data.R")
#message("data prepared")
source("../help_functions.R")

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

#lineare Regression pro OpTyp nach size
d.reads = d[d$OpTyp == 1,]
d.writes = d[d$OpTyp == 2,]
fm_reads = lm(formula = d.reads$Duration ~ d.reads$Size)
fm_writes = lm(formula = d.writes$Duration ~ d.writes$Size)
d.errors_reads = data.frame(d.reads$Duration - fitted.values(fm_reads))
names(d.errors_reads) = "error"
d.errors_writes = data.frame(d.writes$Duration - fitted.values(fm_writes))
names(d.errors_writes) = "error"
d.errors = data.frame(rbind(d.errors_reads, d.errors_writes))
parameter_test[idx,] = get_error_metrics(d.errors, "lin reg (per OpTyp): Size")
idx = idx + 1

#Vorhersage des jeweiligen Means
d.errors = merge(d,d.aggregated,by=c("OpTyp","DeltaOffset","Size"),all = TRUE,sort = FALSE)
names(d.errors)[names(d.errors) == "mean_Duration"] = "pred_Duration"
d.errors = data.frame(d.errors$Duration - d.errors$pred_Duration)
parameter_test[idx,] = get_error_metrics(d.errors, "mean Duration, aggregated")
idx = idx + 1

#Vorhersage des jeweiligen Medians
d.errors = merge(d,d.aggregated,by=c("OpTyp","DeltaOffset","Size"),all = TRUE,sort = FALSE)
names(d.errors)[names(d.errors) == "Quantile_0.5"] = "pred_Duration"
d.errors = data.frame(d.errors$Duration - d.errors$pred_Duration)
parameter_test[idx,] = get_error_metrics(d.errors, "median Duration, aggregated")
idx = idx + 1

#Vorhersage des jeweiligen Means + Korrigierung
d.errors = d.general_pred
d.errors = data.frame(d.errors$mean_Duration + d.errors$mean_Duration * d.errors$rel_correction * 0.5)
parameter_test[idx,] = get_error_metrics(d.errors, "mean Duration, aggregated + correction",d.general_pred)
idx = idx + 1

#Vorhersage des jeweiligen Medians + Korrigierung
# d.errors = merge(d,d.aggregated,by=c("OpTyp","DeltaOffset","Size"),all = TRUE,sort = FALSE)
# names(d.errors)[names(d.errors) == "Quantile_0.5"] = "pred_Duration"
# 
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
# d.errors = data.frame(d.errors$error)
# parameter_test[idx,] = get_error_metrics(d.errors, "median Duration, aggregated + correction")
# idx = idx + 1

message("baselines ready")

save.image("../images/prepared_with_baselines")

write.table(parameter_test, "../parameter_tests/parameter_test-baselines", sep=",", row.names = FALSE)
