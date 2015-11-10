
plot_every = 10
png_width = 6.5
png_height = 5

#####################################################################################################################################################################################

d.read_seq = read.table("../data-cached-off0-seq-R.csv", header = T, sep=",", nrows = 200000)
names(d.read_seq)[2] = "Size"
d.read_seq$OpTyp = 1
last_Offset = head(d.read_seq$Offset,nrow(d.read_seq)-1)
last_Offset = c(0,last_Offset)
d.read_seq$DeltaOffset = d.read_seq$Offset - last_Offset
d.read_seq$throughput = d.read_seq$Size / d.read_seq$Duration
d.read_seq$index = 1:nrow(d.read_seq)

d.write_seq = read.table("../data-cached-off0-seq-W.csv", header = T, sep=",", nrows = 200000)
names(d.write_seq)[2] = "Size"
d.write_seq$OpTyp = 2
last_Offset = head(d.write_seq$Offset,nrow(d.write_seq)-1)
last_Offset = c(0,last_Offset)
d.write_seq$DeltaOffset = d.write_seq$Offset - last_Offset
d.write_seq$throughput = d.write_seq$Size / d.write_seq$Duration
d.write_seq$index = 1:nrow(d.write_seq)

#####################################################################################################################################################################################

d.read_rnd = read.table("../data-cached-off0-rnd-R.csv", header = T, sep=",", nrows = 200000)
names(d.read_rnd)[2] = "Size"
d.read_rnd$OpTyp = 1
d.read_rnd$throughput = d.read_rnd$Size / d.read_rnd$Duration
d.read_rnd$index = 1:nrow(d.read_rnd)

d.write_rnd = read.table("../data-cached-off0-rnd-W.csv", header = T, sep=",", nrows = 200000)
names(d.write_rnd)[2] = "Size"
d.write_rnd$OpTyp = 2
d.write_rnd$throughput = d.write_rnd$Size / d.write_rnd$Duration
d.write_rnd$index = 1:nrow(d.write_rnd)

#####################################################################################################################################################################################
data = list(d.read_seq,d.write_seq,d.read_rnd,d.write_rnd)
data_names = c("read_seq","write_seq","read_rnd","write_rnd")

idx = 1
this_data = data[[idx]]
data_summary = data.frame(1,2,3,4,5,6,7,8)
names(data_summary) = c("Attribut","Min","Quartil1","Median","Mittel","Quartil3","Max","Korrelation")
data_summary[1,] = c("Dauer",min(this_data$Duration),quantile(this_data$Duration,probs = 0.25), median(this_data$Duration), mean(this_data$Duration), quantile(this_data$Duration,probs = 0.75),max(this_data$Duration),cor(this_data$Duration,this_data$Duration))
data_summary[2,] = c("Größe",min(this_data$Size),quantile(this_data$Size,probs = 0.25), median(this_data$Size), mean(this_data$Size), quantile(this_data$Size,probs = 0.75),max(this_data$Size),cor(this_data$Duration,this_data$Size))
data_summary[3,] = c("Delta-Abstand",min(this_data$Delta),quantile(this_data$Delta,probs = 0.25), median(this_data$Delta), mean(this_data$Delta), quantile(this_data$Delta,probs = 0.75),max(this_data$Delta),cor(this_data$Duration,this_data$Delta))
for (idx in 2:ncol(data_summary)) {
  data_summary[,idx] = format(as.numeric(data_summary[,idx]),digits = 2)
}
write.table(data_summary, paste("../csv/data_summary_read_seq.csv"), sep=",", row.names = FALSE,quote = FALSE)

idx = 2
this_data = data[[idx]]
data_summary = data.frame(1,2,3,4,5,6,7,8)
names(data_summary) = c("Attribut","Min","Quartil1","Median","Mittel","Quartil3","Max","Korrelation")
data_summary[1,] = c("Dauer",min(this_data$Duration),quantile(this_data$Duration,probs = 0.25), median(this_data$Duration), mean(this_data$Duration), quantile(this_data$Duration,probs = 0.75),max(this_data$Duration),cor(this_data$Duration,this_data$Duration))
data_summary[2,] = c("Größe",min(this_data$Size),quantile(this_data$Size,probs = 0.25), median(this_data$Size), mean(this_data$Size), quantile(this_data$Size,probs = 0.75),max(this_data$Size),cor(this_data$Duration,this_data$Size))
data_summary[3,] = c("Delta-Abstand",min(this_data$Delta),quantile(this_data$Delta,probs = 0.25), median(this_data$Delta), mean(this_data$Delta), quantile(this_data$Delta,probs = 0.75),max(this_data$Delta),cor(this_data$Duration,this_data$Delta))
for (idx in 2:ncol(data_summary)) {
  data_summary[,idx] = format(as.numeric(data_summary[,idx]),digits = 2)
}
write.table(data_summary, paste("../csv/data_summary_write_seq.csv"), sep=",", row.names = FALSE,quote = FALSE)

idx = 3
this_data = data[[idx]]
data_summary = data.frame(1,2,3,4,5,6,7,8)
names(data_summary) = c("Attribut","Min","Quartil1","Median","Mittel","Quartil3","Max","Korrelation")
data_summary[1,] = c("Dauer",min(this_data$Duration),quantile(this_data$Duration,probs = 0.25), median(this_data$Duration), mean(this_data$Duration), quantile(this_data$Duration,probs = 0.75),max(this_data$Duration),cor(this_data$Duration,this_data$Duration))
data_summary[2,] = c("Größe",min(this_data$Size),quantile(this_data$Size,probs = 0.25), median(this_data$Size), mean(this_data$Size), quantile(this_data$Size,probs = 0.75),max(this_data$Size),cor(this_data$Duration,this_data$Size))
data_summary[3,] = c("Delta-Abstand",min(this_data$Delta),quantile(this_data$Delta,probs = 0.25), median(this_data$Delta), mean(this_data$Delta), quantile(this_data$Delta,probs = 0.75),max(this_data$Delta),cor(this_data$Duration,this_data$Delta))
for (idx in 2:ncol(data_summary)) {
  data_summary[,idx] = format(as.numeric(data_summary[,idx]),digits = 2)
}
write.table(data_summary, paste("../csv/data_summary_read_rnd.csv"), sep=",", row.names = FALSE,quote = FALSE)

idx = 4
this_data = data[[idx]]
data_summary = data.frame(1,2,3,4,5,6,7,8)
names(data_summary) = c("Attribut","Min","Quartil1","Median","Mittel","Quartil3","Max","Korrelation")
data_summary[1,] = c("Dauer",min(this_data$Duration),quantile(this_data$Duration,probs = 0.25), median(this_data$Duration), mean(this_data$Duration), quantile(this_data$Duration,probs = 0.75),max(this_data$Duration),cor(this_data$Duration,this_data$Duration))
data_summary[2,] = c("Größe",min(this_data$Size),quantile(this_data$Size,probs = 0.25), median(this_data$Size), mean(this_data$Size), quantile(this_data$Size,probs = 0.75),max(this_data$Size),cor(this_data$Duration,this_data$Size))
data_summary[3,] = c("Delta-Abstand",min(this_data$Delta),quantile(this_data$Delta,probs = 0.25), median(this_data$Delta), mean(this_data$Delta), quantile(this_data$Delta,probs = 0.75),max(this_data$Delta),cor(this_data$Duration,this_data$Delta))
for (idx in 2:ncol(data_summary)) {
  data_summary[,idx] = format(as.numeric(data_summary[,idx]),digits = 2)
}
write.table(data_summary, paste("../csv/data_summary_write_rnd.csv"), sep=",", row.names = FALSE,quote = FALSE)

#########

this_data = rbind(d.read_seq,d.write_seq)
data_summary = data.frame(1,2,3,4,5,6,7,8)
names(data_summary) = c("Attribut","Min","Quartil1","Median","Mittel","Quartil3","Max","Korrelation")
data_summary[1,] = c("OpTyp",min(this_data$OpTyp),quantile(this_data$OpTyp,probs = 0.25), median(this_data$OpTyp), mean(this_data$OpTyp), quantile(this_data$OpTyp,probs = 0.75),max(this_data$OpTyp),cor(this_data$Duration,this_data$OpTyp))
for (idx in 2:ncol(data_summary)) {
  data_summary[,idx] = format(as.numeric(data_summary[,idx]),digits = 2)
}
write.table(data_summary, paste("../csv/data_summary_seq_optyp.csv"), sep=",", row.names = FALSE,quote = FALSE)

this_data = rbind(d.read_rnd,d.write_rnd)
data_summary = data.frame(1,2,3,4,5,6,7,8)
names(data_summary) = c("Attribut","Min","Quartil1","Median","Mittel","Quartil3","Max","Korrelation")
data_summary[1,] = c("OpTyp",min(this_data$OpTyp),quantile(this_data$OpTyp,probs = 0.25), median(this_data$OpTyp), mean(this_data$OpTyp), quantile(this_data$OpTyp,probs = 0.75),max(this_data$OpTyp),cor(this_data$Duration,this_data$OpTyp))
for (idx in 2:ncol(data_summary)) {
  data_summary[,idx] = format(as.numeric(data_summary[,idx]),digits = 2)
}
write.table(data_summary, paste("../csv/data_summary_rnd_optyp.csv"), sep=",", row.names = FALSE,quote = FALSE)

#####################################################################################################################################################################################

plot_titles = c("Laufzeiten: cached-off0-seq (Lesend)","Laufzeiten: cached-off0-seq (Schreibend)","Laufzeiten: cached-off0-rnd (Lesend)","Laufzeiten: cached-off0-rnd (Schreibend)")
for (idx in 1:length(plot_titles)) {
  sorted_best_data = data[[idx]]
  
  png(filename=paste("../plots/plot_","Duration_",data_names[idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(sorted_best_data$Duration, col="gray28",pch = c(20, rep(NA, plot_every)),ylim = c(quantile(sorted_best_data$Duration,0.01,names = FALSE),quantile(sorted_best_data$Duration,0.99,names = FALSE)),
       xlab = "Index", ylab = "Dauer in Sekunden", main = plot_titles[idx])
  dev.off()
}

getlim = rbind(data[[1]],data[[2]],data[[3]],data[[4]])
limes = c(quantile(getlim$Duration,0.01,names = FALSE),quantile(getlim$Duration,0.99,names = FALSE))

plot_titles = c("Sortierte Laufzeiten: cached-off0-seq (Lesend)","Sortierte Laufzeiten: cached-off0-seq (Schreibend)","Sortierte Laufzeiten: cached-off0-rnd (Lesend)","Sortierte Laufzeiten: cached-off0-rnd (Schreibend)")
for (idx in 1:length(plot_titles)) {
  sorted_best_data = data[[idx]][order(data[[idx]]$Duration),]
  
  png(filename=paste("../plots/plot_","DurationSorted_",data_names[idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(sorted_best_data$Duration, col="gray28",pch = c(20, rep(NA, plot_every)),ylim = limes,
      xlab = "Index", ylab = "Dauer in Sekunden (logarithmisch)", main = plot_titles[idx], log = "y")
  dev.off()
}

plot_titles = c("Sortierte Laufzeiten: cached-off0-seq (Lesend)","Sortierte Laufzeiten: cached-off0-seq (Schreibend)","Sortierte Laufzeiten: cached-off0-rnd (Lesend)","Sortierte Laufzeiten: cached-off0-rnd (Schreibend)")
for (idx in 1:length(plot_titles)) {
  sorted_best_data = data[[idx]][order(data[[idx]]$Duration),]
  
  png(filename=paste("../plots/plot_","DurationSorted_",data_names[idx],"_nolog.png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(sorted_best_data$Duration, col="gray28",pch = c(20, rep(NA, plot_every)),ylim = limes,
       xlab = "Index", ylab = "Dauer in Sekunden", main = plot_titles[idx])
  dev.off()
}

idx = 4
sort(unique(data[[idx]]$Size))

colors = c("chartreuse","aquamarine","chocolate1","darkblue","coral","darkgoldenrod2","darkorchid2","red","chartreuse4","chocolate4","coral3","cyan1","darkred","mediumpurple","deeppink1","black")

plot_titles = c("Sortierte Zugriffsgrößen: cached-off0-seq (Lesend)","Sortierte Zugriffsgrößen: cached-off0-seq (Schreibend)","Sortierte Zugriffsgrößen: cached-off0-rnd (Lesend)","Sortierte Zugriffsgrößen: cached-off0-rnd (Schreibend)")
for (idx in 1:length(plot_titles)) {
  sorted_best_data = data[[idx]][order(data[[idx]]$Size),]
  sorted_best_data$sorted_idx = 1:nrow(sorted_best_data)
  sizes = unique(sorted_best_data$Size)
  
  png(filename=paste("../plots/plot_","SizeSorted_",data_names[idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(sorted_best_data$Duration, col="gray28",pch = c(rep(NA, 1)),ylim = limes,
       xlab = "Index", ylab = "Dauer in Sekunden (logarithmisch)", main = plot_titles[idx], log = "y")
  for (size_idx in 1:length(sizes)) {
    points(y = sorted_best_data$Duration[sorted_best_data$Size == sizes[size_idx]], x = sorted_best_data$sorted_idx[sorted_best_data$Size == sizes[size_idx]],
           col = colors[size_idx], pch = c(20, rep(NA, plot_every)))
  }
  # legend(x = "bottomright",legend = sizes)
  dev.off()
}

plot_titles = c("Punkte 1-250: cached-off0-seq (Lesend)","Punkte 1-250: cached-off0-seq (Schreibend)","Punkte 1-250: cached-off0-rnd (Lesend)","Punkte 1-250: cached-off0-rnd (Schreibend)")
for (idx in 1:length(plot_titles)) {
  sorted_best_data = data[[idx]]
  sorted_best_data = sorted_best_data[1:250,]
  
  png(filename=paste("../plots/plot_","First250_",data_names[idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(sorted_best_data$Duration, col="gray28",#ylim = c(quantile(sorted_best_data$Duration,0.01,names = FALSE),quantile(sorted_best_data$Duration,0.99,names = FALSE)),
       xlab = "Index", ylab = "Dauer in Sekunden", main = plot_titles[idx], type = "l")
  dev.off()
}

idx = 2
sorted_best_data = data[[idx]]
sorted_best_data = sorted_best_data[1:246,]
#plot(sorted_best_data$Duration)
#max(sorted_best_data$Duration)
sorted_best_data$start = sorted_best_data$Duration == 3.9004e-05
#123
sorted_best_data = sorted_best_data[1:246,]
sorted_best_data$pred_Dur = sorted_best_data$Duration
sorted_best_data$pred_Dur[123:245] = sorted_best_data$pred_Dur[1:123]
plot(sorted_best_data$pred_Dur)

png(filename=paste("../plots/plot_","periodicity",data_names[idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
plot(sorted_best_data$pred_Dur, col="dodgerblue3",#ylim = c(quantile(sorted_best_data$Duration,0.01,names = FALSE),quantile(sorted_best_data$Duration,0.99,names = FALSE)),
     xlab = "Index", ylab = "Dauer in Sekunden", main = plot_titles[idx], type = "l")
lines(sorted_best_data$Duration,  col="gray28" )
dev.off()


plot_titles = c("Punkte 100001-100250: cached-off0-seq (Lesend)","Punkte 100001-100250: cached-off0-seq (Schreibend)","Punkte 100001-100250: cached-off0-rnd (Lesend)","Punkte 100001-100250: cached-off0-rnd (Schreibend)")
for (idx in 1:length(plot_titles)) {
  sorted_best_data = data[[idx]]
  sorted_best_data = sorted_best_data[100001:100250,]
  
  png(filename=paste("../plots/plot_","From100001to100250_",data_names[idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(y = sorted_best_data$Duration, col="gray28", x = 100001:100250, ylim = c(0,max(sorted_best_data$Duration)*1.05), #ylim = c(quantile(sorted_best_data$Duration,0.01,names = FALSE),quantile(sorted_best_data$Duration,0.99,names = FALSE)),
       xlab = "Index", ylab = "Dauer in Sekunden", main = plot_titles[idx], type = "l")
  dev.off()
}

idx = 1
sorted_best_data = data[[idx]]
sorted_best_data = sorted_best_data[100001:100250,]
#plot(sorted_best_data$Duration)
max(sorted_best_data$Duration)
sorted_best_data$start = sorted_best_data$Duration == 0.02098331
#129
#sorted_best_data = sorted_best_data[1:250,]
sorted_best_data$pred_Dur = sorted_best_data$Duration
sorted_best_data$pred_Dur[129:250] = sorted_best_data$pred_Dur[1:122]
plot(sorted_best_data$pred_Dur)

png(filename=paste("../plots/plot_","periodicity100001",data_names[idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
plot(sorted_best_data$pred_Dur, col="dodgerblue3",#ylim = c(quantile(sorted_best_data$Duration,0.01,names = FALSE),quantile(sorted_best_data$Duration,0.99,names = FALSE)),
     xlab = "Index", ylab = "Dauer in Sekunden", main = plot_titles[idx], type = "l")
lines(sorted_best_data$Duration,  col="gray28" )
dev.off()

#####################################################################################################################################################################################
#####################################################################################################################################################################################

sect1 = intersect(unique(data[[1]]$Size),unique(data[[2]]$Size))
sect2 = intersect(unique(data[[3]]$Size),unique(data[[4]]$Size))
intersect(sect1,sect2)

plot_titles = c(rep("Zugriffsgröße 1 KB",4))
plot_titles[1] = paste(plot_titles[1],": cached-off0-seq (Lesend)",sep ="")
plot_titles[2] = paste(plot_titles[2],": cached-off0-seq (Schreibend)",sep ="")
plot_titles[3] = paste(plot_titles[3],": cached-off0-rnd (Lesend)",sep ="")
plot_titles[4] = paste(plot_titles[4],": cached-off0-rnd (Schreibend)",sep ="")

for (idx in 1:length(plot_titles)) {
  sorted_best_data = data[[idx]]
  sorted_best_data = sorted_best_data[sorted_best_data$Size == 1,]
  message(min(sorted_best_data$Duration)/max(sorted_best_data$Duration))
  #sorted_best_data = sorted_best_data[1:1000,]
  
  png(filename=paste("../plots/plot_","Size1_",data_names[idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(sorted_best_data$Duration, col="gray28",pch = c(20, rep(NA, plot_every)), ylim = c(0,max(sorted_best_data$Duration)*1.05), #ylim = c(quantile(sorted_best_data$Duration,0.01,names = FALSE),quantile(sorted_best_data$Duration,0.99,names = FALSE)),
       xlab = "Index", ylab = "Dauer in Sekunden", main = plot_titles[idx], type = "l")
  dev.off()
}

plot_titles = c(rep("Zugriffsgröße 16384 KB",4))
plot_titles[1] = paste(plot_titles[1],": cached-off0-seq (Lesend)",sep ="")
plot_titles[2] = paste(plot_titles[2],": cached-off0-seq (Schreibend)",sep ="")
plot_titles[3] = paste(plot_titles[3],": cached-off0-rnd (Lesend)",sep ="")
plot_titles[4] = paste(plot_titles[4],": cached-off0-rnd (Schreibend)",sep ="")

for (idx in 1:length(plot_titles)) {
  sorted_best_data = data[[idx]]
  sorted_best_data = sorted_best_data[sorted_best_data$Size == 16384,]
  message(min(sorted_best_data$Duration)/max(sorted_best_data$Duration))
  #sorted_best_data = sorted_best_data[1:1000,]
  
#   png(filename=paste("../plots/density_","Size16384_",data_names[idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
#   plot(density(sorted_best_data$Duration),col="gray28",#ylim = c(0,max(sorted_best_data$Duration)*1.05), #ylim = c(quantile(sorted_best_data$Duration,0.01,names = FALSE),quantile(sorted_best_data$Duration,0.99,names = FALSE)),
#        xlab = "Index", ylab = "Dauer in Sekunden", main = plot_titles[idx])
#   dev.off()
  
  png(filename=paste("../plots/plot_","Size16384_",data_names[idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(sorted_best_data$Duration, col="gray28",pch = c(20, rep(NA, plot_every)), ylim = c(0,max(sorted_best_data$Duration)*1.05), #ylim = c(quantile(sorted_best_data$Duration,0.01,names = FALSE),quantile(sorted_best_data$Duration,0.99,names = FALSE)),
       xlab = "Index", ylab = "Dauer in Sekunden", main = plot_titles[idx], type = "l")
  dev.off()
}

plot_titles = c(rep("Zugriffsgröße 2097152 KB",4))
plot_titles[1] = paste(plot_titles[1],": cached-off0-seq (Lesend)",sep ="")
plot_titles[2] = paste(plot_titles[2],": cached-off0-seq (Schreibend)",sep ="")
plot_titles[3] = paste(plot_titles[3],": cached-off0-rnd (Lesend)",sep ="")
plot_titles[4] = paste(plot_titles[4],": cached-off0-rnd (Schreibend)",sep ="")

for (idx in 1:length(plot_titles)) {
  sorted_best_data = data[[idx]]
  sorted_best_data = sorted_best_data[sorted_best_data$Size == 2097152,]
  message(min(sorted_best_data$Duration)/max(sorted_best_data$Duration))
  #sorted_best_data = sorted_best_data[1:1000,]
  
  png(filename=paste("../plots/plot_","Size2097152_",data_names[idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(sorted_best_data$Duration, col="gray28",pch = c(20, rep(NA, plot_every)), ylim = c(0,max(sorted_best_data$Duration)*1.05), #ylim = c(quantile(sorted_best_data$Duration,0.01,names = FALSE),quantile(sorted_best_data$Duration,0.99,names = FALSE)),
       xlab = "Index", ylab = "Dauer in Sekunden", main = plot_titles[idx], type = "l")
  dev.off()
}

#####################################################################################################################################################################################

# for (idx in 1:length(plot_titles)) {
#   sorted_best_data = data[[idx]]
#   sorted_best_data$test = 0
#   sorted_best_data$test[sorted_best_data$Size == 2097152] = 1
#   
#   png(filename=paste("../plots/plot_","Size16384_",data_names[idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
#   plot(sorted_best_data$test)
#   dev.off()
# }

#####################################################################################################################################################################################

d_seq = read.table("../prepared_data_seq.csv", header = T, sep=",")
d_rnd = read.table("../prepared_data_rnd.csv", header = T, sep=",")
data[[1]]$outlier = d_seq$outlier[1:200000]
data[[2]]$outlier = d_seq$outlier[200001:400000]
data[[3]]$outlier = d_rnd$outlier[1:200000]
data[[4]]$outlier = d_rnd$outlier[200001:400000]

plot_titles = c(rep("Ausreißer",4))
plot_titles[1] = paste(plot_titles[1],": cached-off0-seq (Lesend)",sep ="")
plot_titles[2] = paste(plot_titles[2],": cached-off0-seq (Schreibend)",sep ="")
plot_titles[3] = paste(plot_titles[3],": cached-off0-rnd (Lesend)",sep ="")
plot_titles[4] = paste(plot_titles[4],": cached-off0-rnd (Schreibend)",sep ="")

colors = c("gray28","red")

for (idx in 1:length(plot_titles)) {
  sorted_best_data = data[[idx]]
  
  png(filename=paste("../plots/plot_","outlier_",data_names[idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(sorted_best_data$Duration, col= colors[sorted_best_data$outlier + 1],pch = c(20, rep(NA, plot_every)),ylim = c(quantile(sorted_best_data$Duration,0.01,names = FALSE),quantile(sorted_best_data$Duration,0.99,names = FALSE)),
       xlab = "Index", ylab = "Dauer in Sekunden", main = plot_titles[idx])
  dev.off()
}

# for (idx in 1:length(plot_titles)) {
#   sorted_best_data = data[[idx]][order(data[[idx]]$Duration),]
#   
#   png(filename=paste("../plots/plot_","outlier_sorted_",data_names[idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
#   plot(sorted_best_data$Duration, col= colors[sorted_best_data$outlier + 1],pch = c(20, rep(NA, plot_every)),ylim = c(quantile(sorted_best_data$Duration,0.01,names = FALSE),quantile(sorted_best_data$Duration,0.99,names = FALSE)),
#        xlab = "Index", ylab = "Dauer in Sekunden", main = plot_titles[idx])
#   dev.off()
# }


#####################################################################################################################################################################################

baselines_results_seq = read.table("../parameter_test-baselines_seq.csv", header = T, sep=",")
baselines_results_seq = baselines_results_seq[names(baselines_results_seq) %in% c("model","mean_arith_error","mean_rel_arith_error",
                                                                                  "mean_rel_rms","mean_third_quartile_rel_error","max_max_rel_error")]
names(baselines_results_seq) = c("Modell","Mittlerer Arithmetischer Fehler","Relativer Mittlerer Arithmetischer Fehler","Relative Mittlere quadratische Abweichung",
                                 "Q3 des Fehlers","Maximaler Fehler")
names(baselines_results_seq) = c("Modell","MAF","RMAF","RMQA","Q3","Max")

baselines_results_seq = baselines_results_seq[baselines_results_seq$Modell %in% c("mean_performance","linreg_Size","linreg_Size+Offset","linreg_Size+Offset+OpTyp","median_Duration_aggregated",
                                              "median_Duration_with_linreg_error_class_aggregated","median_Duration_with_good_model_error_class_aggregated"),]
baselines_results_seq$Modell = c("Durchschnittsdauer","LinReg Größe","LinReg Größe und Abstand","LinReg Größe, Abstand und OpTyp","Median aggregiert",
                                              "Median aggregiert mit Fehlerklasse aus LinReg","Median aggregiert mit Fehlerklasse aus Tupel1")
baselines_results_seq$Modell = c("Durchschnitt","LinReg G","LinReg GA","LinReg GAO","Median agg",
                                 "Median agg LinReg-Fehlerklasse ","Median agg Tupel1-Fehlerklasse")
baselines_results_seq$Typ = c("Agg","Ind","Ind","Ind","Agg","Agg","Agg")

baselines_results_seq = baselines_results_seq[order(baselines_results_seq$RMQA),]

for (idx in 2:ncol(baselines_results_seq)-1) {
  baselines_results_seq[,idx] = format(baselines_results_seq[,idx],digits = 2)
}
write.table(baselines_results_seq, paste("../csv/latex_baseline_seq_results.csv"), sep=",", row.names = FALSE,quote = FALSE)

#####################################################################################################################################################################################

baselines_results_rnd = read.table("../parameter_test-baselines_rnd.csv", header = T, sep=",")
baselines_results_rnd = baselines_results_rnd[names(baselines_results_rnd) %in% c("model","mean_arith_error","mean_rel_arith_error",
                                                                                  "mean_rel_rms","mean_third_quartile_rel_error","max_max_rel_error")]
names(baselines_results_rnd) = c("Modell","Mittlerer Arithmetischer Fehler","Relativer Mittlerer Arithmetischer Fehler","Relative Mittlere quadratische Abweichung",
                                 "Q3 des Fehlers","Maximaler Fehler")
names(baselines_results_rnd) = c("Modell","MAF","RMAF","RMQA","Q3","Max")

baselines_results_rnd = baselines_results_rnd[baselines_results_rnd$Modell %in% c("mean_performance","linreg_Size","linreg_Size+Offset","linreg_Size+Offset+OpTyp","median_Duration_aggregated",
                                                                                  "median_Duration_with_linreg_error_class_aggregated","median_Duration_with_good_model_error_class_aggregated"),]
baselines_results_rnd$Modell = c("Durchschnittsdauer","LinReg Größe","LinReg Größe und Abstand","LinReg Größe, Abstand und OpTyp","Median aggregiert",
                                 "Median aggregiert mit Fehlerklasse aus LinReg","Median aggregiert mit Fehlerklasse aus Tupel1")
baselines_results_rnd$Modell = c("Durchschnitt","LinReg G","LinReg GA","LinReg GAO","Median agg",
                                 "Median agg LinReg-Fehlerklasse ","Median agg Tupel1-Fehlerklasse")

baselines_results_rnd$Typ = c("Agg","Ind","Ind","Ind","Agg","Agg","Agg")

baselines_results_rnd = baselines_results_rnd[order(baselines_results_rnd$RMQA),]


for (idx in 2:ncol(baselines_results_seq)-1) {
  baselines_results_rnd[,idx] = format(baselines_results_rnd[,idx],digits = 2)
}
write.table(baselines_results_rnd, paste("../csv/latex_baseline_rnd_results.csv"), sep=",", row.names = FALSE,quote = FALSE)


#####################################################################################################################################################################################

baselines_seq = read.table("../data_baselines_seq.csv", header = T, sep=",")
d_seq = read.table("../prepared_data_seq.csv", header = T, sep=",")

results_pfad = "../plots/"
best_data = baselines_seq
best_data = best_data[names(best_data) %in% c("Duration","mean_performance","linreg_Size","median_Duration_aggregated",
                                       "median_Duration_with_linreg_error_class_aggregated","median_Duration_with_good_model_error_class_aggregated")] 
                                      #,"linreg_Size.Offset","linreg_Size.Offset.OpTyp"
all_data = cbind(d_seq,best_data[!names(best_data) %in% c("Duration")])

plot_titles = c(rep("Modell ",5))
plot_titles[1] = paste(plot_titles[1],"Durchschnittsdauer",sep ="")
plot_titles[2] = paste(plot_titles[2],"Lineare Regression nach Größe",sep ="")
# plot_titles[3] = paste(plot_titles[3],"Lineare Regression nach Größe und Abstand",sep ="")
# plot_titles[4] = paste(plot_titles[4],"Lineare Regression nach Größe, Abstand und Operationstyp",sep ="")
plot_titles[3] = paste(plot_titles[3],"Median aggregierter Daten",sep ="")
plot_titles[4] = paste(plot_titles[4],"Median aggregierter Daten mit Fehlerklasse aus linearer Regression",sep ="")
plot_titles[5] = paste(plot_titles[5],"Median aggregierter Daten mit Fehlerklasse aus Tupel1",sep ="")
for(col_idx in seq(from = 2, to = ncol(best_data), by = 1)) {
  png(filename=paste(results_pfad,"plot_seq_",names(best_data)[col_idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(best_data$Duration, col="gray28",pch = c(20, rep(NA, plot_every)),ylim = c(quantile(best_data$Duration,0.01,names = FALSE),quantile(best_data$Duration,0.99,names = FALSE)),
       ylab = "Dauer in Sekunden", main = plot_titles[col_idx-1],cex.main = 0.93)
  points(best_data[,col_idx], col="dodgerblue3",pch = c(20, rep(NA, plot_every)))
  dev.off()
}

sorted_best_data = best_data[order(best_data$Duration),]

plot_titles = c(rep("Modell ",5))
plot_titles[1] = paste(plot_titles[1],"Durchschnittsdauer",sep ="")
plot_titles[2] = paste(plot_titles[2],"Lineare Regression nach Größe",sep ="")
# plot_titles[3] = paste(plot_titles[3],"Lineare Regression nach Größe und Abstand",sep ="")
# plot_titles[4] = paste(plot_titles[4],"Lineare Regression nach Größe, Abstand und Operationstyp",sep ="")
plot_titles[3] = paste(plot_titles[3],"Median aggregierter Daten",sep ="")
plot_titles[4] = paste(plot_titles[4],"Median aggregierter Daten mit Fehlerklasse aus linearer Regression",sep ="")
plot_titles[5] = paste(plot_titles[5],"Median aggregierter Daten mit Fehlerklasse aus Tupel1",sep ="")
for(col_idx in seq(from = 2, to = ncol(sorted_best_data), by = 1)) {
  png(filename=paste(results_pfad,"plot_seq_","DurationToPredSorted_",names(best_data)[col_idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(sorted_best_data$Duration, col="gray28",pch = c(20, rep(NA, plot_every)),ylim = c(quantile(sorted_best_data$Duration,0.01,names = FALSE),quantile(sorted_best_data$Duration,0.99,names = FALSE)),
       log = "y",ylab = "Dauer in Sekunden (logarithmisch)", main = plot_titles[col_idx-1],cex.main = 0.93)
  points(sorted_best_data[,col_idx], col="dodgerblue3",pch = c(20, rep(NA, plot_every)))
  dev.off()
}

#####################################################################################################################################################################################

baselines_rnd = read.table("../data_baselines_rnd.csv", header = T, sep=",")
d_rnd = read.table("../prepared_data_rnd.csv", header = T, sep=",")

results_pfad = "../plots/"
best_data = baselines_rnd
best_data = best_data[names(best_data) %in% c("Duration","mean_performance","linreg_Size","median_Duration_aggregated",
                                              "median_Duration_with_linreg_error_class_aggregated","median_Duration_with_good_model_error_class_aggregated")] 
#,"linreg_Size.Offset","linreg_Size.Offset.OpTyp"
all_data = cbind(d_rnd,best_data[!names(best_data) %in% c("Duration")])

plot_titles = c(rep("Modell ",5))
plot_titles[1] = paste(plot_titles[1],"Durchschnittsdauer",sep ="")
plot_titles[2] = paste(plot_titles[2],"Lineare Regression nach Größe",sep ="")
# plot_titles[3] = paste(plot_titles[3],"Lineare Regression nach Größe und Abstand",sep ="")
# plot_titles[4] = paste(plot_titles[4],"Lineare Regression nach Größe, Abstand und Operationstyp",sep ="")
plot_titles[3] = paste(plot_titles[3],"Median aggregierter Daten",sep ="")
plot_titles[4] = paste(plot_titles[4],"Median aggregierter Daten mit Fehlerklasse aus linearer Regression",sep ="")
plot_titles[5] = paste(plot_titles[5],"Median aggregierter Daten mit Fehlerklasse aus Tupel1",sep ="")
for(col_idx in seq(from = 2, to = ncol(best_data), by = 1)) {
  png(filename=paste(results_pfad,"plot_rnd_",names(best_data)[col_idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(best_data$Duration, col="gray28",pch = c(20, rep(NA, plot_every)),ylim = c(quantile(best_data$Duration,0.01,names = FALSE),quantile(best_data$Duration,0.99,names = FALSE)),
       ylab = "Dauer in Sekunden", main = plot_titles[col_idx-1],cex.main = 0.93)
  points(best_data[,col_idx], col="dodgerblue3",pch = c(20, rep(NA, plot_every)))
  dev.off()
}

sorted_best_data = best_data[order(best_data$Duration),]

plot_titles = c(rep("Modell ",5))
plot_titles[1] = paste(plot_titles[1],"Durchschnittsdauer",sep ="")
plot_titles[2] = paste(plot_titles[2],"Lineare Regression nach Größe",sep ="")
# plot_titles[3] = paste(plot_titles[3],"Lineare Regression nach Größe und Abstand",sep ="")
# plot_titles[4] = paste(plot_titles[4],"Lineare Regression nach Größe, Abstand und Operationstyp",sep ="")
plot_titles[3] = paste(plot_titles[3],"Median aggregierter Daten",sep ="")
plot_titles[4] = paste(plot_titles[4],"Median aggregierter Daten mit Fehlerklasse aus linearer Regression",sep ="")
plot_titles[5] = paste(plot_titles[5],"Median aggregierter Daten mit Fehlerklasse aus Tupel1",sep ="")
for(col_idx in seq(from = 2, to = ncol(sorted_best_data), by = 1)) {
  png(filename=paste(results_pfad,"plot_rnd_","DurationToPredSorted_",names(best_data)[col_idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(sorted_best_data$Duration, col="gray28",pch = c(20, rep(NA, plot_every)),ylim = c(quantile(sorted_best_data$Duration,0.01,names = FALSE),quantile(sorted_best_data$Duration,0.99,names = FALSE)),
       log = "y",ylab = "Dauer in Sekunden (logarithmisch)", main = plot_titles[col_idx-1],cex.main = 0.93)
  points(sorted_best_data[,col_idx], col="dodgerblue3",pch = c(20, rep(NA, plot_every)))
  dev.off()
}

#####################################################################################################################################################################################

best_data = baselines_seq
best_data = best_data[names(best_data) %in% c("Duration","mean_performance","linreg_Size","median_Duration_aggregated",
                                              "median_Duration_with_linreg_error_class_aggregated","median_Duration_with_good_model_error_class_aggregated")] 
#,"linreg_Size.Offset","linreg_Size.Offset.OpTyp"
all_data = cbind(d_seq,best_data[!names(best_data) %in% c("Duration")])
d = d_seq

for(col_idx in seq(from = 2, to = ncol(best_data), by = 1)) {
  all_data$error = abs((best_data[,col_idx] - best_data$Duration)/best_data$Duration)
  
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
  
  if (min_features$OpTyp == 0) {
    min_features$OpTyp_b = "lesend"
  } else {
    min_features$OpTyp_b = "schreibend"
  }
  if (min_features$OpTyp == 0) {
    median_features$OpTyp_b = "lesend"
  } else {
    median_features$OpTyp_b = "schreibend"
  }
  if (min_features$OpTyp == 0) {
    max_features$OpTyp_b = "lesend"
  } else {
    max_features$OpTyp_b = "schreibend"
  }
  
  min_data = all_data[all_data$Size == min_features[,"Size"] & all_data$OpTyp == min_features[,"OpTyp"] & all_data$DeltaOffset == min_features[,"DeltaOffset"],]
  max_data = all_data[all_data$Size == max_features[,"Size"] & all_data$OpTyp == max_features[,"OpTyp"] & all_data$DeltaOffset == max_features[,"DeltaOffset"],]
  median_data = all_data[all_data$Size == median_features[,"Size"] & all_data$OpTyp == median_features[,"OpTyp"] & all_data$DeltaOffset == median_features[,"DeltaOffset"],]
  
  png(filename=paste(results_pfad,"plot_density_best_",names(best_data)[col_idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  par(mfrow = c(1,1))
  plot(density(min_data$Duration), col = "gray28", lwd = 1.5,
       ylab = "Dichte", xlab = "Dauer in Sekunden", cex.main = 0.9,
       main = paste("Fehler: ", format(min_features$mean_error*100,digits = 2,nsmall = 2),"%, N ",min_features$Count,", Set (Größe ",min_features$Size," KB, ",min_features$OpTyp_b,", Abstand ",min_features$DeltaOffset," KB)", sep = ""), 
       xlim = c(min(quantile(min_data$Duration,0.01,names = FALSE),mean(min_data[,names(min_data) == names(best_data)[col_idx]])),max(quantile(min_data$Duration,0.99,names = FALSE),mean(min_data[,names(min_data) == names(best_data)[col_idx]]))))
  #, xlim = c(quantile(min_data$Duration,0.01,names = FALSE),quantile(min_data$Duration,0.99,names = FALSE)))
  abline(v = mean(min_data$q0.1), untf = FALSE, col = "darkgreen",lwd = 2)
  abline(v = mean(min_data$q0.9), untf = FALSE, col = "darkorchid2", lwd = 2)
  abline(v = mean(min_data$Duration), untf = FALSE, col = "black", lwd = 2)
  abline(v = mean(min_data[,names(min_data) == names(best_data)[col_idx]]), untf = FALSE, col = "dodgerblue3",lwd = 2)
  dev.off()
  
  png(filename=paste(results_pfad,"plot_density_med_",names(best_data)[col_idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(density(median_data$Duration), col = "gray28", lwd = 1.5,
       ylab = "Dichte", xlab = "Dauer in Sekunden", cex.main = 0.9,
       main = paste("Fehler: ", format(median_features$mean_error*100,digits = 2,nsmall = 2),"%, N ",median_features$Count,", Set (Größe ",median_features$Size," KB, ",median_features$OpTyp_b,", Abstand ",median_features$DeltaOffset," KB)", sep = ""), 
       xlim = c(min(quantile(median_data$Duration,0.01,names = FALSE),mean(median_data[,names(median_data) == names(best_data)[col_idx]])),max(quantile(median_data$Duration,0.99,names = FALSE),mean(median_data[,names(median_data) == names(best_data)[col_idx]]))))
  #, xlim = c(quantile(median_data$Duration,0.01,names = FALSE),quantile(median_data$Duration,0.99,names = FALSE)))
  abline(v = mean(median_data$q0.1), untf = FALSE, col = "darkgreen",lwd = 2)
  abline(v = mean(median_data$q0.9), untf = FALSE, col = "darkorchid2", lwd = 2)
  abline(v = mean(median_data$Duration), untf = FALSE, col = "black", lwd = 2)
  abline(v = mean(median_data[,names(median_data) == names(best_data)[col_idx]]), untf = FALSE, col = "dodgerblue3",lwd = 2)
  dev.off()
  
  png(filename=paste(results_pfad,"plot_density_worst_",names(best_data)[col_idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(density(max_data$Duration), col = "gray28", lwd = 1.5,
       ylab = "Dichte", xlab = "Dauer in Sekunden", cex.main = 0.9,
       main = paste("Fehler: ", format(max_features$mean_error*100,digits = 2,nsmall = 2),"%, N ",max_features$Count,", Set (Größe ",max_features$Size," KB, ",max_features$OpTyp_b,", Abstand ",max_features$DeltaOffset," KB)", sep = ""), 
       xlim = c(min(quantile(max_data$Duration,0.01,names = FALSE),mean(max_data[,names(max_data) == names(best_data)[col_idx]])),max(quantile(max_data$Duration,0.99,names = FALSE),mean(max_data[,names(max_data) == names(best_data)[col_idx]]))))
  #xlim = c(quantile(max_data$Duration,0.01,names = FALSE),quantile(max_data$Duration,0.99,names = FALSE)))
  abline(v = mean(max_data$q0.1), untf = FALSE, col = "darkgreen",lwd = 2)
  abline(v = mean(max_data$q0.9), untf = FALSE, col = "darkorchid2", lwd = 2)
  abline(v = mean(max_data$Duration), untf = FALSE, col = "black", lwd = 2)
  abline(v = mean(max_data[,names(max_data) == names(best_data)[col_idx]]), untf = FALSE, col = "dodgerblue3",lwd = 2)
  dev.off()
}

####################################################################################################################################################################################

#####################################################################################################################################################################################
#####################################################################################################################################################################################

colors = c("chartreuse","aquamarine","deeppink1","red","mediumpurple","darkgoldenrod2","darkorchid2","darkblue","chartreuse4","chocolate4", "coral3","cyan1","darkred","coral","black")

best_data = baselines_seq
best_data = best_data[names(best_data) %in% c("Duration","mean_performance","linreg_Size","median_Duration_aggregated",
                                              "median_Duration_with_linreg_error_class_aggregated","median_Duration_with_good_model_error_class_aggregated")] 

## betrachte linreg-size plot -> normal -> sorted
#nun kommen die fehlerklassen
# Jedes Cluster representiert einen anderen I/O-Path im System
d.linreg_with_error_class = d_seq
d.linreg_with_error_class$error = (best_data$Duration - best_data$linreg_Size)
d.linreg_with_error_class$pred = best_data$linreg_Size

png(filename="../plots/linreg_error_seq.png", width = png_width, height = png_height, units = 'in', res = 300)
plot(d.linreg_with_error_class$error, ylab = "Fehler in Sekunden", main = "LinReg G", pch = c(20, rep(NA, 0)))
dev.off()

nof_clusters = length(unique(d.linreg_with_error_class$linreg_error_class))
png(filename="../plots/linreg_error_clustering_seq.png", width = png_width, height = png_height, units = 'in', res = 300)
# plot(sorted_d.linreg_with_error_class$error), col = c("1","2","3","4","5","6","7","8","9","10")[sorted_d.linreg_with_error_class$linreg_error_class])
plot(d.linreg_with_error_class$error, col = colors[d.linreg_with_error_class$linreg_error_class], ylab = "Fehler in Sekunden",
     main = "LinReg G", pch = c(20, rep(NA, 0)))
dev.off()

png(filename="../plots/linreg_error_sorted_seq.png", width = png_width, height = png_height, units = 'in', res = 300)
plot(sort(d.linreg_with_error_class$error), ylab = "Fehler in Sekunden", main = "LinReg G", pch = c(20, rep(NA, 0)))
dev.off()

nof_clusters = length(unique(d.linreg_with_error_class$linreg_error_class))
sorted_d.linreg_with_error_class = d.linreg_with_error_class[order(d.linreg_with_error_class$error),]
png(filename="../plots/linreg_error_sorted_clustering_seq.png", width = png_width, height = png_height, units = 'in', res = 300)
# plot(sorted_d.linreg_with_error_class$error), col = c("1","2","3","4","5","6","7","8","9","10")[sorted_d.linreg_with_error_class$linreg_error_class])
plot(sorted_d.linreg_with_error_class$error, col = colors[sorted_d.linreg_with_error_class$linreg_error_class], ylab = "Fehler in Sekunden",
     main = "LinReg G", pch = c(20, rep(NA, 0)))
dev.off()


png(filename=paste(results_pfad,"plot_durationToPredErrorclasses_seq.png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
plot(d.linreg_with_error_class$Duration, col="gray28",pch = c(20, rep(NA, plot_every)),ylim = c(quantile(d.linreg_with_error_class$Duration,0.01,names = FALSE),quantile(d.linreg_with_error_class$Duration,0.99,names = FALSE)),
     ylab = "Dauer in Sekunden", main = "LinReg G mit Fehlerklassen",cex.main = 0.93)
points(d.linreg_with_error_class$pred, col = colors[d.linreg_with_error_class$linreg_error_class],pch = c(20, rep(NA, plot_every)))
dev.off()

####################################################################################################################################################################################

## betrachte linreg-size plot -> normal -> sorted
#nun kommen die fehlerklassen
# Jedes Cluster representiert einen anderen I/O-Path im System

colors = c("chartreuse","aquamarine","deeppink1","red","mediumpurple","darkgoldenrod2","darkorchid2","darkblue","chartreuse4","chocolate4", "coral3","cyan1","darkred","coral","black")

best_data = baselines_rnd
best_data = best_data[names(best_data) %in% c("Duration","mean_performance","linreg_Size","median_Duration_aggregated",
                                              "median_Duration_with_linreg_error_class_aggregated","median_Duration_with_good_model_error_class_aggregated")] 
d.linreg_with_error_class = d_rnd
d.linreg_with_error_class$error = (best_data$Duration - best_data$linreg_Size)
d.linreg_with_error_class$pred = best_data$linreg_Size

png(filename="../plots/linreg_error_rnd.png", width = png_width, height = png_height, units = 'in', res = 300)
plot(d.linreg_with_error_class$error, ylab = "Fehler in Sekunden", main = "LinReg G", pch = c(20, rep(NA, 0)))
dev.off()

nof_clusters = length(unique(d.linreg_with_error_class$linreg_error_class))
png(filename="../plots/linreg_error_clustering_rnd.png", width = png_width, height = png_height, units = 'in', res = 300)
# plot(sorted_d.linreg_with_error_class$error), col = c("1","2","3","4","5","6","7","8","9","10")[sorted_d.linreg_with_error_class$linreg_error_class])
plot(d.linreg_with_error_class$error, col = colors[d.linreg_with_error_class$linreg_error_class], ylab = "Fehler in Sekunden",
     main = "LinReg G", pch = c(20, rep(NA, 0)))
dev.off()

png(filename="../plots/linreg_error_sorted_rnd.png", width = png_width, height = png_height, units = 'in', res = 300)
plot(sort(d.linreg_with_error_class$error), ylab = "Fehler in Sekunden", main = "LinReg G", pch = c(20, rep(NA, 0)))
dev.off()

nof_clusters = length(unique(d.linreg_with_error_class$linreg_error_class))
sorted_d.linreg_with_error_class = d.linreg_with_error_class[order(d.linreg_with_error_class$error),]
png(filename="../plots/linreg_error_sorted_clustering_rnd.png", width = png_width, height = png_height, units = 'in', res = 300)
# plot(sorted_d.linreg_with_error_class$error), col = c("1","2","3","4","5","6","7","8","9","10")[sorted_d.linreg_with_error_class$linreg_error_class])
plot(sorted_d.linreg_with_error_class$error, col = colors[sorted_d.linreg_with_error_class$linreg_error_class], ylab = "Fehler in Sekunden",
     main = "LinReg G", pch = c(20, rep(NA, 0)))
dev.off()


png(filename=paste(results_pfad,"plot_durationToPredErrorclasses_rnd.png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
plot(d.linreg_with_error_class$Duration, col="gray28",pch = c(20, rep(NA, plot_every)),ylim = c(quantile(d.linreg_with_error_class$Duration,0.01,names = FALSE),quantile(d.linreg_with_error_class$Duration,0.99,names = FALSE)),
     ylab = "Dauer in Sekunden", main = "LinReg G mit Fehlerklassen",cex.main = 0.93)
points(d.linreg_with_error_class$pred, col = colors[d.linreg_with_error_class$linreg_error_class],pch = c(20, rep(NA, plot_every)))
dev.off()

####################################################################################################################################################################################

results_seq = read.table("../best_results_seq.csv", header = T, sep=",")
results_seq = results_seq[names(results_seq) %in% c("model","arith_error","rel_arith_error","rel_rms","third_quartile_rel_error","max_rel_error","mean_in_range")]
names(results_seq) = c("Modell","Mittlerer Arithmetischer Fehler","Relativer Mittlerer Arithmetischer Fehler","Relative Mittlere quadratische Abweichung",
                                 "Q3 des Fehlers","Maximaler Fehler","Größenordnung")
names(results_seq) = c("Modell","MAF","RMAF","RMQA","Q3","Max","Bereich")

results_seq = results_seq[results_seq$Modell %in% c("tuple1","tuple1_with_error_class_from_linreg"),]
results_seq$Modell = c("Tupel1","Tupel1 LinReg-Fehlerklasse")

results_seq = results_seq[order(results_seq$RMQA),]

for (idx in 2:ncol(results_seq)) {
  results_seq[,idx] = format(results_seq[,idx],digits = 2)
}
write.table(results_seq, paste("../csv/latex_seq_results.csv"), sep=",", row.names = FALSE,quote = FALSE)

#################################

results_seq = read.table("../best_results_seq.csv", header = T, sep=",")
results_seq = results_seq[names(results_seq) %in% c("model","outlier_correct_percent","q0.1_rel_arith_mean","q0.1_rel_rms","q0.9_rel_arith_mean","q0.9_rel_rms")]
names(results_seq) = c("Modell","Q0.1 RMAF","Q0.1 RMQA","Q0.9 RMAF","Q0.9 RMQA","Korrekt")
names(results_seq) = c("Modell","QRMAF","QRMQA","LRMAF","LRMQA","Korrekt")

results_seq$Korrekt =  results_seq$Korrekt*100

results_seq = results_seq[results_seq$Modell %in% c("tuple1","tuple1_with_error_class_from_linreg"),]
results_seq$Modell = c("Tupel1","Tupel1 LinReg-Fehlerklasse")

for (idx in 2:ncol(results_seq)) {
  results_seq[,idx] = format(results_seq[,idx],digits = 2)
}
write.table(results_seq, paste("../csv/latex_seq_outlier.csv"), sep=",", row.names = FALSE,quote = FALSE)

#################################

results_seq = read.table("../best_results_seq.csv", header = T, sep=",")
results_seq = results_seq[names(results_seq) %in% c("model","layers","neurons","nof.steps","avg..training.duration...sec")]
names(results_seq) = c("Modell","verdeckte Schichten","Neuronen","Iterationen","Trainingsdauer")

results_seq = results_seq[results_seq$Modell %in% c("tuple1","tuple1_with_error_class_from_linreg"),]
results_seq$Modell = c("Tupel1","Tupel1 LinReg-Fehlerklasse")

for (idx in 2:ncol(results_seq)) {
  results_seq[,idx] = format(results_seq[,idx],digits = 2)
}
write.table(results_seq, paste("../csv/latex_seq_net-stats.csv"), sep=",", row.names = FALSE,quote = FALSE)


####################################################################################################################################################################################

results_rnd = read.table("../best_results_rnd.csv", header = T, sep=",")
results_rnd = results_rnd[names(results_rnd) %in% c("model","arith_error","rel_arith_error","rel_rms","third_quartile_rel_error","max_rel_error","mean_in_range")]
names(results_rnd) = c("Modell","Mittlerer Arithmetischer Fehler","Relativer Mittlerer Arithmetischer Fehler","Relative Mittlere quadratische Abweichung",
                       "Q3 des Fehlers","Maximaler Fehler","Größenordnung")
names(results_rnd) = c("Modell","MAF","RMAF","RMQA","Q3","Max","Bereich")

results_rnd = results_rnd[results_rnd$Modell %in% c("tuple1","tuple1_with_error_class_from_linreg"),]
results_rnd$Modell = c("Tupel1","Tupel1 LinReg-Fehlerklasse")

results_rnd = results_rnd[order(results_rnd$RMQA),]

for (idx in 2:ncol(results_rnd)) {
  results_rnd[,idx] = format(results_rnd[,idx],digits = 2)
}
write.table(results_rnd, paste("../csv/latex_rnd_results.csv"), sep=",", row.names = FALSE,quote = FALSE)

#################################

results_rnd = read.table("../best_results_rnd.csv", header = T, sep=",")
results_rnd = results_rnd[names(results_rnd) %in% c("model","outlier_correct_percent","q0.1_rel_arith_mean","q0.1_rel_rms","q0.9_rel_arith_mean","q0.9_rel_rms")]
names(results_rnd) = c("Modell","Q0.1 RMAF","Q0.1 RMQA","Q0.9 RMAF","Q0.9 RMQA","Korrekt")
names(results_rnd) = c("Modell","QRMAF","QRMQA","LRMAF","LRMQA","Korrekt")

results_rnd$Korrekt =  results_rnd$Korrekt*100

results_rnd = results_rnd[results_rnd$Modell %in% c("tuple1","tuple1_with_error_class_from_linreg"),]
results_rnd$Modell = c("Tupel1","Tupel1 LinReg-Fehlerklasse")

for (idx in 2:ncol(results_rnd)) {
  results_rnd[,idx] = format(results_rnd[,idx],digits = 2)
}
write.table(results_rnd, paste("../csv/latex_rnd_outlier.csv"), sep=",", row.names = FALSE,quote = FALSE)

#################################

results_rnd = read.table("../best_results_rnd.csv", header = T, sep=",")
results_rnd = results_rnd[names(results_rnd) %in% c("model","layers","neurons","nof.steps","avg..training.duration...sec")]
names(results_rnd) = c("Modell","verdeckte Schichten","Neuronen","Iterationen","Trainingsdauer")

results_rnd = results_rnd[results_rnd$Modell %in% c("tuple1","tuple1_with_error_class_from_linreg"),]
results_rnd$Modell = c("Tupel1","Tupel1 LinReg-Fehlerklasse")

for (idx in 2:ncol(results_rnd)) {
  results_rnd[,idx] = format(results_rnd[,idx],digits = 2)
}
write.table(results_rnd, paste("../csv/latex_rnd_net-stats.csv"), sep=",", row.names = FALSE,quote = FALSE)

#####################################################################################################################################################################################

best_data = read.table("../best_data_seq.csv", header = T, sep=",")

best_data = best_data[names(best_data) %in% c("Duration","tuple1_Duration","tuple1_q0.1","tuple1_q0.9","tuple1_with_error_class_from_linreg_Duration","tuple1_with_error_class_from_linreg_q0.1",
                                              "tuple1_with_error_class_from_linreg_q0.9")]


plot_titles = c(rep("Modell ",2))
plot_titles[1] = paste(plot_titles[1],"Tupel1",sep ="")
plot_titles[2] = paste(plot_titles[2],"Tupel1 LinReg-Fehlerklasse",sep ="")
for(col_idx in seq(from = 2, to = ncol(best_data), by = 3)) {
  png(filename=paste(results_pfad,"plot_onlyPred_",names(best_data)[col_idx],"_seq.png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(best_data$Duration, col="gray28",pch = c(20, rep(NA, plot_every)),ylim = c(quantile(best_data$Duration,0.01,names = FALSE),quantile(best_data$Duration,0.99,names = FALSE)),
       ylab = "Dauer in Sekunden", main = plot_titles[(col_idx+1)/3],cex.main = 0.93)
  points(best_data[,col_idx], col="dodgerblue3",pch = c(20, rep(NA, plot_every)))
  dev.off()
}

plot_titles = c(rep("Modell ",2))
plot_titles[1] = paste(plot_titles[1],"Tupel1",sep ="")
plot_titles[2] = paste(plot_titles[2],"Tupel1 LinReg-Fehlerklasse",sep ="")
for(col_idx in seq(from = 2, to = ncol(best_data), by = 3)) {
  png(filename=paste(results_pfad,"plot_",names(best_data)[col_idx],"_seq.png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(best_data$Duration, col="gray28",pch = c(20, rep(NA, plot_every)),ylim = c(quantile(best_data$Duration,0.01,names = FALSE),quantile(best_data$Duration,0.99,names = FALSE)),
       ylab = "Dauer in Sekunden", main = plot_titles[(col_idx+1)/3],cex.main = 0.93)
  points(best_data[,col_idx], col="dodgerblue3",pch = c(20, rep(NA, plot_every)))
  points(best_data[,col_idx+1], col="lawngreen",pch = c(20, rep(NA, plot_every)))
  points(best_data[,col_idx+2], col="indianred3",pch = c(20, rep(NA, plot_every)))
  dev.off()
}


colors = c("gray28","indianred3")

plot_titles = c(rep("Modell ",2))
plot_titles[1] = paste(plot_titles[1],"Tupel1",sep ="")
plot_titles[2] = paste(plot_titles[2],"Tupel1 LinReg-Fehlerklasse",sep ="")
for(col_idx in seq(from = 2, to = ncol(best_data), by = 3)) {
  big_error = best_data
  big_error$error = abs(big_error$Duration - big_error[,col_idx])
  big_error$big_error = NA
  big_error$big_error[big_error$error > quantile(big_error$error,probs = 0.9)] = 20
  
  
  png(filename=paste(results_pfad,"plot_onlyPred_",names(best_data)[col_idx],"_seq.png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(best_data$Duration, col="gray28",pch = c(20, rep(NA, plot_every)),ylim = c(quantile(best_data$Duration,0.01,names = FALSE),quantile(best_data$Duration,0.99,names = FALSE)),
       ylab = "Dauer in Sekunden", main = plot_titles[(col_idx+1)/3],cex.main = 0.93)
  points(best_data[,col_idx], col= colors[big_error$big_error + 1],pch = big_error$big_error)
  dev.off()
}

sorted_best_data = best_data[order(best_data$Duration),]

plot_titles = c(rep("Modell ",2))
plot_titles[1] = paste(plot_titles[1],"Tupel1",sep ="")
plot_titles[2] = paste(plot_titles[2],"Tupel1 LinReg-Fehlerklasse",sep ="")
for(col_idx in seq(from = 2, to = ncol(sorted_best_data), by = 3)) {
  png(filename=paste(results_pfad,"plot_","DurationToPredSorted_onlyPred_",names(best_data)[col_idx],"_seq.png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(sorted_best_data$Duration, col="gray28",pch = c(20, rep(NA, plot_every)),ylim = c(quantile(sorted_best_data$Duration,0.01,names = FALSE),quantile(sorted_best_data$Duration,0.99,names = FALSE)),
       log = "y",ylab = "Dauer in Sekunden (logarithmisch)", main = plot_titles[(col_idx+1)/3],cex.main = 0.93)
  points(sorted_best_data[,col_idx], col="dodgerblue3",pch = c(20, rep(NA, plot_every)))
  dev.off()
}

plot_titles = c(rep("Modell ",2))
plot_titles[1] = paste(plot_titles[1],"Tupel1",sep ="")
plot_titles[2] = paste(plot_titles[2],"Tupel1 LinReg-Fehlerklasse",sep ="")
for(col_idx in seq(from = 2, to = ncol(sorted_best_data), by = 3)) {
  png(filename=paste(results_pfad,"plot_DurationToPredSorted_",names(best_data)[col_idx],"_seq.png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(sorted_best_data$Duration, col="gray28",pch = c(20, rep(NA, plot_every)),ylim = c(quantile(best_data$Duration,0.01,names = FALSE),quantile(best_data$Duration,0.99,names = FALSE)),
       log = "y",ylab = "Dauer in Sekunden (logarithmisch)", main = plot_titles[(col_idx+1)/3],cex.main = 0.93)
  points(sorted_best_data[,col_idx], col="dodgerblue3",pch = c(20, rep(NA, plot_every)))
  points(sorted_best_data[,col_idx+1], col="lawngreen",pch = c(20, rep(NA, plot_every)))
  points(sorted_best_data[,col_idx+2], col="indianred3",pch = c(20, rep(NA, plot_every)))
  dev.off()
}

all_data = cbind(d,best_data[!names(best_data) %in% c("Duration")])
d = d_seq

for(col_idx in seq(from = 2, to = ncol(best_data), by = 3)) {
  all_data$error = abs((best_data[,col_idx] - best_data$Duration)/best_data$Duration)
  
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
  
  png(filename=paste(results_pfad,"plot_density_best_",names(best_data)[col_idx],"_seq.png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  par(mfrow = c(1,1))
  plot(density(min_data$Duration), col = "gray28", lwd = 1.5,
       ylab = "Dichte", main = "Kleinster relativer Fehler", xlim = c(min(quantile(min_data$Duration,0.01,names = FALSE),mean(min_data[,names(min_data) == names(best_data)[col_idx]])),max(quantile(min_data$Duration,0.99,names = FALSE),mean(min_data[,names(min_data) == names(best_data)[col_idx]]))))
  #, xlim = c(quantile(min_data$Duration,0.01,names = FALSE),quantile(min_data$Duration,0.99,names = FALSE)))
  abline(v = mean(min_data$q0.1), untf = FALSE, col = "darkgreen",lwd = 2)
  abline(v = mean(min_data$q0.9), untf = FALSE, col = "darkorchid2", lwd = 2)
  abline(v = mean(min_data[,names(min_data) == names(best_data)[col_idx]]), untf = FALSE, col = "dodgerblue3",lwd = 2)
  if (mean(min_data[,names(min_data) == names(best_data)[col_idx + 1]] != -1)) {
    abline(v = mean(min_data[,names(min_data) == names(best_data)[col_idx + 1]]), untf = FALSE, col = "lawngreen",lwd = 2)
    abline(v = mean(min_data[,names(min_data) == names(best_data)[col_idx + 2]]), untf = FALSE, col = "indianred3",lwd = 2)
  }
  dev.off()
  
  png(filename=paste(results_pfad,"plot_density_med_",names(best_data)[col_idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(density(median_data$Duration), col = "gray28", lwd = 1.5,
       ylab = "Dichte", main = "Mittlerer relativer Fehler", xlim = c(min(quantile(median_data$Duration,0.01,names = FALSE),mean(median_data[,names(median_data) == names(best_data)[col_idx]])),max(quantile(median_data$Duration,0.99,names = FALSE),mean(median_data[,names(median_data) == names(best_data)[col_idx]]))))
  #, xlim = c(quantile(median_data$Duration,0.01,names = FALSE),quantile(median_data$Duration,0.99,names = FALSE)))
  abline(v = mean(median_data$q0.1), untf = FALSE, col = "darkgreen",lwd = 2)
  abline(v = mean(median_data$q0.9), untf = FALSE, col = "darkorchid2", lwd = 2)
  abline(v = mean(median_data[,names(median_data) == names(best_data)[col_idx]]), untf = FALSE, col = "dodgerblue3",lwd = 2)
  if (mean(min_data[,names(min_data) == names(best_data)[col_idx + 1]] != -1)) {
    abline(v = mean(median_data[,names(median_data) == names(best_data)[col_idx + 1]]), untf = FALSE, col = "lawngreen",lwd = 2)
    abline(v = mean(median_data[,names(median_data) == names(best_data)[col_idx + 2]]), untf = FALSE, col = "indianred3",lwd = 2)
  }
  dev.off()
  
  png(filename=paste(results_pfad,"plot_density_worst_",names(best_data)[col_idx],".png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(density(max_data$Duration), col = "gray28", lwd = 1.5,
       ylab = "Dichte", main = "Größter relativer Fehler", xlim = c(min(quantile(max_data$Duration,0.01,names = FALSE),mean(max_data[,names(max_data) == names(best_data)[col_idx]])),max(quantile(max_data$Duration,0.99,names = FALSE),mean(max_data[,names(max_data) == names(best_data)[col_idx]]))))
  #xlim = c(quantile(max_data$Duration,0.01,names = FALSE),quantile(max_data$Duration,0.99,names = FALSE)))
  abline(v = mean(max_data$q0.1), untf = FALSE, col = "darkgreen",lwd = 2)
  abline(v = mean(max_data$q0.9), untf = FALSE, col = "darkorchid2", lwd = 2)
  abline(v = mean(max_data[,names(max_data) == names(best_data)[col_idx]]), untf = FALSE, col = "dodgerblue3",lwd = 2)
  if (mean(min_data[,names(min_data) == names(best_data)[col_idx + 1]] != -1)) {
    abline(v = mean(max_data[,names(max_data) == names(best_data)[col_idx + 1]]), untf = FALSE, col = "lawngreen",lwd = 2)
    abline(v = mean(max_data[,names(max_data) == names(best_data)[col_idx + 2]]), untf = FALSE, col = "indianred3",lwd = 2)
  }
  dev.off()
}

#####################################################################################################################################################################################

#####################################################################################################################################################################################

best_data = read.table("../best_data_rnd.csv", header = T, sep=",")

best_data = best_data[names(best_data) %in% c("Duration","tuple1_Duration","tuple1_q0.1","tuple1_q0.9","tuple1_with_error_class_from_linreg_Duration","tuple1_with_error_class_from_linreg_q0.1",
                                              "tuple1_with_error_class_from_linreg_q0.9")]
best_data = best_data[order(names(best_data))]

plot_titles = c(rep("Modell ",2))
plot_titles[1] = paste(plot_titles[1],"Tupel1",sep ="")
plot_titles[2] = paste(plot_titles[2],"Tupel1 LinReg-Fehlerklasse",sep ="")
for(col_idx in seq(from = 2, to = ncol(best_data), by = 3)) {
  png(filename=paste(results_pfad,"plot_onlyPred_",names(best_data)[col_idx],"_rnd.png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(best_data$Duration, col="gray28",pch = c(20, rep(NA, plot_every)),ylim = c(quantile(best_data$Duration,0.01,names = FALSE),quantile(best_data$Duration,0.99,names = FALSE)),
       ylab = "Dauer in Sekunden", main = plot_titles[(col_idx+1)/3],cex.main = 0.93)
  points(best_data[,col_idx], col="dodgerblue3",pch = c(20, rep(NA, plot_every)))
  dev.off()
}

plot_titles = c(rep("Modell ",2))
plot_titles[1] = paste(plot_titles[1],"Tupel1",sep ="")
plot_titles[2] = paste(plot_titles[2],"Tupel1 LinReg-Fehlerklasse",sep ="")
for(col_idx in seq(from = 2, to = ncol(best_data), by = 3)) {
  png(filename=paste(results_pfad,"plot_",names(best_data)[col_idx],"_rnd.png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(best_data$Duration, col="gray28",pch = c(20, rep(NA, plot_every)),ylim = c(quantile(best_data$Duration,0.01,names = FALSE),quantile(best_data$Duration,0.99,names = FALSE)),
       ylab = "Dauer in Sekunden", main = plot_titles[(col_idx+1)/3],cex.main = 0.93)
  points(best_data[,col_idx], col="dodgerblue3",pch = c(20, rep(NA, plot_every)))
  points(best_data[,col_idx+1], col="lawngreen",pch = c(20, rep(NA, plot_every)))
  points(best_data[,col_idx+2], col="indianred3",pch = c(20, rep(NA, plot_every)))
  dev.off()
}

sorted_best_data = best_data[order(best_data$Duration),]

plot_titles = c(rep("Modell ",2))
plot_titles[1] = paste(plot_titles[1],"Tupel1",sep ="")
plot_titles[2] = paste(plot_titles[2],"Tupel1 LinReg-Fehlerklasse",sep ="")
for(col_idx in seq(from = 2, to = ncol(sorted_best_data), by = 3)) {
  png(filename=paste(results_pfad,"plot_","DurationToPredSorted_onlyPred_",names(best_data)[col_idx],"_rnd.png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(sorted_best_data$Duration, col="gray28",pch = c(20, rep(NA, plot_every)),ylim = c(quantile(sorted_best_data$Duration,0.01,names = FALSE),quantile(sorted_best_data$Duration,0.99,names = FALSE)),
       log = "y",ylab = "Dauer in Sekunden (logarithmisch)", main = plot_titles[(col_idx+1)/3],cex.main = 0.93)
  points(sorted_best_data[,col_idx], col="dodgerblue3",pch = c(20, rep(NA, plot_every)))
  dev.off()
}

plot_titles = c(rep("Modell ",2))
plot_titles[1] = paste(plot_titles[1],"Tupel1",sep ="")
plot_titles[2] = paste(plot_titles[2],"Tupel1 LinReg-Fehlerklasse",sep ="")
for(col_idx in seq(from = 2, to = ncol(sorted_best_data), by = 3)) {
  png(filename=paste(results_pfad,"plot_DurationToPredSorted_",names(best_data)[col_idx],"_rnd.png",sep = ""), width = png_width, height = png_height, units = 'in', res = 300)
  plot(sorted_best_data$Duration, col="gray28",pch = c(20, rep(NA, plot_every)),ylim = c(quantile(best_data$Duration,0.01,names = FALSE),quantile(best_data$Duration,0.99,names = FALSE)),
       log = "y",ylab = "Dauer in Sekunden (logarithmisch)", main = plot_titles[(col_idx+1)/3],cex.main = 0.93)
  points(sorted_best_data[,col_idx], col="dodgerblue3",pch = c(20, rep(NA, plot_every)))
  points(sorted_best_data[,col_idx+1], col="lawngreen",pch = c(20, rep(NA, plot_every)))
  points(sorted_best_data[,col_idx+2], col="indianred3",pch = c(20, rep(NA, plot_every)))
  dev.off()
}

#####################################################################################################################################################################################

# best_data = read.table("../best_data_seq.csv", header = T, sep=",")
# best_data = best_data[names(best_data) %in% c("Duration","tuple1_Duration")]
# best_data$error = best_data$Duration - best_data$tuple1_Duration
# plot(density(abs(best_data$error[best_data$error > 8e-7])),xlim = c(0,0.0015))
# # plot(density(abs(best_data$error)),xlim = c(0,0.003))
# # summary(abs(best_data$error[best_data$error > 8e-7]))
# 
# best_data = read.table("../best_data_seq.csv", header = T, sep=",")
# best_data = best_data[names(best_data) %in% c("Duration","tuple1_with_error_class_from_linreg_Duration")]
# best_data$error = abs(best_data$Duration - best_data$tuple1_with_error_class_from_linreg_Duration)
# plot(density(abs(best_data$error[best_data$error > 8e-7])),xlim = c(0,0.0015))
# 
# 
# best_data = read.table("../best_data_rnd.csv", header = T, sep=",")
# best_data = best_data[names(best_data) %in% c("Duration","tuple1_Duration")]
# best_data$error = best_data$Duration - best_data$tuple1_Duration
# plot(density(abs(best_data$error[best_data$error > 8e-7])),xlim = c(0,0.03))
# # plot(density(abs(best_data$error)),xlim = c(0,0.003))
# # summary(abs(best_data$error[best_data$error > 8e-7]))
# 
# best_data = read.table("../best_data_rnd.csv", header = T, sep=",")
# best_data = best_data[names(best_data) %in% c("Duration","tuple1_with_error_class_from_linreg_Duration")]
# best_data$error = abs(best_data$Duration - best_data$tuple1_with_error_class_from_linreg_Duration)
# plot(density(abs(best_data$error[best_data$error > 8e-7])),xlim = c(0,0.03))






