library("neuralnet")

testsize = 1000

d = read.table("data.csv", header = T, sep=",")

# Zugriffe / Sekunde als gleitenden Mittelwert (moving average)
n = 30
for (i in 1:n) {
  d$accesses_per_sec[i] = 1/mean(d$DeltaTime[1:i])  
}
for (i in n:length(d$DeltaTime)) {
  d$accesses_per_sec[i] = 1/mean(d$DeltaTime[(i-n):i])  
}

# Funktion zum normalisieren von Werten
normalize = function(x){   
  (x - min(x))/(max(x) - min(x))
}

d.norm <- data.frame(apply(d[c("DeltaTime","Duration","accesses_per_sec")],2,normalize))

d.norm$OpTyp = as.numeric(d$OpTyp)

# Aufteilen der Daten in training und test set
d.learn <- d.norm[1:(4000-testsize),]
d.test <- d.norm[(4000-testsize+1):4000,]

# Einsatz des Neuronalen Netzes
net = neuralnet(Duration~DeltaTime+OpTyp+accesses_per_sec, d.learn, hidden = 8, threshold = 0.0001)#min(d$Duration) / 1 ) # 5% Fehler
plot(net, rep = "best")

# Test des Netzes auf dem test set
temp_test <- subset(d.test, select = c("DeltaTime","OpTyp","accesses_per_sec"))
results <- compute(net, temp_test)
d.results = data.frame(Duration = d$Duration[(4000-testsize+1):4000], prediction = results$net.result)

# Resklaierung des Ergebnisses
d.results$prediction = d.results$prediction * (max(d$Duration) - min(d$Duration)) + min(d$Duration)

d.results$error= d.results$prediction - d.results$Duration
d.results$percent_error = (d.results$error / d.results$Duration)*100

#Plot des Prozentualenfehlers
plot(d.results$percent_error, ylim=c(-200, 1000))

# Plot der echten Duration gegenüber der vorhergesagten
plot(d.results$Duration, col="blue", ylim=c(-0.001, 0.002))
points(d.results$prediction, col="red")

d.results$abserror = abs(d.results$error)
sum((d.results$abserror))
d.results$abspercent_error = abs(d.results$percent_error)
mean(d.results$abspercent_error)
