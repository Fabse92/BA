library("neuralnet")

testsize = 1000

d = read.table("data.csv", header = T, sep=",")

# Funktion zum normalisieren von Werten
normalize = function(x){   
  (x - min(x))/(max(x) - min(x))
}

d.norm <- data.frame(apply(d[c("DeltaTime","Duration","Size","DeltaOffset")],2,normalize))

d.norm$OpTyp = as.numeric(d$OpTyp)

# Aufteilen der Daten in training und test set
d.learn <- d.norm[1:(4000-testsize),]
d.test <- d.norm[(4000-testsize+1):4000,]

# Einsatz des Neuronalen Netzes
net = neuralnet(Duration~DeltaTime+OpTyp+Size+DeltaOffset, d.norm, hidden = 8, threshold = min(d$Duration) / 20 )

net = neuralnet(Duration~DeltaTime+OpTyp, d.learn, hidden = 8, threshold = min(d$Duration) / 20 ) # 5% Fehler
plot(net, rep = "best")

# Test des Netzes auf dem test set
temp_test <- subset(d.test, select = c("DeltaTime","OpTyp"))
results <- compute(net, temp_test)
d.results = data.frame(Duration = d$Duration[(4000-testsize+1):4000], prediction = results$net.result)

# Resklaierung des Ergebnisses
d.results$prediction = d.results$prediction * (max(d$Duration) - min(d$Duration)) + min(d$Duration)

d.results$error= d.results$prediction - d.results$Duration
d.results$percent_error = (d.results$error / d.results$Duration)*100

#Plot des Prozentualenfehlers
plot(d.results$percent_error, ylim=c(-200, 1000))

# Plot der echten Duration gegenüber der vorhergesagten
plot(d.results$Duration, col="blue", ylim=c(-0.0001, 0.0008))
points(d.results$prediction, col="red")

d.results$abserror = abs(d.results$error)
sum((d.results$abserror))
d.results$abspercent_error = abs(d.results$percent_error)
mean(d.results$abspercent_error)
