d = read.table("data.csv",sep=",", header=T)
plot(d$Duration, ylim=c(0,0.03))

d$durchsatz = d$Size / d$Duration / 1024 / 1024
plot(d$durchsatz, xlab="Throughput in MiB/s")

qqplot(d$durchsatz, 1:length(d$durchsatz) / length(d$durchsatz),
       xlab="Throughput in MiB/s", ylab="Cummulative distribution function")

plot(d$durchsatz[1:400])

plot(d$durchsatz, d$OpTyp)

d$index = 1:nrow(d)

#Alle Lese-Operationen herausfiltern:
r=d[d$OpTyp=="R",]
#Alle Schreib-Op
w=d[d$OpTyp=="W",]

plot(r$index, r$durchsatz, col="blue", ylim=c(0, max(d$durchsatz)))
points(w$index, w$durchsatz, col="red")

#Logarithmisch:
plot(r$index, log10(r$durchsatz), col="blue", ylim=c(0,
                                                       log10(max(d$durchsatz))), xlab="Index", ylab="Throughput in
log10(MiB/s)")
points(w$index, log10(w$durchsatz), col="red")


#Hier die echte "Timeline":
plot(r$AbsTime, log10(r$durchsatz), col="blue", ylim=c(0,
                                                         log10(max(d$durchsatz))), xlab="Timeline in s", ylab="Throughput in
log10(MiB/s)")
points(w$AbsTime, log10(w$durchsatz), col="red")