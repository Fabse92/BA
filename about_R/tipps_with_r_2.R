d = read.table("data.csv", header = T, sep=",")
plot(sort(d$Duration))
plot(log10(sort(d$Duration)))

#Alle Lese-Operationen herausfiltern:
r=d[d$OpTyp=="R",]
#Alle Schreib-Op
w=d[d$OpTyp=="W",]

plot(log10(sort(r$Duration)))
plot(log10(sort(w$Duration)))


plot(log10(quantile(d$Duration, 1:1000/ 1000)))
