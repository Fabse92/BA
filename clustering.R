
d = read.table("data.csv", header = T, sep=",")
d$Durchsatz = d$Size / d$Duration / 1024 / 1024
d$index = 1:nrow(d)

normalize = function(x){   
  (x - min(x))/(max(x) - min(x))
}

#Alle Lese-Operationen herausfiltern:
r=d[d$OpTyp=="R",]
#Alle Schreib-Op
w=d[d$OpTyp=="W",]

#einen Datensatz mit Normalisierten Durchsatz zum Clustern erstellen
r.clustering <- apply(r[c("Durchsatz")],2,normalize)
w.clustering <- apply(w[c("Durchsatz")],2,normalize)

# K-Means Cluster Analysis
rfit <- kmeans(r.clustering, 2)
wfit <- kmeans(w.clustering, 2)
# get cluster means
raggr = aggregate(r.clustering,by=list(rfit$cluster),FUN=mean)
waggr = aggregate(w.clustering,by=list(wfit$cluster),FUN=mean)
# append cluster assignment
r$cluster <- rfit$cluster
w$cluster <- wfit$cluster

# 1 soll für Cached und 2 für Uncached stehen
if (raggr$Durchsatz[1] < raggr$Durchsatz[2])
{
  r$cluster[r$cluster == 1] = 3
  r$cluster[r$cluster == 2] = 1
  r$cluster[r$cluster == 3] = 2  
}
if (waggr$Durchsatz[1] < waggr$Durchsatz[2])
{
  w$cluster[w$cluster == 1] = 3
  w$cluster[w$cluster == 2] = 1
  w$cluster[w$cluster == 3] = 2  
}

plot(w$index[w$cluster == 1], w$Durchsatz[w$cluster == 1], col = "blue", ylim = c(min(w$Durchsatz[w$cluster == 2]), max(w$Durchsatz[w$cluster == 1])))
points(w$index[w$cluster == 2], w$Durchsatz[w$cluster == 2], col ="red")

plot(r$index[r$cluster == 1], r$Durchsatz[r$cluster == 1], col = "blue", ylim = c(min(r$Durchsatz[r$cluster == 2]), max(r$Durchsatz[r$cluster == 1])))
points(r$index[r$cluster == 2], r$Durchsatz[r$cluster == 2], col ="red")

rlist2 = w$index[w$cluster == 2]
wlist2 = r$index[r$cluster == 2]
list2 = c(rlist2, wlist2)

d$cluster = 1
d$cluster[is.element(d$index, list2)] = 2

d$index = NULL
