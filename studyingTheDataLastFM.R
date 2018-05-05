library(knitr)

lastfm <- read.csv(file = "lastfm.csv")
head(lastfm)
summary(lastfm)
nrow(lastfm)
ncol(lastfm)
str(lastfm)
res <- sapply(lastfm, class)
kable(data.frame(variables=names(res),clase=as.vector(res)))
unlist(lapply(lastfm, function(x) any(is.na(x))))


#Distribucion de valores
head(table(lastfm$artist))

head(table(lastfm$user))

dist_sex <- table(lastfm$sex)
head(table(lastfm$country))

qtt_users <- aggregate(data.frame(count = lastfm$user), 
               by = list(value = lastfm$user), 
               FUN = length)
#Numero de usuarios
nrow(qtt_users)
qtt_users[order(qtt_users$count,  decreasing = TRUE), ]



qtt_artists <- aggregate(data.frame(count = lastfm$artist), list(value = lastfm$artist), length)
#¿Cuantos grupos musicales hay? 
nrow(qtt_artists)
qtt_artists[order(qtt_artists$count,  decreasing = TRUE), ]


library (plotrix)

lbls <- paste(names(dist_sex),  "\n" , dist_sex, sep= "" ) 
pie3D(dist_sex
       , radius= 0.9
       , labelcex=  0.8
       , labels = lbls
       , explode= 0.1
       , main= "Distribución por Sexo" , col = rainbow(length(lbls)
       ))

