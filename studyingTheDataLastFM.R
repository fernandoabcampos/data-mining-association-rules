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

colnames(lastfm)
drops <- c("sex","country")
lastfm <- lastfm[ , !(names(lastfm) %in% drops)]
class(lastfm)
str(lastfm)

nrow(lastfm)

my_duplicated_list <- lastfm[duplicated(lastfm),]
my_duplicated_list
nrow(my_duplicated_list)

not_duplicated <- lastfm[!duplicated(lastfm), ]
nrow(not_duplicated)

qualquer_coisa <- lapply (lastfm, unique)
summary(qualquer_coisa)



#otra aproximacao
unicos <- unique(lastfm)

str(not_duplicated)
not_duplicated$user <- discretize(not_duplicated$user)
?discretize
head(not_duplicated$user2) 
t_lastfm <- as (not_duplicated, "transactions")
head(t_lastfm)


t_lastfm2 <- lapply (lastfm, unique)
t_lastfm2 <- as (t_lastfm2, "transactions")
head(t_lastfm2)

inspect(head(t_lastfm, 2))




drops <- c("user", "sex")
mbc <- split (x = lastfm[ , !(names(lastfm) %in% drops)], f = lastfm$country)
mbc <- lapply (mbc, unique)
head(mbc)
mbc <- as (mbc, "transactions")

apriori(mbc)
apriori(mbc, parameter=list(support=0.01, confidence=0.5))

mba <- split (x = lastfm [, "artist"], f = lastfm$user)
mba <- lapply (mba, unique)
mba <- as (mba, "transactions")
class(mba)
head(mba)

?apriori
apriori(mba)
image(mba);

apriori(mba)
apriori(mba, parameter=list(support=0.01, confidence=0.5))


rules <- apriori(mba, parameter=list(support=0.01, confidence=0.5));

rules