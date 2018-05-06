library(readr)
library(dplyr)
library(tidyr)
library(arules)
library(arulesViz)
library(methods)
library(plotrix)

# ejercicio 2 -------------------------------------------
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
dist_sex

head(table(lastfm$country))


lbls <- paste(names(dist_sex),  "\n" , dist_sex, sep= "" ) 
pie3D(dist_sex
      , radius= 0.9
      , labelcex=  0.8
      , labels = lbls
      , explode= 0.1
      , main= "Distribución por Sexo" , col = rainbow(length(lbls)
      ))

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

mba <- split (x = lastfm [,  "artist" ], f = lastfm$user) 
mba <- lapply (mba, unique)
mba <- as (mba,  "transactions" )
class(mba)
head(mba)

my_file <-  "lastfm.csv"
#Habia un warning de end of line, por esto el cat
cat( "\n" , file = my_file, append =  TRUE )
tdata <- read.transactions(file = my_file, rm.duplicates =  TRUE , skip =  1 , sep = "," )
head(tdata)
class(tdata)
inspect(head(tdata))



apriori(mba, parameter=list(support= 0.01 , confidence= 0.5 ))
apriori(tdata, parameter=list(support= 0.01 , confidence= 0.5 ))

frequent_items_mba <- eclat (mba, parameter = list(supp =  0.07 , maxlen =  15 ))
inspect(frequent_items_mba)
frequent_items_tdata <- eclat (tdata, parameter = list(supp =  0.07 , maxlen =  15 ))
inspect(frequent_items_tdata)

rules_mba <- apriori(mba, parameter=list(support= 0.015 , confidence= 0.5 ))
rules_tdata <- apriori(tdata, parameter=list(support= 0.015 , confidence= 0.5 ))

items(rules_mba)
items(rules_tdata)
inspect(rules_mba)
inspect(rules_tdata)

rules_conf <- sort (rules_mba, by= "confidence" , decreasing= TRUE ) 
inspect(rules_conf)
rules_lift <- sort (rules_mba, by= "lift" , decreasing= TRUE ) 
inspect(rules_lift)
plot(rules_mba)

rules_mba <- apriori(mba, parameter=list(support= 0.015 , confidence= 0.5 , ext= TRUE ))
inspect(rules_mba)

# ejercicio 3 -------------------------------------------
orders <- read.csv("orders.csv", nrows=100000)
products <- read.csv("products.csv")

order_baskets <- orders %>% 
  inner_join(products, by="product_id") %>% 
  group_by(order_id) %>%
  summarise(basket = as.vector(list(product_name)))

head(order_baskets$basket)
str(order_baskets$basket)


order_baskets$basket <- lapply (order_baskets$basket, unique)
tx <- as(order_baskets$basket, "transactions")
summary(tx)
inspect(tx[1:3])

hist(size(tx), breaks = 0:70, xaxt="n", ylim=c(0,700), 
     main = "Cantidad de artículos por 'basket'", xlab = "Cantidad de Ítems", col = rainbow(size(tx)))
axis(1, at=seq(0,70,by=10), cex.axis=0.8)
mtext(paste("Total de:", length(tx), "baskets,", sum(size(tx)), "ítems"))


apriori(tx, parameter=list(support=0.003, confidence=0.3, ext=TRUE))

frecuentes <- apriori(tx, parameter = list(target = "frequent itemsets", supp=0.008, minlen=2), control = list(verbose = FALSE))
inspect(frecuentes)

frecuentes_eclat <- eclat (tx, parameter = list(supp = 0.003, maxlen = 30))
inspect(head(frecuentes_eclat))

rules <- apriori(tx, parameter = list(support=0.003, confidence=0.3, ext=TRUE, maxlen=3), control = list(verbose = TRUE)) 
summary(rules)
inspect(rules)
summary(quality(rules))

plot(rules)

rules_conf <- sort (rules, by="confidence", decreasing=TRUE)
inspect(rules_conf[1:10])
rules_lift <- sort (rules, by="lift", decreasing=TRUE) 
inspect(rules_lift[1:10])

