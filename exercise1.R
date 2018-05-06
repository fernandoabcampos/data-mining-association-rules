library(arules)
my_file <- "lastfm.csv"
?read.transactions
cat("\n", file = my_file, append = TRUE)
tdata <- read.transactions(file = my_file, rm.duplicates = FALSE, skip = 1, sep = ",")
head(tdata)
class(tdata)
inspect(head(tdata))

frequentItems <- eclat (tdata, parameter = list(supp = 0.07, maxlen = 15))
inspect(frequentItems)
itemFrequencyPlot(tdata, topN=15, type="absolute", main="Item Frequency") 

?apriori
apriori(mba)
rules <- apriori (tdata, parameter = list(supp = 0.001, conf = 0.5))
rules_conf <- sort (rules, by="confidence", decreasing=TRUE)
inspect(head(rules_conf))
rules_lift <- sort (rules, by="lift", decreasing=TRUE) 
inspect(head(rules_lift))


#Suggestion
### Queremos una lista por cada usuario de los grupos que escucha. Eliminan sexo y país
#mba <- split (x = fichero_leido (, “artist”], f = lastfm$user))
### Si hay duplicados los eliminamos
#mba <- laaply (mba, unique)
## y ahora binarizamos los datos
#mba <- as (mba, “transactions”)
