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


rules <- apriori (tdata, parameter = list(supp = 0.001, conf = 0.5))
rules_conf <- sort (rules, by="confidence", decreasing=TRUE)
inspect(head(rules_conf))
rules_lift <- sort (rules, by="lift", decreasing=TRUE) 
inspect(head(rules_lift))