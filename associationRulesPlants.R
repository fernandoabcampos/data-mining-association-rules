plants_csv <- read.csv(file = "plants.csv", header = FALSE)
unlist(lapply(plants_csv, function(x) any(is.na(x))))
head(plants_csv)
summary(plants_csv)
nrow(plants_csv)
ncol(plants_csv)
str(plants_csv)

#qtt_plants <- aggregate(data.frame(count = plants_csv$V1), 
                       #by = list(value = plants_csv$V1), 
                       #FUN = length)
#nrow(qtt_plants)
#qtt_plants[order(qtt_plants$count,  decreasing = TRUE), ]


t_plants <- read.transactions(file = "plants.csv", rm.duplicates = TRUE, skip = 0, sep = ",")
summary(t_plants)
inspect(t_plants[1:5])


rules <- apriori(t_plants, parameter=list(supp=0.12, conf=0.4, minlen=2, maxlen=8, ext=TRUE, maxtime=30))
inspect(rules)

frequent_items_plants <- eclat (t_plants, parameter = list(supp = 0.07, maxlen = 15))



rules_plants <- apriori(t_plants, parameter=list(support=0.07, confidence=0.5, ext=TRUE))
inspect(rules_plants)