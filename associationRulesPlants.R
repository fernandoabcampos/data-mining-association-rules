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
