orders <- read.csv("orders.csv")
products <- read.csv("products.csv")

basket <- orders %>% 
  inner_join(products, by="product_id") %>% 
  group_by(order_id) %>%
  summarise(basket = as.vector(list(product_name)))


head(basket)
summary(basket)
nrow(basket)
ncol(basket)
str(basket)

order_baskets$basket <- lapply (order_baskets$basket, unique)
tx <- as(order_baskets$basket, "transactions")
summary(tx)
inspect(tx[1:3])

hist(size(tx), breaks = 0:70, xaxt="n", ylim=c(0,700), 
     main = "Cantidad de artículos por 'basket'", xlab = "Cantidad de Ítems", col = rainbow(size(tx)))
axis(1, at=seq(0,70,by=10), cex.axis=0.8)
mtext(paste("Total de:", length(tx), "baskets,", sum(size(tx)), "ítems"))


