library(dplyr)
library(ggplot2)
getwd()
setwd("C:/Users/tttoa/OneDrive - VNU-HCMUS/KHTN/Subject/Nam3_HK3_KHDL/R/ProductSale")
getwd()

## 1
sales = read.csv("sales_ug.csv")
head(sales,2)

get_store <- function(a){
  subset(sales, subset = (store_id == a))
}

store_name = unique(sales$store_id)
storeRevenue = lapply(store_name, get_store)
                      
computeTotalRevenue <- function(sales){
  sum_revenue <- tapply(sales$revenue, sales$date, sum)
  dateRevenue <- as.Date(names(sum_revenue))
  df <- data.frame(Date = dateRevenue, Revenue = sum_revenue)
  return(df)
}

storeTotalRevenue <- lapply(storeRevenue, computeTotalRevenue)

head(storeTotalRevenue,3)

sevenDayRevenue = tapply(sales$revenue, sales$date, sum)
sevenDayRevenue

plot(sevenDayRevenue, xlab = "Day", ylab = "Revenue", type = "l", col = 3)


##2
products=read.csv("product_hierarchy.csv")
head(products, 2)

pro_count=table(products$hierarchy1_id)
pro_count

mostPopularProduct=names(pro_count[pro_count==max(pro_count)])
mostPopularProduct

a=which(products$hierarchy1_id==mostPopularProduct)
mostPopularProductID=products$product_id[a]

id=which(sales$product_id%in%mostPopularProductID)
sum(sales$revenue[id])

pro_count=pro_count[order(pro_count,decreasing=TRUE)]
pro_count

secondPopularProduct=names(pro_count[2])
secondPopularProduct

a=which(products$hierarchy1_id==secondPopularProduct)
secondPopularProductID=products$product_id[a]
id=which(sales$product_id%in%secondPopularProductID)
sum(sales$revenue[id])

pro_count

hi1=names(pro_count)
b=data.frame()
getProductData<-function(a){
  product1=subset(products,hierarchy1_id==a)
  numSubtypes=length(unique(product1$hierarchy2_id))
  numProducts=length(unique(product1$product_id))
  ProductID=unique(product1$product_id)
  id=which(sales$product_id%in% ProductID)
  quantity=sum(sales$sales[id])
  sumRevenue=sum(sales$revenue[id])
  a=data.frame(productType1=a, numberOfsubType2=numSubtypes, numberOfProducts=numProducts,
               saleQuantity= quantity, totalRevenue = sumRevenue)
}
a=lapply(hi1,getProductData)

sumSalesData=bind_rows(a[1],a[2],a[3],a[4])
sumSalesData

cor(sumSalesData$saleQuantity,sumSalesData$totalRevenue)


##3
store_cities=read.csv("store_cities.csv")
head(store_cities)

storeType=table(store_cities$storetype_id)
storeType=storeType[order(storeType,decreasing=TRUE)]
storeType

firstTypeID=which(store_cities$storetype_id==names(storeType[1]))
secondTypeID=which(store_cities$storetype_id==names(storeType[2]))
firstTypeStoreID = store_cities$store_id[firstTypeID]
secondTypeStoreID = store_cities$store_id[secondTypeID]

sum(sales$sales[which(sales$store_id %in% firstTypeStoreID)])
sum(sales$sales[which(sales$store_id %in% secondTypeStoreID)])
sum(sales$revenue[which(sales$store_id %in% firstTypeStoreID)])
sum(sales$revenue[which(sales$store_id %in% secondTypeStoreID)])

sizeXrevenue = left_join(sales, store_cities, by = "store_id") %>% select(store_id, store_size, revenue)
head(sizeXrevenue)

fit = lm(store_size~revenue, data = sizeXrevenue)
summary(fit)


co=cor(sizeXrevenue$store_size,sizeXrevenue$revenue)
print(co)

ggplot(data=sizeXrevenue)+geom_point(mapping=aes(x=store_size,y=revenue))+
labs(title="Store size and revenue")

##4
tapply(sales$promo_bin_1,sales$promo_type_1,unique)

salesByType = tapply(sales$sales,sales$promo_type_1, sum)
plot(salesByType, col = 1)

names(which(salesByType == max(salesByType)))
salesXlevel = tapply(sales$sales, sales$promo_bin_1, sum)
salesXlevel

sales$promo_bin_1[sales$promo_bin_1 == ""] <- NA
promoLevel = aggregate(revenue ~ promo_bin_1, data = sales, FUN = sum, na.rm = TRUE)
promotionLevel = c("verylow", "low", "moderate", "high", "veryhigh")
promoLevel <- promoLevel[match(promotionLevel, promoLevel$promo_bin_1), ]
plot(promoLevel$revenue, xlab = "Promotion Level", ylab = "Revenue",
      main = "Revenue based on promotion", type = "l")
















