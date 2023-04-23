# Part 1
# a) 
my_data = read.csv("LaptopSales (1).csv")
head(my_data)
# b) missing data at OS.X.Store, OS.Y.store, and CustomerStoreDistance
summary(my_data)
which(is.na(my_data$OS.X.Store))
which(is.na(my_data$OS.Y.Store))
which(is.na(my_data$CustomerStoreDistance))
### missing values in row 1675 1774 1969 2203

# c) mean: 489.8, median 490

# d) 
data_integrated_wireless <- subset(my_data, Integrated.Wireless. ==
                                    "Yes")
data_non_intergrated_wireless <- subset(my_data, Integrated.Wireless. != "Yes")
summary(data_integrated_wireless)
summary(data_non_intergrated_wireless)
### Average price of a laptop with Integrated Wireless $495.9
### Average price of a laptop without Integrated Wireless $483.3 

# e)
my_data_sorted <- my_data[order(my_data$Retail.Price, decreasing = TRUE),]
my_data_sorted[1, ]
### Configuration type with the highest price is 359

# f) 
sum(my_data$HD.Size..GB. < 150)
### 1749

# g)
sum(my_data$Retail.Price)
### Total price = $ 1231470


### Part2
library(ggplot2)
# a) 
summary(my_data)
ggplot(data= my_data, aes(x = CustomerStoreDistance, fill = ..count..)) + 
  geom_histogram(alpha=1) +
  scale_fill_gradient(low="purple", high="darkblue") + 
  ggtitle("Distrubution of Customer Store Distance") + 
  labs(x = "Customer Store Distance", y = "Count")

### Insights: We could see from the data that as the distance grow, counts decreases. 
# Therefore, distance is one of an important aspect of shopping for customers. 
# Also, we could see that counts are the highest between 2500 – approximately 4000  

# b)
ggplot(data = my_data, aes(y=Retail.Price)) +
  geom_boxplot(notch = TRUE, outlier.colour="orange", outlier.shape=2, outlier.size=3) + 
  ggtitle("Box plot for Retail Price") + 
  labs(y = "Retail Price")
### Insights: We could see from the boxplot that most of the Retail Prices are 
# in the range from 455 – 530. 
# Also, there are more outliers in the minimum side than the maximum side. 


# c) 
ggplot(data <- my_data, aes(x = HD.Size..GB., y=Retail.Price, group = HD.Size..GB.
, fill = HD.Size..GB.)) +
  geom_boxplot(notch = TRUE, outlier.colour="red", outlier.shape=1, outlier.size=3) + 
  scale_fill_gradient(low="blue", high="red") + 
  ggtitle("Retail Price by HD Size GB") + 
  labs(x = "HD Size GB", y = "Retail Price")
### Insights: The four box plot shows that as the HD Size GB become larger the the price will 
# increase too. Also, there are couple of outliers in the purple boxplot which is the size 80. 

# d) 
# part a) 
ggplot(data <- my_data, aes(x= Battery.Life..Hours., y = Retail.Price, color = Battery.Life..Hours. )) + 
  geom_point() + 
  ggtitle("Relationship between Battery life and price") + 
  labs(x = "Battery life in hours", y = "Retail Price")

### Insights: We could see from the graph that as battery life hours increase, so do the price. 
# The highest price with 6 hour Battery life is way higher than the highest price with 
# 4 hour battery life


# part b)
my_data$Battery.Cat[my_data$Battery.Life..Hours. == 4] <- "Battery Life 4 hours"
my_data$Battery.Cat[my_data$Battery.Life..Hours. == 5] <- "Battery Life 5 hours"
my_data$Battery.Cat[my_data$Battery.Life..Hours. == 6] <- "Battery Life 6 hours"
ggplot(data= my_data, aes(x = Retail.Price, fill = Battery.Cat)) + 
  geom_histogram(alpha=0.8) + 
  ggtitle("Retail price by battery category") + 
  labs(x = "Retail Price", y = "Count" )

### Insights: We could see from the graph that battery life with 6 hours appears more 
# in the right side of the graph than 4 hours and 5 hours. This means battery life with 
# 6 hours are priced higher than them. Histogram makes the comaprison easier than the scatter plot




