library(readxl)
library(readr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plotly)
library(arules)
library(viridis)
library(arulesViz)


market <- read_excel("C:/Users/ibeha/OneDrive/Desktop/MY R DATASET/K-NN market basket analysis/Assignment-1_Data.xlsx")
View(market)

#When loading the dataset we can see that there is a warning message saying expecting number but getting
#numeric with alphebets.

#lets see the shape of the datase
dim(market)

#data contains 522064 observations and 7 features.
#lets see the structure of the datase
str(market)
#The data are in the right structure

#lets perform a summary statistic
summary(market)
##data shows that there anr nagative quantity and price, and missing numbers in Bill number
#DATA CLEANING
##lets see the missing numbers
missing_values <- colSums(is.na(market))
missing_values

# Filter rows where 'BillNo' column contains non-digit values
filtered_df <- market[!grepl("^\\d+$", market$BillNo), ]
filtered_df
#we see that the rows for bills that contains non-digit number 
market  <- market [market$Itemname != "Adjust bad debt", ]
market<- market %>%
  filter(!(Itemname %in% c("POSTAGE", "DOTCOM POSTAGE", "Adjust bad debt", "Manual")))

# Check if all 'BillNo' values do not include letters
all_digits <- all(!grepl("[A-Za-z]", market$BillNo))

#drop rows where price is greater than 0
market <- subset(market, Quantity > 0)
market <- subset(market, Price > 0)

#let us seperate the date dataset to year and month
market$year <- year(market$Date)
market$month <- month(market$Date)
market$TotalPrice <- market$Quantity * market$Price

new <- subset(market, year != 2010)
###Exploratory data analysis
total_price <- market %>% 
  group_by(year,month) %>%
  summarise(Total_Price = sum(TotalPrice), total_quantity = sum(Quantity)) 

total_price_df_long <- pivot_longer(total_price, 
                                    cols = c(Total_Price, total_quantity), 
                                    names_to = "Variable", 
                                    values_to = "Value")

# Create a bar plot for quantity and price
ggplot(total_price_df_long, aes(x = interaction(year, month), y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Quantity and Price Over Time", x = "Year-Month", y = "Value", fill = "Variable") +
  scale_fill_manual(values = c("Total_Price" = "blue", "total_quantity" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##september, october november has the highest quantity and sales

monthly_sales <- market %>%
  group_by(month,Country) %>%
  summarise(month_sales = sum(TotalPrice)) 

plot_ly(data = monthly_sales, x = ~factor(month, levels = 1:12), y = ~month_sales, 
        type = 'bar', color = ~Country, text = ~paste("Country: ", Country, "<br>Value: $", month_sales)) %>%
  layout(title = "Monthly Sales by Country",
         xaxis = list(title = "Month", tickvals = 1:12, ticktext = month.abb[1:12]),
         yaxis = list(title = "Sales"),
         barmode = 'stack')

###we can see that united kingdom has the highest sales for each months

# Summarize total purchase quantity by item

item_purchase <- market %>%
  group_by(Itemname) %>%
  summarise(total_quantity = sum(Quantity)) %>%
  ungroup() %>% 
  arrange(desc(total_quantity))  
top_10_items <- item_purchase %>%
  head(10)
top_10_items

country_fre <- market %>%
  group_by(Country) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq))


#united-kingdome, Germany, France and Spain has the highest number of dataset
##########################
market <- market %>% filter(Quantity > 0,
                        Price > 0,
                        Itemname != "")

market %>%
  count(Itemname) %>%
  arrange(desc(n)) %>%
  top_n(n = 10)

#Lets perform our analysis 

# Well... United Kingdom is the top 1 by far, so let's run the MBA for this country
data.uk <- market %>% filter(Country == "United Kingdom")

# First, we need to create a list structure and then transform it into a transactions format

## List formatting
data.list <- split(x = data.uk$Itemname,
                   f = data.uk$BillNo)
## Transactions formatting
data.transactions <- as(object = data.list,
                        Class = "transactions")
class(data.transactions)
inspect(head(data.transactions))

# Plotting the most purchased items
itemFrequencyPlot(x = data.transactions,
                  type = "relative",
                  topN = 20,
                  horiz = T,
                  col = viridis(n = 20))

pdf("item_frequency_plot.pdf", width = 10, height = 6)
itemFrequencyPlot(x = data.transactions, type = "relative", topN = 20, horiz = TRUE, col = viridis(n = 20))
dev.off()  # Don't forget to close the device

# To avoid processing  issues, let's define some boundaries with apriori algorithm
rules <- apriori(data = data.transactions,
                 parameter = list(supp = .01,
                                  conf = .5,
                                  minlen = 2,
                                  maxlen = 5,
                                  target = "rules"))
#supp: This is short for "support." It specifies the minimum support threshold. Support is a measure of how frequently an itemset (a set of items) appears in the dataset. 
#The supp parameter of 0.01 means that only itemsets that appear in at least 1% of the transactions will be considered.
#This is short for "confidence." It specifies the minimum confidence threshold. Confidence measures how often a rule is true. 
#A confidence of 0.5 (or 50%) means that a rule must be true at least 50% of the time for it to be considered.
#minlen and maxlen: These parameters specify the minimum and maximum length of the rules to be generated. In this case, it generates rules with a minimum length of 2 items and a maximum length of 5 items.
#target: This parameter specifies the type of result you want. In this case, "rules" indicates that you want to generate association rules.

###959 rules                                                                                                                                               
inspect(head(rules, n = 10), ruleSep = "----->", itemsep = " + ")
#In the top list value of the purchase, it means that customers are 75% more likely to buy
#"GREEN REGENCY TEACUP AND SAUCER" if you sell "PINK REGENCY TEACUP AND SAUCER".

#rules by by lift
ruleslift = sort (rules, by = "lift", decrease = TRUE)
inspect(head(ruleslift, n= 10))

#rules by confidence
rulesConf = sort (rules, by = "confidence", decrease = TRUE)
inspect(head(rulesConf, n = 10))

#In the top confidence value of the purchase, it means that 92% of the customers who bought
#{HERB MARKER CHIVES} also bought {HERB MARKER PARSLEY} 
#rules by support
rulesSupp = sort (rules, by = "support", decrease = TRUE)
inspect(head(rulesSupp, n = 10))
#In the top support value of purchase, it means that "JUMBO BAG PINK RETROSPOT" is present in 4% of all purchases
#plot 10 items
par(mar=c(5, 5, 2, 2))
# Create the item frequency plot
itemFrequencyPlot(data.transactions, topN = 10, type = "absolute", main = "Top 10 items")

#calculating support for most frequent items
itemsets = eclat(data.transactions,parameter = list(supp = 0.03,maxlen=3))
inspect(itemsets)

##we see the set of items that goes hand in hand

#####
#achieve highest possible turnover
inspect(head(rules, n = 10))

plot(rules,
     method = "graph",
     shading = "support",
     engine = "htmlwidget")

###########################################

## List formatting
data <- split(x = market$Itemname,
                   f = market$BillNo)
## Transactions formatting
data.transactions <- as(object = data,
                        Class = "transactions")
class(data.transactions)
inspect(head(data.transactions))

windows()  # For Windows

# Create the item frequency plot
itemFrequencyPlot(x = data.transactions,
                  type = "relative",
                  topN = 10,
                  horiz = TRUE,
                  col = viridis(n = 10))
# Plotting the most purchased items

# To avoid processing  issues, let's define some boundaries with apriori algorithm
rules <- apriori(data = data.transactions,
                 parameter = list(supp = .01,
                                  conf = .5,
                                  minlen = 2,
                                  maxlen = 5,
                                  target = "rules"))
#supp: This is short for "support." It specifies the minimum support threshold. Support is a measure of how frequently an itemset (a set of items) appears in the dataset. 
#The supp parameter of 0.01 means that only itemsets that appear in at least 1% of the transactions will be considered.
#This is short for "confidence." It specifies the minimum confidence threshold. Confidence measures how often a rule is true. 
#A confidence of 0.5 (or 50%) means that a rule must be true at least 50% of the time for it to be considered.
#minlen and maxlen: These parameters specify the minimum and maximum length of the rules to be generated. In this case, it generates rules with a minimum length of 2 items and a maximum length of 5 items.
#target: This parameter specifies the type of result you want. In this case, "rules" indicates that you want to generate association rules.

###803 rules                                                                                                                                               
inspect(head(rules, n = 10), ruleSep = "----->", itemsep = " + ")

#rules by by lift
ruleslift = sort (rules, by = "lift", decrease = TRUE)
inspect(head(ruleslift, n= 10))
#In the top list value of the purchase, it means that customers are 75% more likely to buy
#"GREEN REGENCY TEACUP AND SAUCER" if you sell "PINK REGENCY TEACUP AND SAUCER".

#rules by confidence
rulesConf = sort (rules, by = "confidence", decrease = TRUE)
inspect(head(rulesConf, n = 10))

#In the top confidence value of the purchase, it means that 96% of the customers who bought
#{HERB MARKER MINT, HERB MARKER THYME}  also bought {HERB MARKER ROSEMARY}also bought {HERB MARKER PARSLEY} 
#rules by support
rulesSupp = sort (rules, by = "support", decrease = TRUE)
inspect(head(rulesSupp, n = 10))
#In the top support value of purchase, it means that "{JUMBO BAG PINK POLKADOT also {JUMBO BAG RED RETROSPOT}" is present in 4% of all purchases
#plot 10 items
par(mar=c(5, 5, 2, 2))
# Create the item frequency plot
itemFrequencyPlot(data.transactions, topN = 10, type = "absolute", main = "Top 10 items")
#calculating support for most frequent items
itemsets = eclat(data.transactions,parameter = list(supp = 0.03,maxlen=3))
inspect(itemsets)
##we see the set of items that goes hand in hand

#####
#achieve highest possible turnover
inspect(head(rules, n = 10))

plot(rules,
     method = "graph",
     shading = "support",
     engine = "htmlwidget")

###########################################

#the most purchase item is 


