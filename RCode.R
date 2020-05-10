##### Order Details R Code #####

##### Data Cleaning #####
library(tidyr)
library(dplyr)
library(psych)
library(readxl)

#Fetching CSV File
data_order_raw <- read_excel("Data/OrderDetails.xls",sheet = "Orders")

#Determining class of data_raw
class(data_order_raw)

#Fetching Dimensions of Dataset
dim(data_order_raw)

#Viewing First 5 rows of dataset
head(data_order_raw)

#Overall view of Dataset
glimpse(data_order_raw)

### Cleaning the data
data_order <- data_order_raw

#Checking for missing values in Dataset
any(is.na(data_order))

#Checking for total number of missing values
sum(is.na(data_order))

#Missing values in each column
na_count <- sapply(data_order, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

#Mean of Missing Values in Postal Code
#data_order$postalcode_values <- data_order$`Postal Code`
#data_order$postalcode_values[is.na(data_order$postalcode_values)] <- 0
#any(is.na(data_order$postalcode_values))

data_order_postalcode_mean <- mean(is.na(data_order$`Postal Code`))
data_order_postalcode_mean

#Dropping Columns with more than 80% missing values
data_order <- data_order[, !sapply(data_order, function(y) mean(is.na(y))) > 0.8]

#Summarizing Dataset
summary(data_order)

#Cost Price
data_order$Cost <- data_order$Sales - data_order$Profit

#Profit Share
data_order$Profit_share <- (data_order$Profit/data_order$Sales)*100

##### Data Processing #####
### Data Operations on Sales and Profit of Product
order_sales_profit_globalarea <- data_order %>%
    group_by(`Global Area`) %>%
    summarize(Average_Sales = round(mean(Sales),2), 
              Total_Sales = round(sum(Sales),2),
              Average_Profitshare = round(mean(Profit_share),2), 
              Total_Profit = round(sum(Profit),2)) %>%
    as.data.frame()

order_sales_profit_globalarea_category <- data_order %>%
    group_by( Category, `Global Area`) %>%
    summarize(Average_Sales = round(mean(Sales),2), Total_Sales = round(sum(Sales),2),
              Average_Profit = round(mean(Profit),2), Total_Profit = round(sum(Profit),2)) %>%
    as.data.frame()

order_sales_profit_globalarea_subcategory <- data_order %>%
        group_by(`Sub-Category`, `Global Area`) %>%
        summarize(Average_Sales = round(mean(Sales),2), Total_Sales = round(sum(Sales),2),
                  Average_Profit = round(mean(Profit),2), Total_Profit = round(sum(Profit),2)) %>%
        as.data.frame()

order_sales_profit_region_category <- data_order %>%
        group_by( Region,Category) %>%
        summarize(Average_Sales = round(mean(Sales),2), Sales = round(sum(Sales),2),
                  Average_Profit = round(mean(Profit),2), Profit = round(sum(Profit),2)) %>%
        as.data.frame()

order_sales_profit_region_subcategory <- data_order %>%
        group_by( Region,`Sub-Category`) %>%
        summarize(Average_Sales = round(mean(Sales),2), Sales = round(sum(Sales),2),
                  Average_Profit = round(mean(Profit),2), Profit = round(sum(Profit),2)) %>%
        as.data.frame()

order_sales_profit_customers <- data_order %>%
    group_by(`Customer ID`, `Customer Name`, Segment) %>%
    summarize(Average_Sales = round(mean(Sales),2), Sales = round(sum(Sales),2),
              Average_Profit = round(mean(Profit),2), Profit = round(sum(Profit),2)) %>%
    as.data.frame()

order_sales_profit_category_quantity <- data_order %>%
    group_by(`Sub-Category`) %>%
    summarize(Quantity = sum(Quantity), 
              Sales = sum(Sales), 
              Profit = sum(Profit)) %>%
    as.data.frame()

order_sales_profit_region_priority <- data_order %>%
        group_by(Region, `Order Priority`) %>%
        summarize(Sales = round(sum(Sales)/1000, 2),
                  Profit = round(sum(Profit)/1000,2)) %>%
        as.data.frame()

##### Data Editing #####
library(formattable)

formattable(order_sales_profit_globalarea)
plain_formatter <- formatter("span")
plain_formatter(c(1, 2, 3))

width_formatter <- formatter("span",
                            style = x ~ formattable::style(width = suffix(x, "px")))
width_formatter(c(10, 11, 12))

sign_formatter <- formatter("span", 
                            style = x ~ formattable::style(color = ifelse(x > 0, "green", 
                                                             ifelse(x < 0, "red", "black"))))
sign_formatter(c(-1, 0, 1))

#formattable(order_sales_profit_globalarea)
#formattable(order_sales_profit_globalarea, list(change = sign_formatter))
order_sales_profit_globalarea

formattable(order_sales_profit_globalarea, list(
    `Global Area` = color_tile("transparent", "lightpink"),
    Average_Profitshare = color_bar("lightpink"),
    #market_share = color_bar("lightblue"),
    Total_Sales = color_bar("lightblue"),
    Total_Profit = sign_formatter))

Table <- formattable(order_sales_profit_globalarea, list(
    `Global Area` = color_tile("transparent", "lightpink"),
    #rating = color_bar("lightgreen"),
    Total_Sales = color_bar("lightblue"),
    #revenue = sign_formatter,
    Total_Profit = sign_formatter))

##### Data Visualization #####
library(ggplot2)
#library(hrbrthemes)
library(plotly)
library(ggpubr)
library(gridExtra)
library(ggridges)

#Plot 1: Heatmap of Sales of Sub-Category according to Region
Plot1 <- ggplot(order_sales_profit_region_subcategory, 
       aes(Region, 
           `Sub-Category`)) + 
    geom_tile(aes(fill = Sales)) + 
    geom_text(aes(fill = Sales, 
                  label = round(Sales, 2))) + 
    #scale_fill_gradient(low = "lightblue", high = "darkslategray") + 
    #scale_fill_gradient(low = "#3CAEA3", high = "#173F5F") + 
    #scale_fill_gradient(low = "#A8DADC", high = "#1D3557") + 
    scale_fill_gradient(low = "#74CCC9", high = "#1C4E4C") + 
    theme(panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill="white"), 
          axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"), 
          plot.title = element_text(size=20,face="bold"),
          axis.text.y = element_text(size = 12,face = "bold")) + 
    ggtitle("Heatmap of Sales of Sub-Category according to Region") + 
    theme(legend.title=element_text(face="bold", size=14)) + scale_y_discrete(name="") + 
    scale_x_discrete(name="") + labs(fill="Sales")


#Plot 2: Sales of Category

order_sales_profit_category_subcategory <- data_order %>%
    group_by(Category,`Sub-Category`) %>%
    summarize(Average_Sales = round(mean(Sales),2), Sales = round(sum(Sales)/1000,2),
              Average_Profit = round(mean(Profit),2), Profit = round(sum(Profit)/1000,2)) %>%
    as.data.frame()
names(order_sales_profit_category_subcategory)[names(order_sales_profit_category_subcategory) == "Sales"] <- "Sales (In Thousands)"
names(order_sales_profit_category_subcategory)[names(order_sales_profit_category_subcategory) == "Profit"] <- "Profit (In Thousands)"
order_sales_profit_category_subcategory

order_sales_profit_Furniture <- order_sales_profit_category_subcategory[
    which(order_sales_profit_category_subcategory$Category == 'Furniture'), ]

order_sales_profit_officesupplies <- order_sales_profit_category_subcategory[
    which(order_sales_profit_category_subcategory$Category == 'Office Supplies'), ]

order_sales_profit_Technology <- order_sales_profit_category_subcategory[
    which(order_sales_profit_category_subcategory$Category == 'Technology'), ]

#par(mfrow = c(1,3))

p1 <- ggplot(data = order_sales_profit_Furniture, 
             aes(x = `Sub-Category`, y = order_sales_profit_Furniture$`Sales (In Thousands)`)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = order_sales_profit_Furniture$`Sales (In Thousands)`), vjust = 1.6, color = "white", size = 2.7) +
    theme_minimal()
p1

p2 <- ggplot(data = order_sales_profit_officesupplies, aes(x = `Sub-Category`, y = order_sales_profit_officesupplies$`Sales (In Thousands)`)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = order_sales_profit_officesupplies$`Sales (In Thousands)`), vjust = 1.6, color = "white", size = 2.7) +
    theme_minimal()
p2

p3 <- ggplot(data = order_sales_profit_Technology, aes(x = `Sub-Category`, y = order_sales_profit_Technology$`Sales (In Thousands)`)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = order_sales_profit_Technology$`Sales (In Thousands)`), vjust = 1.6, color = "white", size = 2.7) +
    theme_minimal()
p3

#??grid.arrange
#grid.arrange(p1, p2, p3, nrow = 1)

#order_sales_profit_category_subcategory$Category <- as.factor(order_sales_profit_category_subcategory$Category)


Plot2 <- ggplot(data = order_sales_profit_category_subcategory, 
       aes(x = `Sub-Category`, y = order_sales_profit_category_subcategory$`Sales (In Thousands)`)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = order_sales_profit_category_subcategory$`Sales (In Thousands)`), vjust = 1.6, color = "white", size = 3.5) +
    theme_minimal()
Plot2

#Plot 3: Sales of Product according to region
order_sales_profit_region_priority

Plot3_Sales <- ggplot(data = order_sales_profit_region_priority,
       aes(x = order_sales_profit_region_priority$Sales, 
           y = order_sales_profit_region_priority$Region,
           fill_palette(order_sales_profit_region_priority$`Order Priority`))) +
    geom_bar(stat = "identity", fill = "#CD5C5C") +
    theme_minimal()
    
#Sales Data
Plot3_Sales1 <- ggplot(order_sales_profit_region_priority, 
       aes(x = Sales,
           fill = `Order Priority`)) +
    geom_density(alpha = 0.4)
Plot3_Sales1

Plot3_Sales2 <- ggplot(order_sales_profit_region_priority, 
       aes(x = Sales, 
           y = Region, 
           fill = Region)) +
    geom_density_ridges() + 
    theme_ridges() +
    labs("Sales Distribution according to Region") +
    theme(legend.position = "none")

#Profit Data

Plot3_Profit1 <- ggplot(order_sales_profit_region_priority, 
       aes(x = Profit,
           fill = `Order Priority`)) +
    geom_density(alpha = 0.4)

Plot3_Profit2 <- ggplot(order_sales_profit_region_priority, 
       aes(x = Profit, 
           y = Region, 
           fill = Region)) +
    geom_density_ridges() + 
    theme_ridges() +
    ggtitle("Profit Distribution according to Region") +
    #labs("Profit Distribution according to Region") +
    theme(legend.position = "none")
