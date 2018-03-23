library(lubridate)
library(stringr)
library(caret)
library(MASS)
library(dplyr)

# Load data, combin labeled und unlabled data for data preparation
df = read.csv("BADS_WS1718_known.csv", colClasses = c("integer", "Date", "Date", "factor", 
    "character", "factor", "factor", "numeric", "factor", "factor", "Date", "factor", "Date", 
    "integer"))

df_class = read.csv("../data/BADS_WS1718_class_20180115.csv", colClasses = c("integer", "Date", 
    "Date", "factor", "character", "factor", "factor", "numeric", "factor", "factor", "Date", 
    "factor", "Date"))
df_class$return = rep(NA, nrow(df_class))
df = rbind(df, df_class)

# df$missing_delivery = ifelse(is.na(df$delivery_date),1,0)
df$delivery_date[df$delivery_date < df$order_date] = NA
df$item_size = as.factor(toupper(df$item_size))

# split age into age groups taking into account return rate and group sizes after
# graphical analysis
df$age = round(as.integer(df$order_date - df$user_dob)/365)
# values will be treated as a categorie itself. Implausible ages form a in the higest
# sparse age group
df$age[is.na(df$age)] = -1
df$age_group = cut(as.integer(df$age), breaks = c(-2, 0, 42, 46, 51, 58, 120))

# reduce title to 'Mrs' and 'other' due to low Freq of other categories
df$user_title = as.factor(ifelse(df$user_title == "Mrs", "Mrs", "other"))

df$shipment_time = as.integer(df$delivery_date - df$order_date)
# more than 50 days seems implausible we therefore assume an error in the data
df$shipment_time[df$shipment_time > 50] = NA

# set.seed(123) inTrainingSet <- createDataPartition(df$return, p = 0.8, list=FALSE)
# true_y = df[-inTrainingSet,'return'] df[-inTrainingSet,'return'] = NA

item_df = data.frame(table(df$item_id), item_return_rate = tapply(df$return, df$item_id, function(x) mean(x, 
    na.rm = T)), item_max_price = tapply(df$item_price, df$item_id, function(x) max(x, na.rm = T)))
names(item_df)[1:2] = c("item_id", "total_ordered")
item_df$item_return_rate[is.nan(item_df$item_return_rate)] = NA

add_cust_vars = function(cust) {
    # adds variables on customer and basket level
    cust$cust_return_rate = rep(mean(cust$return, na.rm = T), nrow(cust))
    cust$cust_item_count = rep(nrow(cust[!is.na(cust$return), ]), nrow(cust))
    cust$size_dupl = duplicated(cust$item_size) | duplicated(cust$item_size, fromLast = T)
    cust$color_dupl = duplicated(cust$item_color) | duplicated(cust$item_color, fromLast = T)
    cust$order_no = as.integer(as.factor(cust$order_date))
    cust$order_date = as.character(cust$order_date)
    cust = inner_join(cust, data.frame(table(cust$order_date)), by = c(order_date = "Var1"))
    cust = rename(cust, Basket_Size = Freq)
    cust$order_date = ymd(cust$order_date)
    cust = cust[c(1, 18:23)]
    return(cust)
}

# add customer and basket features to data set
customers = split(df, df$user_id)
customers = lapply(customers, add_cust_vars)
customers_df = do.call(rbind, customers)

df2 = inner_join(df, customers_df)
df2 = inner_join(df2, item_df)
df2$discount = df2$item_max_price - df$item_price
# save results 
write.csv(df2, 'full_prepared.csv')

