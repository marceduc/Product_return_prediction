library(lubridate)
library(stringr)
library(forecast)
library(reshape2)
library(caret)

load_clean = function() {
    # loads and formats raw data
    df       = read.csv("BADS_WS1718_known.csv", colClasses = c("integer", 
                                                                "Date", "Date", "integer", "character", "character", "integer", "numeric", "factor", 
                                                                "factor", "Date", "factor", "Date", "integer"))
    
    df_class = read.csv("BADS_WS1718_class_20180115.csv", colClasses = c("integer", 
                                                                         "Date", "Date", "integer", "character", "character", "integer", "numeric", "factor", 
                                                                         "factor", "Date", "factor", "Date"))
    # combine known & class data sets to engineer features on customer and basket level
    df_class$return = rep(NA, nrow(df_class))
    df = rbind(df, df_class)
    rm(df_class)
    
    # capitalize size values
    df$item_size = toupper(df$item_size)
    
    # add missing_delivery to distinguish prior NAs and implausible values
    df$missing_delivery = as.integer(ifelse(is.na(df$delivery_date), 1, 0))
    df$delivery_date[df$delivery_date < df$order_date] = NA
    
    df
}

create_time_vars = function(df) {
    # clean time variables delivery_date, order_date,user_reg_date in raw df creates various
    # time features
    
    df$shipment_time = as.integer(df$delivery_date - df$order_date)
    # impute delivery_date and shipment time based on avg shipment time
    avg_shipment_time                         = round(mean(df$shipment_time, na.rm = T))
    df$shipment_time[is.na(df$shipment_time)] = avg_shipment_time
    df$delivery_date[is.na(df$delivery_date)] = df$order_date[is.na(df$delivery_date)] + days(avg_shipment_time)
    
    #create days since delivery and time series feature
    df$days_since_reg = as.integer(df$order_date - df$user_reg_date)
    df$TS             = get_TS(df)
    # create features for weekday, month and day of the month for order and delivery date
    df$ord_wd  = factor(wday(df$order_date, label = T), ordered = F)
    df$del_wd  = factor(wday(df$delivery_date, label = T), ordered = F)
    
    df$ord_mon = factor(month(df$order_date, label = T), ordered = F)
    df$del_mon = factor(month(df$delivery_date, label = T), ordered = F)
    
    df$ord_md  = day(df$order_date)
    df$del_md  = day(df$delivery_date)
    
    return(df)
}

get_TS = function(df){
    #create Time Series of avg return rates for every date
    #sums up trend as linear regression over time and saisonal effects as moving average if detrendet TimeSeries
    #calculates TS = linear tend + saisonal efects
    t                     = tapply(df$return[1:100000],df$order_date[1:100000], mean)
    
    time_frame            = data.frame(order_date= as.Date(names(t)), return_rate = t )
    time_frame            = time_frame[order(time_frame$order_date),]
    
    trend_model           = lm(time_frame$return_rate ~time_frame$order_date )
    
    time_frame$trend      = predict(trend_model)
    time_frame$trend_rmvd = time_frame$return_rate - time_frame$trend
    
    #duplicate trend_rmvd to avoid missing values at start and end of moving average
    trend_rmvd = c(rep(time_frame$trend_rmvd,3))
    dates      = c((time_frame$order_date - years(1)), time_frame$order_date, time_frame$order_date+ years(1) )
    #calculate moving avg to capature seasonal effects
    ma = ma(trend_rmvd, 30)
    #reduce moving average back to orginal values
    ma2           = ma[366:(366+364)]
    time_frame$TS = time_frame$trend + ma2
    TS            = time_frame$TS[(match(df$order_date, time_frame$order_date))]
    
    return(TS)
}

lookup = function(x) {
    # returns vector of frequencies with the length of x
    
    lookup_table = table(x)
    value_count_vec = lookup_table[match(x, names(lookup_table))]
    return(value_count_vec)
}

add_basket_dups = function(df, feat_name) {
    # adds the counts of duplicated values
    dups_name       = paste0(feat_name, "_dups")
    df[, dups_name] = as.integer(ave(df[, feat_name], as.factor(df[, "basket_id"]), FUN = lookup))
    df
}

brand_clust = function(df, id1 = "user_id", id2 = "brand_id", min_count = 100) {
    # assignes less frequent brands to more frequent ones
    
    df     = df[, c(id1, id2)]
    freq_2 = table(df[, id2])
    # assigne new id to all single occurances
    unique_id2s                         = names(freq_2)[freq_2 == 1]
    df[df[, id2] %in% unique_id2s, id2] = "999999"
    
    # update freq table
    freq_2 = table(df[, id2])
    
    while (min(freq_2) < min_count) {
        # get ids with low frequencies
        low_counts = names(freq_2)[freq_2 < min_count]
        # create cross table of customer ids and brand ids containing order counts
        t             = data.frame(table(df$brand_id, df$user_id))
        names(t)[1:2] = c(id2, id1)
        t_wide        = dcast(t, user_id ~ brand_id)
        # create correlation matrix between brands
        cor = cor(t_wide[, 2:ncol(t_wide)])
        
        #assigne all ids with less than min counts to the id with the highest correlation
        id = low_counts[1]
        for (id in low_counts) {
            # get correlation of ids
            row = cor[id, ][cor[id, ] != 1]
            # assign id to highest correlated category
            assigne_to = names(row[which.max(row)])
            print(paste("assigne: ", id, " to: ", assigne_to))
            df[df[, id2] == id, id2] = assigne_to
        }
        
        freq_2 = table(df[, id2])
        print("min_count:")
        print(min(freq_2))
    }
    # return updated brand ids
    categ = df$brand_id
    return(categ)
}


item_clust = function(df, id1 = "user_id", id2 = "item_id", min_count = 100) {
    # assignes less frequent items to more frequent ones works equivalent to brand_clust has
    # been rewriten because we couldnt use variable feature names as input to the dcast
    # function
    df = df[, c(id1, id2)]
    
    freq_2 = table(df[, id2])
    # assigne new id to all single occurances
    unique_id2s = names(freq_2)[freq_2 == 1]
    df[df[, id2] %in% unique_id2s, id2] = "999999"
    
    # update freq table
    freq_2 = table(df[, id2])
    
    print("min_count:")
    print(min(freq_2))
    
    while (min(freq_2) < min_count) {
        
        low_counts = names(freq_2)[freq_2 < min_count]
        
        
        t = data.frame(table(df$item_id, df$user_id))
        names(t)[1:2] = c(id2, id1)
        
        # create cross table of customer ids and item ids containing order counts
        t_wide = dcast(t, user_id ~ item_id)
        # create correlation matrix between brands
        cor = cor(t_wide[, 2:ncol(t_wide)])
        
        id = low_counts[1]
        
        
        for (id in low_counts) {
            # get correlation of id
            row = cor[id, ][cor[id, ] != 1]
            # assigne id to highest correlated category
            assigne_to = names(row[which.max(row)])
            print(paste("assigne: ", id, " to: ", assigne_to))
            df[df[, id2] == id, id2] = assigne_to
        }
        
        freq_2 = table(df[, id2])
        print("min_count:")
        print(min(freq_2))
    }
    # return updated brand ids
    categ = df$item_id
    return(categ)
}



clean_col = function(col) {
    # assignes low frequent colors to more frequent ones based on RGB distance
    col_df = read.csv("clean_col.csv", stringsAsFactors = F)
    col_vec = col_df$col3[match(col, col_df$item_color)]
    return(col_vec)
}


create_features = function(df) {
    #creates various features base on the raw data set
    
    # combine rare user_titles
    df$user_title = as.factor(ifelse(df$user_title == "Mrs", "female", "other"))
    
    # impute delivery_date with means, add time related Features
    df = create_time_vars(df)
    
    # add wheater a article was discounted when ordered
    item_max_price = ave(df[, "item_price"], df[, "item_id"], FUN = function(x) max(x, na.rm = T))
    df$discount    = item_max_price - df$item_price
    
    # add age
    df$age = round(as.integer(df$order_date - df$user_dob)/365)
    
    # clean item_color
    df$item_color = clean_col(df$item_color)
    
    # group age by 5 yrs to facilate WOE, put NAs in own group by decoding as -5 
    df$age = as.character(cut(df$age, breaks = c(0, 20, seq(25, 65, 5), 130)))
    df$age[is.na(df$age)] = "no_age"
    df$age = as.factor(df$age)
    
    # customer levels features waiting for feedback wheater we can even use this feauture
    df$user_order_total = ave(df$return, as.factor(df$user_id), FUN = length)
    
    # item level features
    df$item_order_total = ave(df$return, as.factor(df$item_id), FUN = length)
    
    # cluster item_ids and brand_ids
    time1 = Sys.time()
    df$brand_cat = brand_clust(df)
    # df$brand_cat = as.factor(df$brand_cat) df$brand_id = as.factor(df$brand_id)
    time2 = Sys.time()
    
    df$item_cat = item_clust(df)
    # df$item_cat = as.factor(df$item_cat) df$item_id = as.factor(df$item_id)
    time3 = Sys.time()
    
    # create basket level features
    df$basket_id = paste0(df$order_date, df$user_id)
    df$basket_id = match(df$basket_id, unique(df$basket_id))
    df$basket_size = ave(df$return, as.factor(df$basket_id), FUN = length)
    
    # get multiple occurances on basket level
    feat_list = c("item_size", "item_color", "brand_cat", "item_cat")
    for (feat in feat_list) {
        print(feat)
        df = add_basket_dups(df, feat)
    }
    df$item_color = as.factor(df$item_color)
    df$item_size = as.factor(df$item_size)
    df$brand_id = as.factor(df$brand_id)
    df$brand_cat = as.factor(df$brand_cat)
    df$item_cat = as.factor(df$item_cat)
    df$item_id = as.factor(df$item_id)
    
    return(df)
}