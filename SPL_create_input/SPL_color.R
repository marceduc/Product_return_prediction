library(dplyr)
library(stringr)


# load color dictionary from https://github.com/meodai/color-names
col      = read.csv("colors.csv", stringsAsFactors = F)
col$name = tolower(col$name)

# load full data set to get all raw color names
df = read.csv("full_prepared.csv")


item_color = tolower(unique(df$item_color))
# create lookup data frame with item colors and matching hex values from the dictionary
col_match = data.frame(item_color, hex = col$hex[match(item_color, col$name)], stringsAsFactors = F)

# copy the original item colors to new column
col_match$new_col = col_match$item_color

# clean spelling errors
col_match$new_col[grep("blau", col_match$item_color)]     = "blue"
col_match$new_col[grep("darkblue", col_match$item_color)] = "blue"
col_match$new_col[grep("brwon", col_match$item_color)]    = "brown"
col_match$new_col[grepl("oliv", col_match$new_col)]       = "olive"
col_match$new_col                                         = str_replace_all(col_match$new_col, ".* ", "")
col_match$new_col                                         = str_replace_all(col_match$new_col, "mocca", "mocha")
col_match$hex2                                            = col$hex[match(col_match$new_col, col$name)]

# get frequency of orginal item colors
counts = data.frame(table(tolower(df$item_color)), stringsAsFactors = F)
names(counts)[1] = "item_color"

# create new df based on color_match and frequencies
new = inner_join(col_match, counts)
# add rgb columns
new = cbind(new, t(col2rgb(new$hex2)))
min_count

# assigned colors without hex value and frequency smaller than 100 to a seperate class
# called other
new$new_col[which(is.na(new$hex2) & new$Freq < 100)] = "other"
# get idexes of this colors
too_less = which(!is.na(new$hex2) & new$Freq < 100)
# create distance matrix based on rgb values
d = as.matrix(dist(new[, 6:8]))

# copy the new_col column to update it
new$col3 = new$new_col

# assigne colors with low frequencies to new color group
for (idx in too_less) {
    print(new$col3[idx])
    print(new$col3[which.min(d[idx, -idx])])
    new$col3[idx] = new$col3[which.min(d[idx, -idx])]
}

write.csv(new, 'clean_col.csv')

