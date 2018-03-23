
library(yamldebugger)
workdir = "/Users/deepthought42/Documents/Studium/SPL/Product_return_prediction"

d_init = yaml.debugger.init(workdir, show_keywords = TRUE)

qnames = yaml.debugger.get.qnames(d_init$RootPath)

d_results = yaml.debugger.run(qnames, d_init)

OverView = yaml.debugger.summary(qnames, d_results, summaryType = "mini")


#### Format R Code
files = list.files()
files
for(file in files){
    tidy_source(source = file, file = paste0(substr(file, 1,nchar(file)-2), "_formated.R"))
}


tidy_source(source = "create_input_df.R", file = "create_input_df_formated.R")