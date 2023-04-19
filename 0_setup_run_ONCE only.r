library(quantmod)
library(fst)
library(data.table)
# extend vas data with index data data ------------------------------------
getSymbols("^AXJO",from=as.Date("1970-01-01"))

b1 = as.data.table(AXJO)[!is.na(AXJO.Adjusted),.(date=index, price=AXJO.Adjusted)]

plot(b1)

fst::write_fst(b1,"AXJO.fst")
arrow::write_parquet(b1, "c:/data/axjo.parquet")


a=arrow::read_parquet("c:/data/lol.parquet")

m = glm(result~-1+., data=a, family=binomial)
broom::tidy(m)


a = data.table::fread("c:/data/demo-data/cs-training.csv")

library(dplyr)
data.table::setnames(a, "V1", "Unnamed_1")

data.table::fwrite(
  janitor::clean_names(dplyr::select(a, -c(SeriousDlqin2yrs)), case="none"),
  "c:/data/demo-data/cs-training-scoring.csv")
