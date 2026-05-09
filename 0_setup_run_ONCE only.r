library(quantmod)
library(fst)
library(data.table)
# extend vas data with index data data ------------------------------------
# this is the asx 200 and the VAS tracks vas 300
getSymbols("^AXJO",from=as.Date("1970-01-01"))

#getSymbols("^AORD",from=as.Date("1970-01-01"))

b1 = as.data.table(AXJO)[!is.na(AXJO.Adjusted),.(date=index, price=AXJO.Adjusted)]

plot(b1)

fst::write_fst(b1,"AXJO.fst")
nanoparquet::write_parquet(b1, "c:/data/axjo.parquet")
