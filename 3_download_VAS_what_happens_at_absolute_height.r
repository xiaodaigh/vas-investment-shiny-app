# Conclusion:
# this method is sound as it would have shown that GFC was over valued!

library(dplyr)
library(magrittr)

# ZJ: this is needed
source("0_setup_run_ONCE only.r")

quantmod::getSymbols("VAS.AX",from=as.Date("1970-01-01"))

#quantmod::getDividends("VAS.AX",from=as.Date("1970-01-01"))

b1 = nanoparquet::read_parquet("c:/data/axjo.parquet") #%>% 
  #filter(date < "2020-3-09")

a1 = VAS.AX %>% as.data.frame %>% 
  mutate(date = rownames(.) %>% as.Date) %>% 
  inner_join(b1, by = "date")

m = glm(VAS.AX.Close~price, data=a1)

summary(m)
# plot(m)


b2 = b1 %>% 
  mutate(VAS.AX.Close = predict(m, b1)) %>% 
  filter(date < min(a1$date)) %>% 
  select(-price) %>% 
  rename(close = VAS.AX.Close) %>% 
  dplyr::bind_rows(select(a1, date,  close = VAS.AX.Close))


max_date = b2 |> 
  filter(year(date)<2010) |> 
  filter(close == max(close)) |> 
  pull(date)
  

b3 = b2 %>% 
  mutate(id = 1) |> 
  filter(date <= max_date)


b4  = merge(b3, b3, by = "id", allow.cartesian = TRUE) %>% 
  filter(date.x < date.y)

b5 = b4 %>% 
  mutate(years  = as.integer(date.y-date.x)/365.25) %>% 
  mutate(rate = exp(log(close.y/close.x)/years)-1) %>% 
  arrange(desc(years))

b6 = b5 %>% 
  summarise(sum(rate*years)/sum(years)) %T>% 
  print() %>% 
  pull()


b7 = b2 %>% 
  mutate(inc = (1+b6)^(as.integer(date - date[1])/365.25)) %>% 
  mutate(expected = close[1]*inc)
  
b7f = b7 |> 
  filter(date <= max_date)


# add in percentile in title
m = glm(close~expected, data = b7f) 
summary(m)


m.std.err = broom::tidy(m)$std.error[2]

print(summary(m))

b8 = b7 |> 
  mutate(m = predict(m, b7)) |> 
  mutate(pct = 1-close/m)

b8 |> 
  slice(seq(nrow(b7), 1, by=round(-365.25*5/7/12,0)))

b8 |> 
  arrange(pct) |> 
  View()


b8 |> 
  arrange(pct) |> 
  filter(year(date)>2007) |> 
  filter(year(date)<2010) |> 
  View()


covid_pct_quantile = mean(sort(b8$pct) < 0.3000662) |> 
covid_pct_quantile

gfc_pct_quantile = mean(sort(b8$pct) < 0.2444259149)
gfc_pct_quantile



# what's the deviation from close lik -------------------------------------
plot(density(b8$pct))


last_pct = b8 |> last(1) |> pull(pct)

last_pct_quantile = mean(sort(b8$pct) < last_pct)
last_pct_quantile

plot(b7$date, b7$close, type="l", col="blue", 
     main = glue::glue("Actual close vs Expected: %tile today={round(last_pct_quantile*100,0)}% gfc={(gfc_pct_quantile*100) |> round(0)}%; covid={(covid_pct_quantile*100) |> round(0)}%"))
lines(b7$date, b7$expected)
lines(b7$date, predict(m, b7), col="red")

# # find the line that makes all values % above the line --------------------
# pulldown <- function(downby) {
#   pct_above = b8 |> 
#     summarise(mean(close > m - downby)) |> 
#     pull
#   
#   
#   (pct_above - target)^2
# }
# 
# target=0.9
# par90=optim(0, pulldown, method = "Brent", lower = 0, upper=50)$par
# 
# target=0.95
# par95=optim(0, pulldown, method = "Brent", lower = 0, upper=50)$par
# 
# target=0.99
# par99=optim(0, pulldown, method = "Brent", lower = 0, upper=50)$par
# 
# target=0.995
# par995=optim(0, pulldown, method = "Brent", lower = 0, upper=50)$par
# 
# target=0.999
# par999=optim(0, pulldown, method = "Brent", lower = 0, upper=50)$par
# 
# 
# lines(b7$date, predict(m, b7)-par90, col="red", lty=2)
# lines(b7$date, predict(m, b7)-par95, col="red", lty=3)
# lines(b7$date, predict(m, b7)-par99, col="red", lty=4)
# lines(b7$date, predict(m, b7)-par995, col="red", lty=5)
# lines(b7$date, predict(m, b7)-par999, col="red", lty=6)


# 
# 
# b8 = b7 %>% 
#   filter(lubridate::year(date) ==  2020)
# 
# library(data.table)
# setDT(b7)
# 
# 
# d9=slice(b7, seq(1, nrow(b7),  21)) %>% 
#   mutate(money = 1000 * 1.03^(as.integer(date - date[1])/365.25)) %>% 
#   mutate(shares = floor(money/expected)) %>% 
#   mutate(cshares = cumsum(shares)) %>% 
#   mutate(value = close*cshares) %>% 
#   mutate(spent =  cumsum(money))
# 
# meh =  function(x, d, r) {
#   y = x
#   for (i in 2:length(y)) {
#     y[i] = y[i] + y[i-1]*r/(as.integer(d[i]-d[i-1])/365.25)
#   }
#   sum(y)
# }
# 
# t = d9$value[nrow(d9)]
# optim(0.05, function(rate) {
#   py = d9 %>% 
#      summarise(y = meh(money, date, rate)) %>% 
#      pull(y)
#   
#   (py-t)^2
# }, method = "Brent", lower=0, upper=0.1)
  

  
# VAS.AX
# .0
 
# 
# glm(VAS.AX$VAS.AX.Close~I(1:length(VAS.AX$VAS.AX.Close)))
# 
# 0.002872*365
# 
# 
# x = exp(log(68.49/50)/12)
