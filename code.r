

# cross tab to discover long term growth rate ---------------------------------------------------------------
a1[,id:=1]
act=merge(a1,a1,by="id",allow.cartesian = T)
act1=act[date.y>date.x,]
act1[,aret:=exp(log(price.y/price.x)/(as.integer(date.y-date.x)/365.25))-1]
act1[,since:=as.integer(date.y-date.x)]
act2 = act1[,.(mean(aret), sd(aret)),since]

plot(act2[order(since),.(since, V1)],ylim=c(0,0.1),type="l")
lines(act2[order(since),.(since, V1+V2*1.96)],ylim=c(0,0.1),type="l")
lines(act2[order(since),.(since, V1-V2*1.96)],ylim=c(0,0.1),type="l")
abline(h=act2[order(since),][.N,V1],col="red",lty=2)
abline(h=act2[order(since),][.N,4.8/100],col="red",lty=2)





# calculation code --------------------------------------------------------


# price plot ---------------------------------------------
plot(bb[, .(date,targetprice)],type='l',col="blue")
lines(bb[, .(date, pricenow)],type='l',col="black")

lines(bb[,.(date,(targetprice/pricenow-1)*30+40)],type="l",col="red",lty=2)
abline(h=40,col="red")


# price 1 yr from now as well ---------------------------------------------


bb1 = bb[,.(date=date-365,price1yr=pricenow)]
bb2 = merge(bb,bb1,by="date",all.x=T)
setDT(bb2)

plot(bb2[, .(date,targetprice)],type='l',col="blue")
lines(bb2[, .(date, pricenow)],type='l',col="black")
lines(bb2[, .(date, price1yr)],type='l',col="green")


plot(bb[targetprice<5, .(date+365,targetprice %>% log)],type='l',col="blue")
lines(bb[, .(date,price %>% log)],type='l',col="black")



bb2[, buy:=targetprice>pricenow]
bb2[,`:=`(right=price1yr>pricenow)]
bb2[,buyright:=buy & right]
bb2[,buywrong:=buy & !right]
bb2[,.(buy=sum(buy), right=sum(right),buyright=sum(buyright),buywrong=sum(buywrong))]

bb2[buy==F,.(.N, mean(pricenow-price1yr), mean(pricenow-targetprice)),
    .(year(date), month(date))]
write.csv(bb2,"bb2.csv")