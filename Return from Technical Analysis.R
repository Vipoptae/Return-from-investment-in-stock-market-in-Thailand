############import library################
rm(list = ls())
library(quantmod)

#####  specify startdate,end,forecast times #####
stockname=readline("Enter your Stock name : ")
stockname=sprintf("%s.BK",stockname)
startdate='2014-12-01' ### changeable##
# enddate='2019-12-31' ### changeable##

##### get data #####
stockvar=getSymbols(c(stockname),src = 'yahoo',from=startdate,auto.assign=FALSE) # enddate=last day
## stockvar=getSymbols(c(stockname),src = 'yahoo',from=startdate,to=enddate,auto.assign=FALSE)
# stockvar=to.weekly(stockvar)
data=na.omit(stockvar[,1:4])
cl_data=Cl(data)

############################################################ find signal from rsi ########################


###### condition (changeable) ######

# ovb=60
# ovs=50
rsicheck = 50
#ovb/obs=over bought/over sold 
RSI  = na.omit(merge(data, RSI(Cl(data))))

RSI.signbuy    = RSI$rsi > rsicheck
RSI.signsell   = RSI$rsi < rsicheck


RSI$sign_RSI[diff(RSI.signbuy) == 1  ] = 1 #diff -> prevent signal overlap
RSI$sign_RSI[diff(RSI.signsell) == 1 ] = -1


############ end condition of RSI ########

RSI$sign_RSI = Lag(RSI$sign_RSI) ## becasue we can't immediately buy at close day price(TA come after price end.)
RSI$sign_RSI[1]=0

#################################### find signal from adx ############################# 

###### condition (changeable) ######


ADX  = na.omit(merge(data, ADX((data))))
plus.over.neg = ADX$DIp > ADX$DIn
adx.up        = lag(ADX$ADX) < ADX$ADX
neg.over.plus = ADX$DIp < ADX$DIn
adx.down      = lag(ADX$ADX) > ADX$ADX

ADX$sign_ADX = rep(0,nrow(ADX))
ADX$sign_ADX[diff(adx.up)==1 ] = 1
ADX$sign_ADX[diff(adx.down)==1 ] = -1

############ end condition of ADX ########


ADX$sign_ADX = Lag(ADX$sign_ADX)
ADX$sign_ADX[1]=0

################################################## find signal from stochastic ####################

###### condition (changeable) ######

STO  = na.omit(merge(data, stoch(Cl(data),nFastK = 50)))

sto.up=STO$fastK>0.5
sto.down=STO$fastK<0.5

STO$sign_STO = rep(0,nrow(STO))
STO$sign_STO[diff(sto.up)==1] = 1
STO$sign_STO[diff(sto.down)==1] = -1

############ end condition of STO ########

STO$sign_STO = Lag(STO$sign)
STO$sign_STO[1] = 0

############################################# merge all signals(for buy) #############################


all_buy = merge(RSI$sign_RSI,ADX$sign_ADX) #-> condition (changeable) ###### This code choose buy signal from RSI and ADX ##
all_buy[is.na(all_buy)] = 0
all_buy = cbind(all_buy,rowSums(all_buy))

sign_buy= merge(Cl(data),all_buy)
sign_buy[is.na(sign_buy)] = 0

############ sign_buy  ################   
BUY = all_buy[which(all_buy[,ncol(all_buy)] == ncol(all_buy)-1)] # buy at close price

BUY=na.omit(merge(Cl(data),BUY)) 

########################################### merge all signals(for sell) ######################
all_sell = merge(RSI$sign_RSI)  #-> condition (changeable) ###### This code choose sell signal from RSI  ##
all_sell[is.na(all_sell)] = 0
all_sell = cbind(all_sell,rowSums(all_sell))
sign_sell = merge(Cl(data),all_sell)
sign_sell[is.na(sign_sell)] = 0
################ sign_sell ####################
SELL = all_sell[which(all_sell[,ncol(all_sell)] <=-1)]
SELL=na.omit(merge(Cl(data),SELL))  

#################################### merge buy-sell ##########################
BUY_SELL=merge(BUY,SELL)

###############################################    return    #############################
if(max(sign_buy$rowSums.all_buy) == ncol(all_buy)-1){
  return.before.sum=na.omit((BUY_SELL[,1+ncol(BUY)]-lag(BUY_SELL[,1]))*(100/BUY_SELL[,1+ncol(BUY)]))
  return=sum(na.omit(BUY_SELL[,1+ncol(BUY)]-lag(BUY_SELL[,1]))*(100/BUY_SELL[,1+ncol(BUY)]))
  return}else{
    return.before.sum=0
    return = 0
  }

plot(cumsum(return.before.sum))
return
stockname
return.before.sum

