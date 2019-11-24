##Risk/BETA of portfolio of stock
#install.packages("BatchGetSymbols", repos = "http://cran.us.r-project.org")
library(BatchGetSymbols)
library(ggplot2)

client_risk = Sys.getenv("R_RISK")

#client_risk =0.3
#portfolio
if (client_risk > 0.7) { 
  myportfolio <- c("MJ","IPO", "VGT") #Tech + IPO + Marijuana
} else if (client_risk > 0.5) {
  myportfolio <- c("IPO","VOO","VGT") #Tech + S&P500 + IPO
} else if (client_risk > 0.2) {
  myportfolio <- c("VGT", "VOO") #Tech + S&P500
} else if  (client_risk == 0.1) {
  myportfolio <- c("VOO") #S&P500
} else {
  myportfolio <- c("VOO","IAU") #Gold + S&P500
}  



#date
first.date <- as.Date('2017-06-01')
last.date <- Sys.Date()

#market price of S&P500
ticker.MktIdx <- '^GSPC'

my.tickers <- c( myportfolio,ticker.MktIdx)

l.out <- BatchGetSymbols(tickers = my.tickers, 
                         first.date = first.date,
                         last.date = last.date)


df.stocks <- l.out$df.tickers

#print(l.out$df.control)


#plotting price


p <- ggplot(df.stocks, aes(x=ref.date, y=price.adjusted))
p <- p + geom_line()
p <- p + facet_wrap(~ticker, scales = 'free')
#print(p)


#calculate return
calc.ret <- function(p){
  
  my.n <- length(p)
  arit.ret <- c(NA, log(p[2:my.n]/p[1:(my.n-1)]))
  return(arit.ret)
}

list.ret <- tapply(X = df.stocks$price.adjusted, 
                   INDEX = df.stocks$ticker,
                   FUN = calc.ret)

list.ret <- list.ret[unique(df.stocks$ticker)]

df.stocks$ret <- unlist(list.ret)

df.MktIdx <- df.stocks[df.stocks$ticker==ticker.MktIdx, ]

idx <- match(df.stocks$ref.date, df.MktIdx$ref.date)

df.stocks$ret.MktIdx <- df.MktIdx$ret[idx]


# Check unique tickers
unique.tickers <- unique(df.stocks$ticker)

# create a empty vector to store betas
beta.vec <- c()

for (i.ticker in unique.tickers){
  
  # message to prompt
  #cat('\nRunning ols for',i.ticker)
  
  # filter the data.frame for stock i.ticker
  df.temp <- df.stocks[df.stocks$ticker==i.ticker, ]
  
  # calculate beta with lm
  my.ols <- lm(data = df.temp, formula = ret ~ ret.MktIdx)
  
  # save beta
  my.beta <- coef(my.ols)[2]
  
  # store beta em beta.vec
  beta.vec <- c(beta.vec, my.beta)
}


# print result
#print(data.frame(unique.tickers,beta.vec))
Result_df <- data.frame(unique.tickers,beta.vec)
result_num_row <- nrow(data.frame(Result_df))

nloop <- result_num_row 
sum_Beta <- 0
i <- 1
while (i < nloop) {
  sum_Beta = sum_Beta + Result_df[i,2]
  i = i+1
}
#sum_Beta

target_beta = sum_Beta/(nloop-1)
target_beta

#if (target_beta > 1.5) {
  #print("This portfolio has a lot of risk")
#} else if (target_beta > 1) {
  #print("This portfolio is a little risky")
#} else if  (target_beta == 0) {
  #print("This portfolio is in line with the S&P500")
#} else {
  #print("This portfolio has lower risks than S&P500 but may result in a lower return")
}  

#High_risk <- c("MJ","IPO", "VGT")
#Med_high_risk <- c("IPO","VOO","VGT")
#Med_risk <- c("VGT", "VOO")
#No_risk <- c("VOO")


#if (myportfolio == High_risk) {
    #print("We recommend purchasing Tech, Start ups, and Canabis Related ETFs with tickers VGT,IPO , and MJ") #Tech + IPO + Marijuana
    #print(paste("With a beta of", round(target_beta,3)))
  #} else if (myportfolio == Med_high_risk) {
   #print("We recommend purchasing Tech, S&P500, and Start up Related ETFs with tickers VOO, VGT, and IPO") #Tech + S&P500 + IPO
   #print(paste("With a beta of", round(target_beta,3)))
  #} else if (myportfolio == Med_risk) {
     #print("We recommend purchasing Tech and S&P500 Related ETFs with tickers VOO and VGT") #Tech + S&P500
     #print(paste("With a beta of", round(target_beta,3)))
  #} else if  ( myportfolio == No_risk) {
  #print("We recommend purchasing S&P500 Related ETFs with tickers VOO") #S&P500
  #print(paste("With a beta of", round(target_beta,3)))
#} else {
  #print("We recommend purchasing S&P500 and Gold Related ETFs with tickers VOO and IAU") #Gold + S&P500
  #print(paste("With a beta of", round(target_beta,3)))
#}
