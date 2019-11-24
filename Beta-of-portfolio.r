##Risk/BETA of portfolio of stock
#install.packages("BatchGetSymbols", repos = "http://cran.us.r-project.org")
library(BatchGetSymbols)


#portfolio
myportfolio <- c("TSLA","AAPL","MC")

#date
first.date <- as.Date('2015-01-01')
last.date <- as.Date('2017-01-01')

#market price of S&P500
ticker.MktIdx <- '^GSPC'

my.tickers <- c( myportfolio,ticker.MktIdx)

l.out <- BatchGetSymbols(tickers = my.tickers, 
                         first.date = first.date,
                         last.date = last.date)


df.stocks <- l.out$df.tickers

#print(l.out$df.control)


#plotting price
library(ggplot2)

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
sum_Beta

target_beta = sum_Beta/(nloop-1)
target_beta

if (target_beta > 1.5) { 
  print("This portfolio has a lot of risk")
} else if (target_beta > 1) {
  print("This portfolio is a little risky")
} else if  (target_beta == 0) {
  print("This portfolio is in line with the S&P500")
} else {
  print("This portfolio has lower risks than S&P500 but may result in a lower return")
}  


