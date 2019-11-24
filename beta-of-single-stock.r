
##Risk/BETA of one stock
rm(list = ls(all.names = TRUE))
#install.packages("BatchGetSymbols", repos = "http://cran.us.r-project.org")
library(BatchGetSymbols)

#Interested stock
myportfolio=Sys.getenv("R_STOCK")
#myportfolio <- "AAPL"

#dates of porfolio
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

result.df <- data.frame(unique.tickers,beta.vec)
target_beta <- result.df[1,2]
target_beta

"""if (target_beta > 1.5) {
  print("This stock has a lot of risk")
} else if (target_beta > 1) {
  print("This stock is a little risky")
} else if  (target_beta == 0) {
  print("This stock is in line with the S&P500")
} else {
  print("This stock has lower risks than S&P500 but may result in a lower return")
}  """
