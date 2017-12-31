require(methods)
require("quantmod")

# setup
dir <- getwd()
invisible(Sys.setlocale("LC_MESSAGES", "C"))
invisible(Sys.setlocale("LC_TIME", "C"))

input <- "data/kompas100plus.txt"
df <- read.table(input, sep=" ", header=FALSE, col.names=c("code"))

analysis <- function(code) {

    # transform name to download data from google finance api
    stock_code <- paste("IDX", code, sep=":")
    ihsg_code <- paste("IDX", "COMPOSITE", sep=":")
    print(stock_code)

    # get ohlc data
    today <- Sys.Date()
    last_year <- today - 365
    stock_data <- na.approx(getSymbols(stock_code, from=last_year, to=today, auto.assign=FALSE, src="google"))
    ihsg_data <- na.approx(getSymbols(ihsg_code, from=last_year, to=today, auto.assign=FALSE, src="google"))

    # charting
    svg_file = paste("figs/", code, ".svg", sep="")
    svg(svg_file, height=9, width=20)
    chartSeries(stock_data,
                theme="white",
                TA="addBBands();addVo();addMACD();addCMF(n=5);addRSI(n=2);addROC(n=5);
                addSAR();addSMA(n=100, c='blue');addSMA(n=5);
                addZigZag(change=5);")
    dev.off()

    # stock
    stock.close <- coredata(last(Cl(stock_data)))
    stock.daily <- last(dailyReturn(Cl(stock_data)))
    stock.weekly <- last(weeklyReturn(Cl(stock_data)))
    stock.monthly <- last(monthlyReturn(Cl(stock_data)))
    stock.yearly <- last(annualReturn(Cl(stock_data)))

    # ihsg
    ihsg.close <- coredata(last(Cl(ihsg_data)))
    ihsg.daily <- last(dailyReturn(Cl(ihsg_data)))
    ihsg.weekly <- last(weeklyReturn(Cl(ihsg_data)))
    ihsg.monthly <- last(monthlyReturn(Cl(ihsg_data)))
    ihsg.yearly <- last(annualReturn(Cl(ihsg_data)))

    # result
    result <- c(
                stock.close=stock.close,
                stock.daily=stock.daily,
                stock.weekly=stock.weekly,
                stock.monthly=stock.monthly,
                stock.yearly=stock.yearly,
                ihsg.close=ihsg.close,
                ihsg.daily=ihsg.daily,
                ihsg.weekly=ihsg.weekly,
                ihsg.monthly=ihsg.monthly,
                ihsg.yearly=ihsg.yearly
                )
    return(result)
}


data <- sapply(df$code, FUN=analysis)

df$stock.close <- data["stock.close",]
df$stock.daily <- data["stock.daily",]
df$stock.weekly <- data["stock.weekly",]
df$stock.monthly <- data["stock.monthly",]
df$stock.yearly <- data["stock.yearly",]
df$ihsg.close <- data["ihsg.close",]
df$ihsg.daily <- data["ihsg.daily",]
df$ihsg.weekly <- data["ihsg.weekly",]
df$ihsg.monthly <- data["ihsg.monthly",]
df$ihsg.yearly <- data["ihsg.yearly",]

# fixed precision
numbs2format <- df[, (sapply(df, function(x) isTRUE(sum(x %% 1) > 0)))]
other.columns <- df[, (sapply(df, function(x) !isTRUE(sum(x %% 1) > 0)))]
df <- cbind(other.columns, format(numbs2format, digits=3, nsmall=3))

# output file
output_file.sore <- "output/analisis.csv"
write.table(df, file=output_file.sore, sep=",", row.names=FALSE)
