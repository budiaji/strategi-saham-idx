require(methods)
require("quantmod")

print("Hello Friend")

# setup
# TODO: get dir executing script
dir <- getwd()
invisible(Sys.setlocale("LC_MESSAGES", "C"))
invisible(Sys.setlocale("LC_TIME", "C"))

input <- "data/kompas100plus.txt"
df <- read.table(input, sep=" ", header=FALSE, col.names=c("code"))

analysis <- function(code) {

    # transform name to download data from google finance api
    stock_code <- paste("IDX", code, sep=":")
    print(stock_code)

    # get ohlc data
    today <- Sys.Date()
    last_year <- today - 360
    stock_data <- na.approx(getSymbols(stock_code, from=last_year, to=today, auto.assign=FALSE, src="google"))

    # charting
    svg_file = paste("figs/", code, ".svg", sep="")
    svg(svg_file, height=9, width=20)
    chartSeries(stock_data,
                theme="white",
                TA="addBBands(n=10);addVo();addRSI(n=2);
                addSAR();addSMA(n=100, c='blue');addSMA(n=5);
                addATR(5);addMACD();")
    dev.off()

    # temporary data
    temp.close <- Cl(stock_data)
    temp.close_three <- last(Cl(stock_data), 3)
    temp.low_three <- last(Lo(stock_data), 3)
    temp.rsi_two <- RSI(price=temp.close, 2)
    temp.ma_five <- SMA(temp.close, 5)
    temp.ma_hundred <- SMA(temp.close, 100)
    temp.atr_five <- ATR(stock_data, 5)

    # last close data
    result.close <- coredata(last(temp.close))

    # last two period rsi
    result.rsi <- last(temp.rsi_two)

    # last atr five period
    result.atr <- coredata(last(temp.atr_five$atr))

    # percentage atr (volatility)
    result.volatility <- result.atr / result.close

    # buy low next day
    result.buy_low <- result.close - 0.5 * result.atr

    # is close above ma 100
    result.is_above_ma_100 <- result.close > last(temp.ma_hundred)

    # is close below ma 5
    result.is_below_ma_5 <- result.close < last(temp.ma_five)

    # decresing low
    result.decreasing_low <- all(diff(coredata(temp.low_three)) < 0)

    # decresing close
    result.decreasing_close <- all(diff(coredata(temp.close_three)) < 0)

    # is rsi below 10 (oversold, time to buy?)
    result.is_rsi_below <- result.rsi < 10


    # result
    result <- c(
                close=result.close,
                rsi=result.rsi,
                atr=result.atr,
                volatility=result.volatility,
                buy_low=result.buy_low,
                is_above_ma_100=result.is_above_ma_100,
                is_below_ma_5=result.is_below_ma_5,
                decreasing_low=result.decreasing_low,
                decreasing_close=result.decreasing_close,
                is_rsi_below=result.is_rsi_below
                )
    return(result)
}


data <- sapply(df$code, FUN=analysis)

df$close <- data["close",]
df$rsi <- data["rsi",]
df$atr <- data["atr",]
df$volatility <- data["volatility",]
df$buy_low <- data["buy_low",]
df$is_above_ma_100 <- data["is_above_ma_100",]
df$is_below_ma_5 <- data["is_below_ma_5",]
df$decreasing_low <- data["decreasing_low",]
df$decreasing_close <- data["decreasing_close",]
df$is_rsi_below <- data["is_rsi_below",]

# output file
output_file.sore <- "output/analisis.csv"
write.table(df, file=output_file.sore, sep=",", row.names=FALSE)
