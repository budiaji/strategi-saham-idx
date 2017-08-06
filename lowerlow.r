require(methods)
require("quantmod")
source("dv.r")

# analysis
# 1. semeru
#   measuring mean percentage difference from high today - close yesterday
# 2. penanggungan
#   alternatif harga masuk selain close hari kemarin, misal harga low atau harga moderate hari kemarin
# 

"iFish" <- function(x) {
    cl <- Cl(x)
    xx <- 0.1 * (RSI(cl, 2) - 50)
    #return(xx)
    return((exp(2*xx)-1)/(exp(2*xx)+1))
}

normalizePrice <- function(x) {
    if(x > 0.9999) {
        y <- 0.9999
    } else if (x < -0.9999) {
        y <- -0.9999
    } else {
        y <- x
    }
    return(y)
}

calcFisher <- function(x) {
    length = 10
    #print(Cl(x))
    #print("lag")
    #print(lag(Cl(x)))
    xhl2 <- (Hi(x) + Lo(x)) / 2
    xMaxH <- max(last(xhl2, length))
    xMinL <- min(last(xhl2, length))
    nVal1 <- 2 * ((xhl2 - xMinL) / (xMaxH - xMinL) - 0.5)
    nVal2 <- sapply(nVal1, FUN=normalizePrice)
    nFish <- log((1+nVal2) / (1-nVal2))
    return(nFish)
}

addOpCl <- newTA(OpCl,col='green',type='h') 
addFishInverse <- newTA(iFish, col='red', type='l')

input <- "/home/budiaji/saham/lq45/lq45.txt"

df <- read.table(input, sep=" ", header=FALSE, col.names=c("code"))

graph_svg <- function(code) {
    stock_code <- paste("IDX", code, sep=":")
    print(stock_code)
    svg_file = paste(stock_code, "svg", sep=".")
    invisible(Sys.setlocale("LC_MESSAGES", "C"))
    invisible(Sys.setlocale("LC_TIME", "C"))
    stock_data <- na.approx(getSymbols(stock_code, from=as.Date("2016-05-01"), auto.assign=FALSE, src="google"))
    svg(svg_file, height=9, width=20)
    #layout(matrix(1:8, nrow=4), height=c(4,2.5,4,2.5)) 
    chartSeries(stock_data,
                theme="white",
                #TA="addVo();addOpCl();addFishInverse();addRSI(n=2)")
                TA="addBBands(n=10);addVo();addRSI(n=2);
                addSAR();addSMA(n=100);addSMA(n=5);
                addSMA(n=50, c='blue');addATR(10);addSMI(n=3);addCMO();addMACD();")
    dev.off()


    # analysis
    temp.close <- Cl(stock_data)
    print(tail(stock_data))
    print(tail(Delt(Cl(stock_data), Op(stock_data), k=1)))
    temp.lo <- last(Lo(stock_data), 3)
    temp.cl <- last(Cl(stock_data), 3)
    temp.rsi <- RSI(temp.close, 2)
    temp.ma_five <- SMA(temp.close, 5)
    temp.ma_twenty <- SMA(temp.close, 20)
    temp.ma_fifty <- SMA(temp.close, 50)
    temp.ma_hundred <- SMA(temp.close, 100)
    temp.atr <- ATR(stock_data, 10)
    temp.pick_atr <- sort(coredata(last(temp.atr$atr, 100)))
    temp.above_ma <- temp.close > last(temp.ma_hundred)
    temp.below_ma <- temp.close < last(temp.ma_five)
    result.close <- coredata(last(temp.close))
    result.lo <- coredata(last(temp.lo))
    result.volume <- last(Vo(stock_data))
    result.rsi <- last(temp.rsi)
    result.rsi_yesterday <- first(last(temp.rsi, 2))
    result.five_days_ma <- last(temp.ma_five)
    result.hundred_days_ma <- last(temp.ma_hundred)
    result.atr <- coredata(last(temp.atr$atr))
    result.atr_rank <- findInterval(result.atr, temp.pick_atr)
    result.decreasing <- all(diff(coredata(temp.lo)) < 0)
    result.rsi_below <- result.rsi < 10
    result.fulfill_lowerlow <- all(temp.above_ma, temp.below_ma, result.decreasing)
    result.fulfill_reversion <- all(temp.above_ma, result.rsi < 10)
    result <- c(
                close=result.close,
                volume=result.volume,
                rsi=result.rsi,
                rsi_yesterday=result.rsi_yesterday,
                five_days_ma=result.five_days_ma,
                hundred_days_ma=result.hundred_days_ma,
                atr=result.atr,
                atr_rank=result.atr_rank,
                volat_perc=result.atr / result.close,
                buy_if_below=result.close - result.atr * 0.5,
                buy_limit_reversion=result.lo - result.atr * 0.33,
                decreasing=as.numeric(result.decreasing),
                rsi_below=as.numeric(result.rsi_below),
                fulfill_lowerlow=as.numeric(result.fulfill_lowerlow),
                fulfill_reversion=as.numeric(result.fulfill_reversion)
                )
    return(result)
}

calc_ihsg <- function(code) {
    invisible(Sys.setlocale("LC_MESSAGES", "C"))
    invisible(Sys.setlocale("LC_TIME", "C"))
    ihsg_data <- getSymbols("IDX:COMPOSITE", from=as.Date("2016-01-01"), auto.assign=FALSE, src="google")
    result.ihsg <- Delt(first(Cl(ihsg_data)), last(Cl(ihsg_data)))
    return(result.ihsg)
}

data <- sapply(df$code, FUN=graph_svg)

df$volume <- data["volume",]
df$close <- data["close",]
df$rsi <- data["rsi",]
df$rsi_yesterday <- data["rsi_yesterday",]
df$five_days_ma <- data["five_days_ma",]
df$hundred_days_ma <- data["hundred_days_ma",]
df$atr <- data["atr",]
df$atr_rank <- data["atr_rank",]
df$volat_perc <- data["volat_perc",]
df$buy_if_below <- data["buy_if_below",]
df$buy_limit_reversion <- data["buy_limit_reversion",]
df$decreasing <- data["decreasing",]
df$rsi_below <- data["rsi_below",]
df$fulfill_lowerlow <- data["fulfill_lowerlow",]
df$fulfill_reversion <- data["fulfill_reversion",]
output_file.sore <- "/home/budiaji/saham/lq45/analisis-lowerlow.csv"
write.table(df, file=output_file.sore, sep=",", row.names=FALSE)
