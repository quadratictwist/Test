
#################################
### maximum overlap intervals
library(plyr)
library(reshape2)
# prepare test case
df <- data.frame(id=seq(10,80,by=10),
                 anest=c("baker","baker",rep("dow",6)),
                 start=c("08:00","09:00","09:00","08:00","10:00","12:30","13:30","18:00"),
                 end=c("11:00","13:00","15:30","13:30","11:30","13:30","14:30","19:00"))
df$start <- as.character(df$start)
df$end <- as.character(df$end)

## i. overlaps
overlaps <- function(df) {
    findPaste <- function(v) {
        overlap <- df$id[df$start < v["end"] & df$end > v["start"]]
        Reduce(paste, overlap)
    }
    df$w <- apply(df, 1, findPaste)
    df
}
df <- ddply(df, .(anest), overlaps)

## ii. maximum overlaps
# count the number of concurrent tasks when a tast starts
curCount <- function(df) {
    df <- melt(df,id.vars = c("id","anest"), variable.name = "status", value.name = "time")
    df$cur <- 1
    df$cur[df$status == "end"] <- -1
    df <- df[order(df$time, df$cur), ]
    df$cur <- cumsum(df$cur)
    df[df$status == "start", c("id", "cur")]
}
maxOverlap <- function(df) {
    df <- merge(df, curCount(df), by = "id")
    # count max number of concurrent tasks
    df$s <- apply(df, 1, function(v) max(df$cur[df$start < v["end"] & df$start >= v["start"]]))
    df$cur <- NULL
    df
}
# result
df <- ddply(df, .(anest), maxOverlap)


#################################
### Pascal triangle
pascal <- function(n) {
    row <- 1
    print(row)
    for (i in 1:n) {
        row <- c(row,0) + c(0,row)
        print(row)
    }
}
# result
pascal(10)



#################################
### Portfolio VaR CVaR
library(tseries)
library(plyr)

# prepare data
tickers <- c("AAPL","IBM","GOOG","BP","XOM","COST","GS")
startDate <- "2005-01-01" # google ipo 2004
endDate <- "2015-12-31"

df <- list()
for (ticker in tickers) {
    df[[ticker]] <- get.hist.quote(instrument = ticker,
                                   start = startDate, end = endDate,
                                   quote = c("Close"),
                                   provider = "yahoo",
                                   compression = "d")
}
data <- do.call(cbind, df)
names(data) <- tickers
ret <- apply(log(data),2,diff)
weights <- matrix(c(0.15, 0.2, 0.2, 0.15, 0.1, 0.15, 0.05), 7, 1)

## i. VaR CVaR
# both VaR, CVaR are written in terms of return
p <- 0.95
histRet <- ret %*% weights
VaR1 <- quantile(histRet, p = 1 - p) * sqrt(252)
# use riemann sum to approximate CVaR
CVaR1 <- mean(quantile(histRet, p = seq(0, 1 - p, 0.001))) * sqrt(252)

## ii. parametric method
mu <- mean(histRet) * 252
sigma <- sqrt(t(weights) %*% cov(ret) %*% weights * 252)
VaR2 <- qnorm(1 - p, mu, sigma)
# use closed form formula for CVaR
CVaR2 <- -exp(-qnorm(1 - p)^2 / 2) / sqrt(2 * pi) / (1 - p) * sigma + mu

## iii. This part is not clear to me


#################################
### Position calculator
library(plyr)

# read data
path <- "~/Documents/Berkeley/Millennium/test/"
pos <- read.csv(paste0(path, "pos.csv"))
trd <- read.csv(paste0(path, "trd.csv"))

## i. netted position per each user
nettedPos <- ddply(pos, .(user), function(df) sum(df$pos[!is.na(df$pos)]))

## ii. boxed positions
boxPos <- ddply(pos, .(user), function(df) df[duplicated(df$sym) | duplicated(df$sym, fromLast = TRUE), ])

## iii. crossing
# ??? not sure what journal means here

## iv. total quantity to trade
trade <- ddply(trd, .(sym), trd = sum(qty[!is.na(qty)]))

## v. final position
posTot <- ddply(pos, .(user, sym), summarize, tot = sum(pos[!is.na(pos)]))
final <- merge(trd, posTot, by = c("sym","user"), all = TRUE)
final[is.na(final)] <- 0
final$finalPos <- final$qty + final$tot
final

## vi. unit test
# potential problems
# Some user may have a net position of zero, need to consider if such user should be included
# In cases some user's qty is recorded as zero, it should not be counted towards a box position
# If there is NA in the data, it will be carried into the calcuations. Should remove incomplete rows first
pos <- data.frame(user = c("B","B","B"),
                  pb = c("DB","CS","DB"),
                  sym = c(rep("1003.T",3)),
                  pos = c(10,0,NA))
