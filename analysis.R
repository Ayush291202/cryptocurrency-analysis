############################################################
# CRYPTO MARKET ANALYSIS — FULL END-TO-END PROJECT
############################################################

rm(list = ls(all = TRUE))

############################################################
# PACKAGES
############################################################

pkgs <- c("DBI","RSQLite","ggplot2","grid","gtable",
          "corrplot","zoo","magrittr")

for (p in pkgs) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

set.seed(123)

############################################################
# 1. CREATE SQLITE DATABASE
############################################################

con <- dbConnect(SQLite(), "crypto.db")

dbExecute(con, "DROP TABLE IF EXISTS currency")
dbExecute(con, "DROP TABLE IF EXISTS val")

dbExecute(con, "
CREATE TABLE currency (
  slug TEXT,
  name TEXT,
  symbol TEXT
)
")

dbExecute(con, "
CREATE TABLE val (
  currency_slug TEXT,
  datetime DATE,
  price_usd REAL,
  price_btc REAL,
  volume_usd REAL,
  market_cap_usd REAL,
  available_supply REAL
)
")

currencies <- data.frame(
  slug   = c("bitcoin","ethereum","ripple"),
  name   = c("Bitcoin","Ethereum","Ripple"),
  symbol = c("BTC","ETH","XRP")
)

dbWriteTable(con, "currency", currencies, append = TRUE)

dates <- seq(as.Date("2017-01-01"), as.Date("2018-12-31"), by = "day")
n <- length(dates)

generate_crypto <- function(slug, base, vol) {
  ret <- rnorm(n, 0.0008, vol)
  price <- base * cumprod(1 + ret)
  data.frame(
    currency_slug = slug,
    datetime = dates,
    price_usd = price,
    price_btc = price / price[1] * runif(n, 0.02, 0.1),
    volume_usd = runif(n, 1e7, 5e9),
    market_cap_usd = price * runif(n, 1e6, 5e7),
    available_supply = runif(n, 1e6, 1e8)
  )
}

vals <- rbind(
  generate_crypto("bitcoin", 1000, 0.02),
  generate_crypto("ethereum", 100, 0.03),
  generate_crypto("ripple", 0.25, 0.04)
)

dbWriteTable(con, "val", vals, append = TRUE)
dbDisconnect(con)

############################################################
# 2. LOAD & CLEAN DATA
############################################################

con <- dbConnect(SQLite(), "crypto.db")
currencies <- dbGetQuery(con, "SELECT * FROM currency")
vals <- dbGetQuery(con, "SELECT * FROM val")
dbDisconnect(con)

vals$datetime <- as.Date(vals$datetime)

num_cols <- c("price_usd","price_btc","volume_usd",
              "market_cap_usd","available_supply")

vals[num_cols] <- lapply(vals[num_cols], as.numeric)

missing.date.rows <- function(currency, data) {
  d <- sort(unique(data$datetime[data$currency_slug == currency]))
  if (length(d) < 2) return(NULL)
  miss <- setdiff(seq(min(d), max(d), by="day"), d)
  if (!length(miss)) return(NULL)
  data.frame(currency_slug=currency, datetime=miss,
             price_usd=NA, price_btc=NA, volume_usd=NA,
             market_cap_usd=NA, available_supply=NA)
}

interpolate.missing.data <- function(data) {
  new <- do.call(rbind,
                 lapply(unique(data$currency_slug),
                        missing.date.rows, data=data))
  if (!is.null(new)) data <- rbind(data, new)
  data <- data[order(data$currency_slug, data$datetime),]
  for (c in unique(data$currency_slug)) {
    idx <- data$currency_slug == c
    for (col in num_cols)
      data[idx,col] <- zoo::na.approx(data[idx,col],
                                      na.rm=FALSE)
  }
  data
}

vals <- interpolate.missing.data(vals)

############################################################
# 3. MARKET STATISTICS
############################################################

market.data <- function(data) {
  d <- sort(unique(data$datetime))
  cap <- sapply(d, function(x)
    sum(data$market_cap_usd[data$datetime==x], na.rm=TRUE))
  ret <- c(NA, diff(cap)/cap[-length(cap)])
  lret <- c(NA, diff(log(cap)))
  vol30 <- sapply(seq_along(lret),
                  function(i) sd(lret[max(1,i-30):i], na.rm=TRUE))*sqrt(365)
  vol90 <- sapply(seq_along(lret),
                  function(i) sd(lret[max(1,i-90):i], na.rm=TRUE))*sqrt(365)
  hhi <- sapply(d, function(x){
    m <- data$market_cap_usd[data$datetime==x]
    sum((m/sum(m))^2, na.rm=TRUE)
  })
  data.frame(datetime=d, cap=cap, return=ret,
             logreturn=lret, volatility.30d=vol30,
             volatility.90d=vol90, herfindahl=hhi)
}

market <- market.data(vals)

############################################################
# 4. RETURNS & LOGRETURNS
############################################################

vals$return <- ave(vals$price_usd, vals$currency_slug,
                   FUN=function(x) c(NA, diff(x)/x[-length(x)]))
vals$logreturn <- ave(vals$price_usd, vals$currency_slug,
                      FUN=function(x) c(NA, diff(log(x))))

############################################################
# 5. PLOTS — MARKET
############################################################

plot.market <- function(m) {
  g <- rbind(
    ggplotGrob(ggplot(m,aes(datetime,cap))+geom_line()+labs(y="Market Cap")),
    ggplotGrob(ggplot(m,aes(datetime,logreturn))+geom_line()+labs(y="Log return")),
    ggplotGrob(ggplot(m,aes(datetime,volatility.30d))+geom_line()+labs(y="Volatility")),
    ggplotGrob(ggplot(m,aes(datetime,herfindahl))+geom_line()+labs(y="HHI")),
    size="first"
  )
  grid.newpage(); grid.draw(g)
  ggsave("Market-statistics.png", g, width=8, height=6)
}

plot.market(market)

############################################################
# 6. RISK–RETURN ANALYSIS
############################################################

tmp <- aggregate(
  logreturn ~ currency_slug,
  vals,
  function(x) c(
    mean = mean(x, na.rm = TRUE) * 365,
    vol  = sd(x, na.rm = TRUE) * sqrt(365)
  )
)

rr <- data.frame(
  currency   = tmp$currency_slug,
  return     = tmp$logreturn[, "mean"],
  volatility = tmp$logreturn[, "vol"]
)

ggplot(rr, aes(volatility, return, label=currency)) +
  geom_point(size=3) + geom_text(vjust=-0.7) +
  labs(title="Risk–Return Profile")

ggsave("Risk-return.png", width=6, height=5)

############################################################
# 7. ROLLING CORRELATION
############################################################

rolling.corr <- function(x,y,w=60)
  sapply(seq_along(x), function(i)
    if(i<w) NA else cor(x[(i-w+1):i],y[(i-w+1):i],use="complete.obs"))

d1 <- subset(vals, currency_slug=="bitcoin",
             select=c(datetime,logreturn))
d2 <- subset(vals, currency_slug=="ethereum",
             select=c(datetime,logreturn))
d <- merge(d1,d2,by="datetime")
d$corr <- rolling.corr(d$logreturn.x, d$logreturn.y)

ggplot(d,aes(datetime,corr))+geom_line()+
  labs(title="Rolling BTC–ETH Correlation")

ggsave("Rolling-correlation-BTC-ETH.png", width=8, height=4)

############################################################
# 8. CORRELATION MATRIX
############################################################

analysis.return.data <- function(slugs,data){
  d <- subset(data, currency_slug %in% slugs,
              select=c(datetime,currency_slug,logreturn))
  w <- reshape(d, direction="wide",
               idvar="datetime", timevar="currency_slug")
  colnames(w) <- gsub("logreturn.","",colnames(w))
  w
}

png("Corrplot.png",800,700)
corrplot(cor(analysis.return.data(currencies$slug, vals)[,-1],
             use="pairwise.complete.obs"), method="ellipse")
dev.off()

############################################################
# 9. MARKET REGIMES
############################################################

market$regime <- ifelse(
  market$volatility.30d >
    quantile(market$volatility.30d,0.75,na.rm=TRUE),
  "High volatility","Normal")

ggplot(market,aes(datetime,volatility.30d,color=regime))+
  geom_line()+labs(title="Market Volatility Regimes")

ggsave("Market-regimes.png", width=8, height=4)

############################################################
# 10. SAVE OUTPUTS
############################################################

saveRDS(vals,"cleaned_crypto_data.rds")
saveRDS(market,"market_data.rds")
writeLines(capture.output(sessionInfo()),"sessionInfo.txt")

cat("✅ FULL CRYPTO PROJECT COMPLETED SUCCESSFULLY\n")
