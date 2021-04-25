# Create pv
pv <- 100
# Create r
r <- 0.1
# Calculate fv1
fv1 <- pv * (1 + r)
# Calculate fv2
fv2 <- pv *(1+r) ** 2
# Calculate pv1
pv1 <- fv1 / (1 + r)
# Calculate pv2
pv2 <- fv2/ (1+r) ** 2
# Print pv1 and pv2
print(pv1)
print(pv2)
# Create vector of cash flows
cf <- c(5,5,5,5,105)
# Convert to data frame
cf <- data.frame(cf)
cf
# Add column t to cf
cf$t <- as.numeric(rownames(cf))
# Calculate pv_factor
cf$pv_factor <- 1 / (1 + 0.06)^cf$t
# Calculate pv
cf$pv <-  cf$cf * cf$pv_factor
# Calculate the bond price
sum(cf$pv)
# Create function
bondprc <- function(p, r, ttm, y) {
cf <- c(rep(p * r, ttm - 1), p * (1 + r))
cf <- data.frame(cf)
cf$t <- as.numeric(rownames(cf))
cf$pv_factor <- 1 / (1 + y)^cf$t
cf$pv <- cf$cf * cf$pv_factor
sum(cf$pv)
}
# Verify prior result
bondprc(100, 0.05, 5, 0.06)
# Load Quandl package
library(Quandl)
# Obtain Moody's Baa index data
baa <- Quandl("FED/RIMLPBAAR_N_M")
# Identify 9/30/16 yield
baa_yield <- subset(baa, baa$Date == "2016-09-30")
# Convert yield to decimals and view
baa_yield <- baa_yield$Value/ 100
baa_yield
# Value bond
bondprc(p = 100, r = 0.05, ttm = 5, y = 0.0429)
# Convert prc_yld to data frame
prc_yld <- data.frame(prc_yld)
# Calculate bond price given different yields
for (i in 1:nrow(prc_yld)) {
prc_yld$price[i] <- bondprc(100, 0.10, 20, prc_yld$prc_yld[i])
}
# Plot P/YTM relationship
plot(prc_yld, prc_yld$price,
type = "l",
col = "blue",
main = "Price/YTM Relationship")
# Load quantmod package
library('quantmod')
# Obtain Treasury yield data
t10yr <- getSymbols(Symbols = "DGS10", src = "FRED", auto.assign = FALSE)
# Subset data
t10yr <-  t10yr["2006-01-01/2016-09-30"]
# Plot yields
plot(x = index(t10yr),
y = t10yr$DGS10,
xlab = "Date",
ylab = "Yield (%)",
type = "l",
col = "red",
main = "10-Year US Treasury Yields")
# Generate AAA grade bond
aaa <- Quandl("FED/RIMLPAAAR_N_M")
# Examine first and last six elements in spread
spread <- merge(aaa, baa, by="Date")
aaa <- aaa %>%
filter(Date >= "2006-01-01")
baa <- baa %>%
filter(Date >= "2006-01-01")
colnames(spread) <- c("date", "aaa", "baa")
head(spread,6)
tail(spread,6)
# Calculate spread$diff
spread$diff <- (spread$baa - spread$aaa) * 100
# Plot spread
plot(x = spread$date,
y = spread$diff,
type = "l",
xlab = "Date",
ylab = "Spread (bps)",
col = "red",
main = "Baa - Aaa Spread")
# Value bond using 5% yield
bondprc(p = 100, r = 0.05, ttm = 5, y = 0.05)
# Value bond using 7% yield
bondprc(p = 100, r = 0.05, ttm = 5, y = 0.07)
# Value bond using 6% yield
bondprc(p = 100, r = 0.05, ttm = 5, y = 0.06)
# Create cash flow vector
cf <- c(-95.79, 5, 5, 5, 5, 105)
# Create bond valuation function
bval <- function(i, cf,
t=seq(along = cf))
sum(cf / (1 + i)^t)
# Create ytm() function using uniroot
ytm <- function(cf) {
uniroot(bval, c(0, 1), cf = cf)$root
}
# Use ytm() function to find yield
ytm(cf)
# Calculate the PV01
abs(bondprc(100, 0.10, 20, 0.1001)
- bondprc(100, 0.10, 20, 0.10))
# Calculate bond price today
px <- bondprc(p = 100, r = 0.10, ttm = 20, y = 0.10)
px
# Calculate bond price if yields increase by 1%
px_up <- bondprc(p = 100, r = 0.10, ttm = 20, y = 0.11)
px_up
# Calculate bond price if yields decrease by 1%
px_down <- bondprc(p = 100, r = 0.10, ttm = 20, y = 0.09)
px_down
# Calculate approximate duration
duration <- (px_down - px_up) / (2 * px * 0.01)
duration
# Estimate percentage change
duration_pct_change <- -duration * - 0.01
duration_pct_change
# Estimate dollar change
duration_dollar_change <- duration_pct_change * px
duration_dollar_change
# Calculate approximate convexity
convexity <- (px_up + px_down - 2 * px) / (px * (0.01)^2)
convexity
# Estimate percentage change
convexity_pct_change <- 0.5 * convexity * (0.01)^2
convexity_pct_change
# Estimate dollar change
convexity_dollar_change <- convexity_pct_change * 100
convexity_dollar_change
# Estimate change in price
price_change <- duration_dollar_change + convexity_dollar_change
# Estimate price
# Create pv
pv <- 100
# Create r
r <- 0.1
# Calculate fv1
fv1 <- pv * (1 + r)
# Calculate fv2
fv2 <- pv *(1+r) ** 2
# Calculate pv1
pv1 <- fv1 / (1 + r)
# Calculate pv2
pv2 <- fv2/ (1+r) ** 2
# Print pv1 and pv2
print(pv1)
print(pv2)
# Create vector of cash flows
cf <- c(5,5,5,5,105)
# Convert to data frame
cf <- data.frame(cf)
cf
# Add column t to cf
cf$t <- as.numeric(rownames(cf))
# Calculate pv_factor
cf$pv_factor <- 1 / (1 + 0.06)^cf$t
# Calculate pv
cf$pv <-  cf$cf * cf$pv_factor
# Calculate the bond price
sum(cf$pv)
# Create function
bondprc <- function(p, r, ttm, y) {
  cf <- c(rep(p * r, ttm - 1), p * (1 + r))
  cf <- data.frame(cf)
  cf$t <- as.numeric(rownames(cf))
  cf$pv_factor <- 1 / (1 + y)^cf$t
  cf$pv <- cf$cf * cf$pv_factor
  sum(cf$pv)
}
# Verify prior result
bondprc(100, 0.05, 5, 0.06)
# Load Quandl package
library(Quandl)
# Obtain Moody's Baa index data
baa <- Quandl("FED/RIMLPBAAR_N_M")
# Identify 9/30/16 yield
baa_yield <- subset(baa, baa$Date == "2016-09-30")
# Convert yield to decimals and view
baa_yield <- baa_yield$Value/ 100
baa_yield
# Value bond
bondprc(p = 100, r = 0.05, ttm = 5, y = 0.0429)
# Convert prc_yld to data frame
prc_yld <- data.frame(prc_yld)
# Calculate bond price given different yields
for (i in 1:nrow(prc_yld)) {
  prc_yld$price[i] <- bondprc(100, 0.10, 20, prc_yld$prc_yld[i])
}
# Plot P/YTM relationship
plot(prc_yld, prc_yld$price,
     type = "l",
     col = "blue",
     main = "Price/YTM Relationship")
# Load quantmod package
library('quantmod')
# Obtain Treasury yield data
t10yr <- getSymbols(Symbols = "DGS10", src = "FRED", auto.assign = FALSE)
# Subset data
t10yr <-  t10yr["2006-01-01/2016-09-30"]
# Plot yields
plot(x = index(t10yr),
     y = t10yr$DGS10,
     xlab = "Date",
     ylab = "Yield (%)",
     type = "l",
     col = "red",
     main = "10-Year US Treasury Yields")
# Generate AAA grade bond
aaa <- Quandl("FED/RIMLPAAAR_N_M")
# Examine first and last six elements in spread
spread <- merge(aaa, baa, by="Date")
aaa <- aaa %>%
  filter(Date >= "2006-01-01")
baa <- baa %>%
  filter(Date >= "2006-01-01")
colnames(spread) <- c("date", "aaa", "baa")
head(spread,6)
tail(spread,6)
# Calculate spread$diff
spread$diff <- (spread$baa - spread$aaa) * 100
# Plot spread
plot(x = spread$date,
     y = spread$diff,
     type = "l",
     xlab = "Date",
     ylab = "Spread (bps)",
     col = "red",
     main = "Baa - Aaa Spread")
# Value bond using 5% yield
bondprc(p = 100, r = 0.05, ttm = 5, y = 0.05)
# Value bond using 7% yield
bondprc(p = 100, r = 0.05, ttm = 5, y = 0.07)
# Value bond using 6% yield
bondprc(p = 100, r = 0.05, ttm = 5, y = 0.06)
# Create cash flow vector
cf <- c(-95.79, 5, 5, 5, 5, 105)
# Create bond valuation function
bval <- function(i, cf,
                 t=seq(along = cf))
  sum(cf / (1 + i)^t)
# Create ytm() function using uniroot
ytm <- function(cf) {
  uniroot(bval, c(0, 1), cf = cf)$root
}
# Use ytm() function to find yield
ytm(cf)
# Calculate the PV01
abs(bondprc(100, 0.10, 20, 0.1001)
    - bondprc(100, 0.10, 20, 0.10))
# Calculate bond price today
px <- bondprc(p = 100, r = 0.10, ttm = 20, y = 0.10)
px
# Calculate bond price if yields increase by 1%
px_up <- bondprc(p = 100, r = 0.10, ttm = 20, y = 0.11)
px_up
# Calculate bond price if yields decrease by 1%
px_down <- bondprc(p = 100, r = 0.10, ttm = 20, y = 0.09)
px_down
# Calculate approximate duration
duration <- (px_down - px_up) / (2 * px * 0.01)
duration
# Estimate percentage change
duration_pct_change <- -duration * - 0.01
duration_pct_change
# Estimate dollar change
duration_dollar_change <- duration_pct_change * px
duration_dollar_change
# Calculate approximate convexity
convexity <- (px_up + px_down - 2 * px) / (px * (0.01)^2)
convexity
# Estimate percentage change
convexity_pct_change <- 0.5 * convexity * (0.01)^2
convexity_pct_change
# Estimate dollar change
convexity_dollar_change <- convexity_pct_change * 100
convexity_dollar_change
# Estimate change in price
price_change <- duration_dollar_change + convexity_dollar_change
# Estimate price
price <- px + duration_dollar_change + convexity_dollar_change
