# Install the 'demography' package from CRAN 
#install.packages('demography', dependencies = TRUE)

# Or the development version from GitHub 
# The devtools package is required to be installed and loaded first
#install.packages("devtools")
#library(devtools)
#install_github("robjhyndman/demography")

library(demography)
# To create awesome tables in HTML format
#install.packages("kableExtra")
library(kableExtra)


# Import data from Human Mortality Database ####
# Use your credentials to get the data
gb_mx <- hmd.mx("GBR_NP", "username", "password", "Great Britain")

fr_mx <- hmd.mx("FRATNP", "username", "password", "France")

# These are objects of the class "demogdata" (lists)
# with the following structure
str(gb_mx)
str(fr_mx)


# Plot log death rates by age ####
par(mfrow = c(1,2), mar = c(6,4.2,4,1))
plot(gb_mx, series=names(gb_mx$rate)[3], year=1922:2018,
     main="Great Britain",cex.main=1)
plot(fr_mx, series=names(fr_mx$rate)[3], year=1922:2018,
     main="France",cex.main=1)



# Plot log death rates by year (sex: total) ####
par(mfrow = c(1,2), mar = c(6,4.2,4,1))
plot(gb_mx, series=names(gb_mx$rate)[3], year=1922:2018,
     plot.type=c("time"),xlab="Year", ylim=c(-10,2.5),
     main="Great Britain",cex.main=1)
plot(fr_mx, series=names(fr_mx$rate)[3], year=1922:2018,
     plot.type=c("time"),xlab="Year", ylim=c(-10,2.5),
     main="France",cex.main=1)


# Import e0 time series data from HMD ####
gb_e0 <- hmd.e0("GBR_NP", "username", "password")

fr_e0 <- hmd.e0("FRATNP", "username", "password")

# Objects of the class "mts", "ts", "matrix"
gb_e0
str(gb_e0)
class(gb_e0)


# Plot time series (for France subset values form 1922 to 2018)
plot(gb_e0, xlab="Year",
     main = "Great Britain - Life expectancy at birth series 1922-2018")
plot(window(fr_e0, start = 1922, end = 2018), xlab="Year",
     main = "France - Life expectancy at birth series 1922-2018")


# Life tables 1922-2018 (sex: total) ####
gb_lt <- lifetable(gb_mx, series = names(gb_mx$rate)[3], years = gb_mx$year,
                   ages = gb_mx$age, max.age = min(110, max(gb_mx$age)),
                   type = c("period"))

fr_lt <- lifetable(fr_mx, series = names(fr_mx$rate)[3], years = 1922:2018,
                   ages = fr_mx$age, max.age = min(110, max(fr_mx$age)),
                   type = c("period"))


# Plot life expectancy curves by age (sex: total) ####
par(mfrow = c(1,2), mar = c(6,4.2,4,1))
plot(gb_lt, main = "Great Britain", cex.main = 1, ylab = "ex")
plot(fr_lt, main = "France", cex.main = 1, ylab = "ex")



# Life tables 2018 (sex: total) ####
gb_lt2018 <- lifetable(gb_mx, series = names(gb_mx$rate)[3], years = 2018,
                       ages = gb_mx$age, max.age = min(110, max(gb_mx$age)),
                       type = c("period"))

gb_lt2018

fr_lt2018 <- lifetable(fr_mx, series = names(fr_mx$rate)[3], years = 2018,
                       ages = fr_mx$age, max.age = min(110, max(fr_mx$age)),
                       type = c("period"))

fr_lt2018



# Life tables 1922 (sex: total) ####
gb_lt1922 <- lifetable(gb_mx, series = names(gb_mx$rate)[3], years = 1922,
                       ages = gb_mx$age, max.age = min(110, max(gb_mx$age)),
                       type = c("period"))

fr_lt1922 <- lifetable(fr_mx, series = names(fr_mx$rate)[3], years = 1922,
                       ages = fr_mx$age, max.age = min(110, max(fr_mx$age)),
                       type = c("period"))



# Calculate Index of Mortality Entropy (Keyfitz H-index) - Great Britain ####
gb_H <- rep(1,97)
gb_year <- seq(1922,2018,1)

for (index in 1:97) {
  
  gb_ltyear <- lifetable(gb_mx, series = names(gb_mx$rate)[3],
                         years = gb_mx$year[index], ages =gb_mx$age,
                         max.age = min(100, max(gb_mx$age)),
                         type = c("period"))
  
  gb_lx_year <- gb_ltyear$lx
  
  gb_ln_lx_year <- log(gb_lx_year)
  gb_ln_lx_year
  
  gb_product_year <- gb_ln_lx_year * gb_lx_year
  gb_product_year
  
  gb_sum_product_year <- sum(gb_product_year)
  gb_sum_product_year
  
  gb_sum_dem_year <- sum(gb_lx_year)
  gb_sum_dem_year
  
  gb_H_year <- -(gb_sum_product_year / gb_sum_dem_year)
  gb_H_year
  gb_H[index] = gb_H_year
}

gb_Hseries <- cbind(Year=gb_year, H_index=gb_H)



# Calculate Index of Mortality Entropy (Keyfitz H-index) - France ####
fr_H <- rep(1,203)
fr_year <- seq(1816,2018,1)

for (index in 1:203) {
  
  fr_ltyear <- lifetable(fr_mx, series = names(fr_mx$rate)[3],
                         years = fr_mx$year[index], ages=fr_mx$age,
                         max.age = min(100, max(fr_mx$age)),
                         type = c("period"))
  
  fr_lx_year <- fr_ltyear$lx
  
  fr_ln_lx_year <- log(fr_lx_year)
  fr_ln_lx_year
  
  fr_product_year <- fr_ln_lx_year * fr_lx_year
  fr_product_year
  
  fr_sum_product_year <- sum(fr_product_year)
  fr_sum_product_year
  
  fr_sum_dem_year <- sum(fr_lx_year)
  fr_sum_dem_year
  
  fr_H_year <- -(fr_sum_product_year / fr_sum_dem_year)
  fr_H_year
  fr_H[index] = fr_H_year
}


# Subset only values from 1922 to 2018
fr_Hseries <- cbind(Year=fr_year[107:203], H_index=fr_H[107:203])



# Plot lx curves and H index series #####
par(mfrow = c(1,2), mar = c(6,4.2,4,1))
plot(gb_lt1922$lx, col="blue", xlab="Age", ylab="lx", type="l",
     lty=2,main = "Survival Curves")
lines(fr_lt1922$lx, col="red", lty=2)
lines(gb_lt2018$lx, col="blue", lty=1)
lines(fr_lt2018$lx, col="red", lty=1)

legend("bottomleft",c("GB 1922", "FR 1922", "GB 2018", "FR 2018"),
       col=c("blue", "red","blue", "red"),
       lty=c(2,2,1,1), cex = 0.5)


plot(gb_Hseries[,1], gb_Hseries[,2], type = "l", xlab="Year", ylab="H index",
     col="blue", ylim = c(0.1,0.5),main="Index of Mortality Entropy")
lines(x=fr_Hseries[,1], y=fr_Hseries[,2], col="red")
legend("topright" , c("Great Britain", "France"),cex=0.5,
       col=c("blue","red"),lty=1)


# Plot 3D lx Curves by age and year ####
par(mfrow = c(1,2), mar = c(2,1,5,1))

persp(gb_lt$age, gb_lt$year, gb_lt$lx, theta = 40, xlab = "age",
      ylab = "year", zlab = "lx", main = "Great Britain")

persp(fr_lt$age, fr_lt$year, fr_lt$lx, theta = 40, xlab = "age",
      ylab = "year", zlab = "lx", main = "France")



# Plot 3D dx Curves by age and year ####
par(mfrow = c(1,2), mar = c(2,1,5,1))

persp(gb_lt$age, gb_lt$year, gb_lt$dx, theta = 40, xlab = "age",
      ylab = "year", zlab = "lx", main = "Great Britain")

persp(fr_lt$age, fr_lt$year, fr_lt$dx, theta = 40, xlab = "age",
      ylab = "year", zlab = "lx", main = "France")


# Lee-Carter Models - Great Britain ####
gb_LCt <- lca(gb_mx, series=names(gb_mx$rate)[3],
              years=1922:2018, interpolate = TRUE)

gb_LCf <- lca(gb_mx, series=names(gb_mx$rate)[1],
              years=1922:2018,interpolate = TRUE)

gb_LCm <- lca(gb_mx, series=names(gb_mx$rate)[2],
              years=1922:2018, interpolate = TRUE)



# Model results and plots - Great Britain ####
summary(gb_LCt)

par(mfrow = c(2,3), mar = c(4,4,2,2))
plot(gb_LCt$ax, main="ax", xlab="Age",ylab="ax",type="l")
lines(x=gb_LCf$age, y=gb_LCf$ax, main="ax", col="red")
lines(x=gb_LCm$age, y=gb_LCm$ax, main="ax", col="blue")
legend("topleft" , c("Total","Female","Male"),cex=0.7,
       col=c("black","red","blue"),lty=1)

plot(gb_LCt$bx, main="bx", xlab="Age",ylab="bx",type="l")
lines(x=gb_LCf$age, y=gb_LCf$bx, main="bx", col="red")
lines(x=gb_LCm$age, y=gb_LCm$bx, main="bx", col="blue")
legend("topright" , c("Total","Female","Male"),cex=0.7,
       col=c("black","red","blue"),lty=1)

plot(gb_LCt$kt, main="kt", xlab="Year",ylab="kt",type="l")
lines(x=gb_LCf$year, y=gb_LCf$kt, main="kt", col="red")
lines(x=gb_LCm$year, y=gb_LCm$kt, main="kt", col="blue")
legend("topright" , c("Total","Female","Male"),cex=0.6,
       col=c("black","red","blue"),lty=1)

plot(forecast(gb_LCt, h = 32)$kt, main="Forecast form ARIMA(0,1,0)",
     xlab = "Year", ylab = "kt")

gb_LCt_res=residuals(gb_LCt, "residuals")

plot(rep(gb_LCt_res$y,length(gb_LCt_res$x)),(gb_LCt_res$z), xlab="Age",
     ylab="residuals",main="Residuals by ages")


plot(rep(gb_LCt_res$x,length(gb_LCt_res$y)),(gb_LCt_res$z), xlab="Year",
     ylab="residuals",main="Residuals by years")




# Comparison actual and forecast values - Great Britain (sex: total) ####
par(mfrow = c(2,2), mar = c(4,4,2,1))
plot(gb_mx, series=names(gb_mx$rate)[3], year=1922:2018,
     main="Actual (1922-2018)", cex.main=1)
plot(forecast(gb_LCt, h = 32), main="Forecast (2019-2050)", cex.main=1)

plot(gb_lt, main = "Actual (1922-2018)", cex.main = 1,
     ylab = "ex")
plot(lifetable(forecast(gb_LCt, h = 32)),ylab = "ex",
     main="Forecast (2019-2050)", cex.main=1)




# Lee-Carter Models - France ####
fr_LCt <- lca(fr_mx, series=names(fr_mx$rate)[3],
              years=1922:2018, interpolate = TRUE)

fr_LCf <- lca(fr_mx, series=names(fr_mx$rate)[1],
              years=1922:2018,interpolate = TRUE)

fr_LCm <- lca(fr_mx, series=names(fr_mx$rate)[2],
              years=1922:2018, interpolate = TRUE)



# Model results and plots - France ####
summary(fr_LCt)

par(mfrow = c(2,3), mar = c(4,4,2,2))
plot(fr_LCt$ax, main="ax", xlab="Age",ylab="ax",type="l")
lines(x=fr_LCf$age, y=fr_LCf$ax, main="ax", col="red")
lines(x=fr_LCm$age, y=fr_LCm$ax, main="ax", col="blue")
legend("topleft" , c("Total","Female","Male"),cex=0.7,
       col=c("black","red","blue"),lty=1)

plot(fr_LCt$bx, main="bx", xlab="Age",ylab="bx",type="l")
lines(x=fr_LCf$age, y=fr_LCf$bx, main="bx", col="red")
lines(x=fr_LCm$age, y=fr_LCm$bx, main="bx", col="blue")
legend("topright" , c("Total","Female","Male"),cex=0.7,
       col=c("black","red","blue"),lty=1)

plot(fr_LCt$kt, main="kt", xlab="Year",ylab="kt",type="l")
lines(x=fr_LCf$year, y=fr_LCf$kt, main="kt", col="red")
lines(x=fr_LCm$year, y=fr_LCm$kt, main="kt", col="blue")
legend("topright" , c("Total","Female","Male"),cex=0.6,
       col=c("black","red","blue"),lty=1)

plot(forecast(fr_LCt, h = 32)$kt, main="Forecast form ARIMA(0,1,0)",
     xlab = "Year", ylab = "kt")

fr_LCt_res=residuals(fr_LCt, "residuals")

plot(rep(fr_LCt_res$y,length(fr_LCt_res$x)),(fr_LCt_res$z), xlab="Age",
     ylab="residuals",main="Residuals by ages")


plot(rep(fr_LCt_res$x,length(fr_LCt_res$y)),(fr_LCt_res$z), xlab="Year",
     ylab="residuals",main="Residuals by years")


# Comparison actual values and forecast - France (sex: total) ####
par(mfrow = c(2,2), mar = c(4,4,2,1))
plot(fr_mx, series=names(fr_mx$rate)[3], year=1922:2018,
     main="Actual (1922-2018)", cex.main=1)
plot(forecast(fr_LCt, h = 32), main="Forecast (2019-2050)", cex.main=1)

plot(fr_lt, main = "Actual (1922-2018)", cex.main = 1,
     ylab = "ex")
plot(lifetable(forecast(fr_LCt, h = 32)),ylab = "ex",
     main="Forecast (2019-2050)", cex.main=1)



# e0 time series (sex: total): joining past and forecast values ####
dev.off() #clean plot viewer

gb_e0t <- life.expectancy(gb_mx, series=names(gb_mx$rate)[3],
                          years = gb_mx$year, type = c("period"))

gb_e0LCt <- life.expectancy(forecast(gb_LCt, h=32),type = c("period"))

gb_e0join <- c(gb_e0t, gb_e0LCt)



fr_e0t <- life.expectancy(fr_mx, series=names(fr_mx$rate)[3],
                          years = 1922:2018, type = c("period"))

fr_e0LCt <- life.expectancy(forecast(fr_LCt, h=32),type = c("period"))

fr_e0join <- c(fr_e0t, fr_e0LCt)


e0LC_years <- seq(1922, 2050, 1)

plot(x=e0LC_years, y=gb_e0join,type = "l",ylim = c(45,90),
     main ="Life expectancies at birth (1922-2050)",xlab = "Years",ylab = "e0")
lines(x=e0LC_years, y=fr_e0join, col="blue")
abline(v=2019, col="red")
text(2024, 70, "2019", col="red",cex=0.8)
legend("bottomright" , c("Great Britain", "France"),cex=0.7,
       col=c("black","blue"),lty=1)



# Comparison e0, e20, e60, e80 in 2018 and 2050 ####
gb_lt2018f <- lifetable(gb_mx, series = names(gb_mx$rate)[1], years = 2018,
                        ages = gb_mx$age, max.age = min(110, max(gb_mx$age)),
                        type = c("period"))

gb_lt2018m <- lifetable(gb_mx, series = names(gb_mx$rate)[2], years = 2018,
                        ages = gb_mx$age, max.age = min(110, max(gb_mx$age)),
                        type = c("period"))


gb_lt2050f <- lifetable(forecast(gb_LCf, h = 32), years = 2050)
gb_lt2050m <- lifetable(forecast(gb_LCm, h = 32), years = 2050)
gb_lt2050 <- lifetable(forecast(gb_LCt, h = 32), years = 2050)


fr_lt2018f <- lifetable(fr_mx, series = names(fr_mx$rate)[1], years = 2018,
                        ages = fr_mx$age, max.age = min(110, max(fr_mx$age)),
                        type = c("period"))

fr_lt2018m <- lifetable(fr_mx, series = names(fr_mx$rate)[2], years = 2018,
                        ages = fr_mx$age, max.age = min(110, max(fr_mx$age)),
                        type = c("period"))


fr_lt2050f <- lifetable(forecast(fr_LCf, h = 32), years = 2050)
fr_lt2050m <- lifetable(forecast(fr_LCm, h = 32), years = 2050)
fr_lt2050 <- lifetable(forecast(fr_LCt, h = 32), years = 2050)


# Extract values
e0_2018GBf <- gb_lt2018f$ex[1]
e20_2018GBf <- gb_lt2018f$ex[21]
e60_2018GBf <- gb_lt2018f$ex[61]
e80_2018GBf <- gb_lt2018f$ex[81]

e0_2018GBm <- gb_lt2018m$ex[1]
e20_2018GBm <- gb_lt2018m$ex[21]
e60_2018GBm <- gb_lt2018m$ex[61]
e80_2018GBm <- gb_lt2018m$ex[81]

e0_2018GBt <- gb_lt2018$ex[1]
e20_2018GBt <- gb_lt2018$ex[21]
e60_2018GBt <- gb_lt2018$ex[61]
e80_2018GBt <- gb_lt2018$ex[81]

e0_2050GBf <- gb_lt2050f$ex[1]
e20_2050GBf <- gb_lt2050f$ex[21]
e60_2050GBf <- gb_lt2050f$ex[61]
e80_2050GBf <- gb_lt2050f$ex[81]

e0_2050GBm <- gb_lt2050m$ex[1]
e20_2050GBm <- gb_lt2050m$ex[21]
e60_2050GBm <- gb_lt2050m$ex[61]
e80_2050GBm <- gb_lt2050m$ex[81]

e0_2050GBt <- gb_lt2050$ex[1]
e20_2050GBt <- gb_lt2050$ex[21]
e60_2050GBt <- gb_lt2050$ex[61]
e80_2050GBt <- gb_lt2050$ex[81]

e0_2018FRf <- fr_lt2018f$ex[1]
e20_2018FRf <- fr_lt2018f$ex[21]
e60_2018FRf <- fr_lt2018f$ex[61]
e80_2018FRf <- fr_lt2018f$ex[81]

e0_2018FRm <- fr_lt2018m$ex[1]
e20_2018FRm <- fr_lt2018m$ex[21]
e60_2018FRm <- fr_lt2018m$ex[61]
e80_2018FRm <- fr_lt2018m$ex[81]

e0_2018FRt <- fr_lt2018$ex[1]
e20_2018FRt <- fr_lt2018$ex[21]
e60_2018FRt <- fr_lt2018$ex[61]
e80_2018FRt <- fr_lt2018$ex[81]

e0_2050FRf <- fr_lt2050f$ex[1]
e20_2050FRf <- fr_lt2050f$ex[21]
e60_2050FRf <- fr_lt2050f$ex[61]
e80_2050FRf <- fr_lt2050f$ex[81]

e0_2050FRm <- fr_lt2050m$ex[1]
e20_2050FRm <- fr_lt2050m$ex[21]
e60_2050FRm <- fr_lt2050m$ex[61]
e80_2050FRm <- fr_lt2050m$ex[81]

e0_2050FRt <- fr_lt2050$ex[1]
e20_2050FRt <- fr_lt2050$ex[21]
e60_2050FRt <- fr_lt2050$ex[61]
e80_2050FRt <- fr_lt2050$ex[81]


# Merge values into a data frame ####
data_ex <- data.frame(
  Country = c(rep("GB",3),rep("FR",3)),
  Sex = c(rep(c("F","M","T"),2)),
  e0_2018 = c(e0_2018GBf,e0_2018GBm,e0_2018GBt,
              e0_2018FRf,e0_2018FRm,e0_2018FRt),
  e0_2050 = c(e0_2050GBf,e0_2050GBm,e0_2050GBt,
              e0_2050FRf,e0_2050FRm,e0_2050FRt),
  e20_2018 = c(e20_2018GBf,e20_2018GBm,e20_2018GBt,
              e20_2018FRf,e20_2018FRm,e20_2018FRt),
  e20_2050 = c(e20_2050GBf,e20_2050GBm,e20_2050GBt,
              e20_2050FRf,e20_2050FRm,e20_2050FRt),
  e60_2018 = c(e60_2018GBf,e60_2018GBm,e60_2018GBt,
              e60_2018FRf,e60_2018FRm,e60_2018FRt),
  e60_2050 = c(e60_2050GBf,e60_2050GBm,e60_2050GBt,
              e60_2050FRf,e60_2050FRm,e60_2050FRt),
  e80_2018 = c(e80_2018GBf,e80_2018GBm,e80_2018GBt,
              e80_2018FRf,e80_2018FRm,e80_2018FRt),
  e80_2050 = c(e80_2050GBf,e80_2050GBm,e80_2050GBt,
              e80_2050FRf,e80_2050FRm,e80_2050FRt))

data_ex




# Create a table in HTML format ####
kbl(data_ex,format = "html",digits = 1,
    col.names = c(" ", " ", "2018", "2050","2018", "2050",
                  "2018", "2050", "2018", "2050")) %>%
  kable_styling(c("striped")) %>%
  column_spec(column = c(2:10),extra_css = "vertical-align:middle;") %>%
  add_header_above(c(" " = 2, "e0" = 2, "e20" = 2, "e60" = 2, "e80" = 2))



# Projected life tables for 2050 ####
gb_lt2050
fr_lt2050
