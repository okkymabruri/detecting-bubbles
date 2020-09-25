library(psymonitor)  # For testting for bubble monitoring
library(ggplot2)     # To handle plots
library(knitr)       # for nice looking tables
library(xlsx)

setwd('/mnt/sdb2/OneDrive/Who-Am-I/datasci-ai/projects/buble')
snp <- read.xlsx('pd.xlsx', 1, colIndex = 1:2)
#snp$pd <-  1/snp$value
snp$pd <-  snp$value
head(snp)

y        <- snp$pd
obs      <- length(y)
r0       <- 0.01 + 1.8/sqrt(obs)
swindow0 <- floor(r0*obs)
dim      <- obs - swindow0 + 1
IC       <- 2
adflag   <- 6
yr       <- 2
Tb       <- 12*yr + swindow0 - 1
nboot    <- 99

bsadf          <- PSY(y, swindow0, IC, adflag)
quantilesBsadf <- cvPSYwmboot(y, swindow0, IC, adflag, Tb, nboot, nCores = 2) #Note that the number of cores is arbitrarily set to 2.

monitorDates <- snp$date[swindow0:obs]
quantile95   <- quantilesBsadf %*% matrix(1, nrow = 1, ncol = dim)
ind95        <- (bsadf > t(quantile95[2, ])) * 1
periods      <- locate(ind95, monitorDates)

bubbleDates <- disp(periods, obs)
kable(bubbleDates, caption = "Bubble and Crisis Periods in the S&P 500")

ggplot() + 
    geom_rect(data = bubbleDates, aes(xmin = start, xmax = end, 
                                      ymin = -Inf, ymax = Inf), alpha = 0.5) + 
    geom_line(data = snp, aes(date, pd)) +
    labs(title = "Figure: Golden Price",
         subtitle = "February 1970 - September 2020",
         caption = "Notes: The solid
line is the price-to-dividend ratio and the shaded areas are the periods where
the PSY statistic exceeds its 95% bootstrapped critical value.", 
         x = "Year", y = "Ratio") 

write.xlsx(bubbleDates, file = "bubbleDates-direct.xlsx", sheetName="bubbleDates", append=TRUE)

           