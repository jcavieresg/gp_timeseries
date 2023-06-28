rm(list = ls())

library(pacman)
pacman::p_load(RandomFields, geoR, fields, prodlim, TMB, INLA, dplyr, parallel, rstan,
               tidybayes, gridExtra, mvtnorm)

options(scipen = 999)

# Calculate the number of cores
no_cores <- parallelly::availableCores() - 1  
rstan_options(auto_write = TRUE)


#===================================================================
# Example 2
library(quantmod)
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Apple price using quantmod

getSymbols("AAPL", 
           from = '2017-01-01',
           to = "2018-12-31",warnings = FALSE,
           auto.assign = TRUE)

head(AAPL, 3)
dim(AAPL)

# Create an index to represent the time
data2 <- cbind(ID = 1:nrow(AAPL), AAPL)    # Applying cbind function
data2  



# Convert the AAPL xts to a data.frame
data2 <- as.data.frame(data2)
head(data2$ID)

#data_ts = list(N = nrow(data2), x1 = data2$ID, y1 = data2$AAPL.Close)


N_predict = 50
x_predict = seq(range(data2$ID)[1], 503, length.out = N_predict)
N_obs = 100
Obs = sort(sample(1:length(data2$ID), N_obs))

stan_data <- list(N1 = N_obs, x1 = data2$ID[Obs], y1 = data2$AAPL.Close[Obs],
                  N2 = N_predict, x2 = x_predict)



# Stan model
startTime <- Sys.time()
pred_stan <- stan(file='gp_timeseries2.stan', data = stan_data, 
                  iter = 3000, chains = 3, warmup = 1000, 
                  control = list(max_treedepth = 10, adapt_delta = 0.8), 
                  cores = no_cores, open_progress=FALSE, seed=483892929)
endTime <- Sys.time()
timeUsed = endTime


pred_params = extract(pred_stan)


# 
plot(c(data2$ID[1], 503), c(20, 60), ty = "n", xlab = substitute(paste(bold('Days'))), 
     ylab = substitute(paste(bold('Valores APPL al ciere'))), cex.lab = 1.2)
for(i in 1:300){
  lines(x_predict, pred_params$f[i, (N_obs+1):(N_obs + N_predict)], col = rgb(0, 0, 0.1, 0.1))
}
lines(data2$ID, data2$AAPL.Close, lty = 1, col = "blue", lwd = 3)
legend(x = "topleft",          # Position
       legend = c("APPL values", "GP_pred"),  # Legend texts
       pch = c(NA, NA),
       lty = c(1, 1),           # Line types
       col = c("blue", "grey"),           # Line colors
       lwd = 2,
       cex = 1.2)                 # Line width

