rm(list = ls())

library(pacman)
pacman::p_load(RandomFields, geoR, fields, prodlim, TMB, INLA, dplyr, parallel, rstan,
               tidybayes, gridExtra, mvtnorm)

options(scipen = 999)

# Calculate the number of cores
no_cores <- parallelly::availableCores() - 1  
rstan_options(auto_write = TRUE)


set.seed(123) 
draws <- 5 # number of draws 
n <- 100   # length of f 
x <- seq(0, 1, len = n) 
alpha <- 2 
rho <- 0.05


K <- matrix(rep(0, n*n), nrow = n) 
for (i in 1:n) {
  for (j in 1:n) { 
    K[i,j] <- alpha^2 * exp(-0.5/rho^2*(x[i]-x[j])^2) 
  }
}

# GP
gp <- mvtnorm::rmvnorm(5, sigma=K) # mean defaults to 0
matplot(x, t(gp), 
        type="l", 
        ylab="gp")



# Adding in a variance parameter
sigma_gp <- 2
gp <- rmvnorm(5, sigma=(sigma_gp^2)*K)
matplot(x, t(gp), 
        type="l", 
        ylab="y")


# Simulating the response y with additional noise
sigma_y <- 1.0
epsilon <- rnorm(length(x), 0, sigma_y) # random noise (mean = 0, sigma = sigma_y)

# GP (again with only 1 draw)
gp <- rmvnorm(1, sigma=(sigma_gp^2)*K) 

# Simulating the response variable
y <- c(gp) + epsilon 

# Plot the data
data = tibble(day = day,
              x = x,
              y = y, 
              gp = c(gp))


ggplot(data, aes(x = day, y = y)) +
  geom_point(aes(colour = "Datos sim"), size = 2.2) +
  geom_line(aes(x = day, y = gp, colour = "GP"), linewidth = 0.8) +
  labs(x = "Day", y = "y", colour = "") + 
  theme(axis.text=element_text(size=12),
        axis.text.x = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12, face="bold"),
        axis.title=element_text(size=14, face="bold"),
        legend.text=element_text(size = 18))



#================================================
#             Stan model 
#================================================
n_pred = 25
x_pred = seq(range(data$x)[1], 1.2, length.out = n_pred)
n_obs = 50
obs = sort(sample(1:length(x), n_obs))

# Stan data
stan_data <- list(N1 = n_obs,  x1 = x[obs], y1 = y[obs],
                  N2 = n_pred, x2 = x_pred)

#================
# Fit the model
startTime <- Sys.time()
fit_stan <- stan(file='gp_timeseries1.stan', data = stan_data, 
                 iter = 3000, chains = 3, warmup = 1000, 
                 control = list(max_treedepth = 12, adapt_delta = 0.9), 
                 cores = no_cores, open_progress=FALSE, seed=483892929)
endTime <- Sys.time()
timeUsed = endTime

require(MCMCvis)
posterior <- as.matrix(fit_stan)
mcmc_pairs(posterior, pars = c("rho","alpha","sigma"),
           off_diag_args = list(size = 0.75))

# 
pred_params = extract(fit_stan)


plot(c(x[1], 1.2), c(-10, 10), ty = "n", xlab = substitute(paste(bold('Day/100'))), 
     ylab = substitute(paste(bold('y'))), cex.lab = 1.2)
for(i in 1:100){
  lines(x_pred, pred_params$f[i, (n_obs+1):(n_obs + n_pred)], col = rgb(0, 0, 0.1, 0.1))
}

points(x, y, pch = 20, col = "blue", bg=5, cex = 1.5)
legend(x = "topleft",          # Position
       legend = c("Points", "GP_pred"),  # Legend texts
       pch = c(19, NA),
       lty = c(NA, 1),           # Line types
       col = c("blue", "grey"),           # Line colors
       lwd = 2,
       cex = 1.2)                 # Line width




par_dat <-  posterior %>% spread_draws(alpha, rho, sigma)

plot1 <- ggplot(par_dat, aes(x = alpha)) +
  stat_halfeye() +
  geom_vline(data = tibble(sigma_gp), aes(xintercept = sigma_gp, colour = "Valor inicial"))+
  labs(x = "alpha", y = "y", colour = "") + 
  theme(axis.text=element_text(size=12),
        axis.text.x = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12, face="bold"),
        axis.title=element_text(size=14, face="bold"),
        legend.text=element_text(size = 18))


plot2 <- ggplot(par_dat, aes(x = rho)) +
  stat_halfeye() +
  geom_vline(data = tibble(phi), aes(xintercept = phi, colour = "Valor inicial"))+
  labs(x = "rho", y = "y", colour = "") + 
  theme(axis.text=element_text(size=12),
        axis.text.x = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12, face="bold"),
        axis.title=element_text(size=14, face="bold"),
        legend.text=element_text(size = 18))


plot3 <- ggplot(par_dat, aes(x = sigma)) +
  stat_halfeye() +
  geom_vline(data = tibble(sigma_y), aes(xintercept = sigma_y, colour = "Valor inicial"))+
  labs(x = "sigma", y = "y", colour = "") + 
  theme(axis.text=element_text(size=12),
        axis.text.x = element_text(size = 12, face="bold"), 
        axis.text.y = element_text(size = 12, face="bold"),
        axis.title=element_text(size=14, face="bold"),
        legend.text=element_text(size = 18))

grid.arrange(plot2, plot1, plot3, ncol = 1)
