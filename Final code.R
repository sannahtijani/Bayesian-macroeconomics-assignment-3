
data <- read.csv("rugged_data.csv")

#the differents model

model1 <- lm(log(rgdppc_2000) ~ rugged * cont_africa, data = data)
model2 <- lm(log(rgdppc_2000) ~ (rugged + log(1 + pop_1400)) * cont_africa, data = data)
model3 <- lm(log(rgdppc_2000) ~ (rugged + dist_coast + log(1 + pop_1400))* cont_africa, data = data)

fit1 <- brm(log(rgdppc_2000) ~ (rugged + dist_coast + log(1 + pop_1400))* cont_africa, data = data,
            prior = c(set_prior("normal(0, 1)", class = "b"),
                      set_prior("inv_gamma(2, 1)", class = "sigma")),
            save_pars = save_pars(all = TRUE),chains=4,
            sample_prior = "yes")


#computing the marginal likelihood
ML1=bridge_sampler(fit1)

#changing the prior parameter
#changing the mean
fit2 <- brm(log(rgdppc_2000) ~ (rugged + dist_coast + log(1 + pop_1400))* cont_africa, data = data,
            prior = c(set_prior("normal(15, 1)", class = "b"),
                      set_prior("inv_gamma(2, 1)", class = "sigma")),
            save_pars = save_pars(all = TRUE),chains=10,
            sample_prior = "yes")

ML2=bridge_sampler(fit2)
#changing the variance 
fit3 <- brm(log(rgdppc_2000) ~ (rugged + dist_coast + log(1 + pop_1400))* cont_africa, data = data,
            prior = c(set_prior("normal(0, 5)", class = "b"),
                      set_prior("inv_gamma(2, 1)", class = "sigma")),
            save_pars = save_pars(all = TRUE),
            sample_prior = "yes")

ML3= bridge_sampler(fit3)

#changing the inverse gamma priors d0 to 0.1

fit4 <- brm(log(rgdppc_2000) ~ (rugged + dist_coast + log(1 + pop_1400))* cont_africa, data = data,
            prior = c(set_prior("normal(0, 1)", class = "b"),
                      set_prior("inv_gamma(2, 0.1)", class = "sigma")),
            save_pars = save_pars(all = TRUE),
            sample_prior = "yes")
ML4=bridge_sampler(fit4)

#changing the inverse gamma prior c0 to 50 
fit5 <- brm(log(rgdppc_2000) ~ (rugged + dist_coast + log(1 + pop_1400))* cont_africa, data = data,
            prior = c(set_prior("normal(0, 1)", class = "b"),
                      set_prior("inv_gamma(50, 1)", class = "sigma")),
            save_pars = save_pars(all = TRUE),
            sample_prior = "yes")
ML5=bridge_sampler(fit5)

#need to run several time the bridgesample to check the stability of the answer 

#We will look at the demanded changes in variance 
fit6 <- brm(log(rgdppc_2000) ~ (rugged + dist_coast + log(1 + pop_1400))* cont_africa, data = data,
            prior = c(set_prior("normal(0, sqrt(0.0001))", class = "b"),
                      set_prior("inv_gamma(2, 1)", class = "sigma")),
            save_pars = save_pars(all = TRUE),
            sample_prior = "yes")

fit7 <- brm(log(rgdppc_2000) ~ (rugged + dist_coast + log(1 + pop_1400))* cont_africa, data = data,
            prior = c(set_prior("normal(0, sqrt(0.01))", class = "b"),
                      set_prior("inv_gamma(2, 1)", class = "sigma")),
            save_pars = save_pars(all = TRUE),
            sample_prior = "yes")

fit8 <- brm(log(rgdppc_2000) ~ (rugged + dist_coast + log(1 + pop_1400))* cont_africa, data = data,
            prior = c(set_prior("normal(0, sqrt(100))", class = "b"),
                      set_prior("inv_gamma(2, 1)", class = "sigma")),
            save_pars = save_pars(all = TRUE),
            sample_prior = "yes")


ML6= bridge_sampler(fit6)
ML7=bridge_sampler(fit7)
ML8=bridge_sampler(fit8)

#We put a normal (0,1) and inverted gamma (2,1) to each model to compare the ML
fit9 <- brm(log(rgdppc_2000) ~ rugged * cont_africa, data = data,
            prior = c(set_prior("normal(0,sqrt(0.01))", class = "b"),
                      set_prior("inv_gamma(2, 1)", class = "sigma")),
            save_pars = save_pars(all = TRUE),
            sample_prior = "yes")


ML9= bridge_sampler(fit9)

fit10 <- brm(log(rgdppc_2000) ~ (rugged + log(1 + pop_1400)) * cont_africa, data = data,
            prior = c(set_prior("normal(0, sqrt(0.01))", class = "b"),
                      set_prior("inv_gamma(2, 1)", class = "sigma")),
            seed = 100,
            save_pars = save_pars(all = TRUE),
            sample_prior = "yes")


ML10= bridge_sampler(fit10)

bf10_9<- bayes_factor(fit10,fit9)
bf1_9<- bayes_factor(fit1,fit9)
bf1_10<- bayes_factor(fit1,fit10)
bf9_10<- bayes_factor(fit9,fit10)
bf10_1<- bayes_factor(fit10,fit1)
bf9_1<-bayes_factor(fit9,fit1)

fit1 <- brm(log(rgdppc_2000) ~ (rugged + dist_coast + log(1 + pop_1400))* cont_africa, data = data,
            prior = c(set_prior("normal(0, 1)", class = "b"),
                      set_prior("inv_gamma(2, 1)", class = "sigma")),
            save_pars = save_pars(all = TRUE),chains=4,
            sample_prior = "yes")

pp_check(fit1)

simulation=data.frame(rgdppc_2000=runif(50,min=2000, max=7000), rugged=runif(50, min=0, max=1),
                      dist_coast= runif(50,min=0, max=0.5), 
                      pop_1400=runif(50,min=200000, max=400000),
                      cont_africa=1)

draw<- as_draws(fit1)

prediction <- posterior_predict(fit1, newdata = simulation, draws = draw)

d <- density(prediction)

# calculate the confidence interval
ci <- quantile(prediction, c(0.025, 0.975))

# plot the density with the confidence interval shaded
plot(d)
polygon(c(d$x[d$x >= ci[1] & d$x <= ci[2]], rev(d$x[d$x >= ci[1] & d$x <= ci[2]])),
        c(d$y[d$x >= ci[1] & d$x <= ci[2]], rep(0, sum(d$x >= ci[1] & d$x <= ci[2]))),
        col = "gray")



