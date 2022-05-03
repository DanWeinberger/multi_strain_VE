#use random walk prior on beta1
jags_mod3 <- 
"
model{
     
for(i in 1:2){
#Likelihood
y[ 1:N_cats,i] ~ dmulti(p[ 1:N_cats,i], m[i])
#Multionmial Logistic Regression    
for(j in 1:(N_cats-1)){
p_temp[j,i] <- exp(beta0[j] + beta1[j]*vax[i])
}
for(j in 1:(N_cats-1)){
p[j,i] <- p_temp[j,i]/(1 + sum(p_temp[ 1:(N_cats-1),i]))
}
p[N_cats,i] <- 1/(1 + sum(p_temp[ 1:(N_cats-1),i]))
}
#Serotype-Specific Vaccine Effects
#100*(p_{no_vax} - p_{vax})/p_{no_vax}
for(j in 1:(N_cats-1)){
sero_vax_effect[j] <- 100*(1 - p[j,2]/p[j,1])
}
#Priors--iid for intercept; random walk for slope
for(j in 1:(N_cats-1)){
beta0[j] ~ dnorm(mu_beta0, inv_var_beta0)
}

beta1[1] <- delta0 #effect of vax for smallest dist category
delta1[1] <- 0

for(j in 2:(N_cats-1)){
  beta1[j] <- delta0 + delta1[j]*dist[j]
  delta1[j] <- rho*delta1[j-1] + phi_delta1
}
phi_delta1 ~ dnorm(0,inv_var_phi_delta1)

rho ~ dunif(0,1)
delta0 ~ dnorm(0, 0.0001)
mu_beta0 ~ dnorm(0, 0.0001)
mu_beta1 ~ dnorm(0, 0.0001)
inv_var_beta0 ~ dgamma(0.01, 0.01)
inv_var_phi_delta1 ~ dgamma(0.01, 0.01)
#Overall Vaccine Effect
overall_vax_effect <- 100*(1 - (1 - p[N_cats,2])/(1 - p[N_cats,1]))
}
"