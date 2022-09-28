jags_mod1a <- 
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
#Priors

#log.beta[1] ~ dnorm(0,(1-rho3^2)*tau3.beta)
# for(i in 2:10){
#  log.beta[i] ~ dnorm(rho3*beta[i-1], tau3.beta) #AR(1) model
# }

  beta0[1] ~ dnorm(0, (1-rho0^2)*tau0.beta)
  beta1[1] ~ dnorm(0, (1-rho1^2)*tau1.beta)
  
for(j in 2:(N_cats-1)){
  beta0[j] ~ dnorm(rho0*beta0[j-1], tau0.beta)
  beta1[j] ~ dnorm(rho1*beta1[j-1], tau1.beta)
}

tau0.beta ~ dgamma(3,2)
tau1.beta ~ dgamma(3,2)

rho0 ~ dunif (-1,1)
rho1 ~ dunif (-1,1)

#Overall Vaccine Effect
overall_vax_effect <- 100*(1 - (1 - p[N_cats,2])/(1 - p[N_cats,1]))
}
"