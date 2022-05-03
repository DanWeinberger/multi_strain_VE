call_jags <- function(set.mod=jags_mod1, y=cat.pop1.c[,-1],n.burn=10000, n.samples=20000){

m <- apply(y,2, sum)
  
##############################################################
#Model Fitting
##############################################################
inits1=list(".RNG.seed"=c(123), ".RNG.name"='base::Wichmann-Hill')
inits2=list(".RNG.seed"=c(456), ".RNG.name"='base::Wichmann-Hill')
inits3=list(".RNG.seed"=c(789), ".RNG.name"='base::Wichmann-Hill')


##############################################
#Model Organization
##############################################
model_spec<-textConnection(jags_mod1)
model_jags<-jags.model(model_spec, 
                       inits=list(inits1,inits2, inits3),
                       data=list('y' = y,
                                 'm' = m,
                                 'N_cats'=nrow(y),
                                 'vax' = c(0,1)),
                       n.adapt=n.burn, 
                       n.chains=3)

params<-c('overall_vax_effect',
          'sero_vax_effect')

##############################################
#Posterior Sampling
##############################################
posterior_samples<-coda.samples(model_jags, 
                                params, 
                                n.iter=n.samples)
posterior_samples.all<-do.call(rbind,posterior_samples)
#post1.summary<-summary(posterior_samples)
#post_means<-colMeans(posterior_samples.all)

return(posterior_samples.all)
}