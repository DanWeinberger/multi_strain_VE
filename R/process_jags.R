process_jags <- function(jags_post_samples){

post_means<-apply(jags_post_samples, 2, median)
sample.labs<-names(post_means)
ci<-t(hdi(jags_post_samples, credMass = 0.95))
ci<-matrix(sprintf("%.1f",round(ci,1)), ncol=2)
row.names(ci)<-sample.labs
post_means<-sprintf("%.1f",round(post_means,1))
names(post_means)<-sample.labs

yrange<-range(ci)

overall.VE<-c(post_means[1], ci[1,])
st.VE<- cbind(post_means[-1], ci[-1,])

out.list=list('overall.VE'=overall.VE,'st.VE'=st.VE)
return(out.list)

}