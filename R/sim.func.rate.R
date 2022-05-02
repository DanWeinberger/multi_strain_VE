
sim.func.rate <- function(vax.irr=0.4,prop.vax=0.8, p.case=0.1,p.control=0.3, n.people=100000){
  prop.vax <- prop.vax
  vax.irr <- vax.irr
  p.case <- p.case
  vax <- rbinom(n.people,1,prop.vax)
  
  log_pi_case <- log(p.case) + vax*log(vax.irr)
  pi_case <- exp(log_pi_case)
  
  dist= 1 - (vax.irr) #for simulation, assume linear inverse relationship between genertic distance and IRR (perfect match=highest VE=smallest IRR)
  
  y.full_case <- rbinom(n.people,1,pi_case)

  y.full_control <- rbinom(n.people,1,p.control)
  
  y.case <- cbind.data.frame('vax'=vax[y.full_case==1]) 
  y.case$case <- 1
  
  y.control <- cbind.data.frame('vax'=vax[y.full_case==0 & y.full_control==1 ]) 
  y.control$case <- 0
  
  ds <- dplyr::bind_rows( y.case, y.control)

  ds$dist <- 1-vax.irr
    return(ds)
}
