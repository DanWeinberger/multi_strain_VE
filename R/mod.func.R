mod.func <- function(ds){
  mod1 <- glm(case ~ vax, family='binomial', data=ds)
  irr <- exp(mod1$coefficients['vax'])
  return(c(irr, 'dist'=mean(unique(ds$dist))))
}