# Sum of log likelihoods
likelihood <- function(param){
  a = param[1]
  b = param[2]
  sd = param[3]
  
  pred = a*x + b
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
  sumll = sum(singlelikelihoods)
  return(sumll)   
}

# Prior distribution
prior <- function(param){
  a = param[1]
  b = param[2]
  sd = param[3]
  aprior = dunif(a, min=0, max=10, log = T)
  bprior = dnorm(b, sd = 5, log = T)
  sdprior = dunif(sd, min=0, max=30, log = T)
  return(aprior+bprior+sdprior)
}

# Posterior distribution
posterior <- function(param){
  return (likelihood(param) + prior(param))
}

# Proposal distribution
proposalfunction <- function(param){
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
}

# Metropolis-Hastings algorithm
run_metropolis_MCMC <- function(startvalue, iterations){
  chain = array(dim = c(iterations+1,3))
  chain[1,] = startvalue
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,])
    
    probab = exp(posterior(proposal) - posterior(chain[i,]))
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
  }
  return(chain)
}

# Create summary plots
createplots <- function(chain, burnIn, param){
  hists <- function(chain, burnIn, param, paramrow, paramname){
    hist(chain[-(1:burnIn),paramrow],nclass=30, main=paste(c("Posterior of ", paramname)), xlab="True value = red line" )
    abline(v = mean(chain[-(1:burnIn),paramrow]))
    abline(v = param[paramrow], col="red" )
  }
  plots <- function(chain, burnIn, param, paramrow, paramname){
    plot(chain[-(1:burnIn),paramrow], type = "l", xlab="True value = red line" , main = paste(c("Chain values of ", paramname)))
    abline(h = param[paramrow], col="red" )
  }
  
  hists(chain,burnIn,param,1,"a")
  hists(chain,burnIn,param,2,"b")
  hists(chain,burnIn,param,3,"sd")
  
  plots(chain,burnIn,param,1,"a")
  plots(chain,burnIn,param,2,"b")
  plots(chain,burnIn,param,3,"sd")
}

compare_outcomes <- function(iterations){
  mean.a <- rep(0,10)
  sd.a <- rep(0,10)
  
  for(i in 1:10){
    # Randomly create starting values for a, b and sd
    startvalues <- rnorm(3, mean = c(4,0,10), sd = 1)
    chain <- run_metropolis_MCMC(startvalues, iterations)
    
    # Store the mean of the values in the chain for a
    mean.a[i] <- mean(chain[,1])
    # Store the std of the values in the chain for a
    sd.a[i] <- sd(chain[,1])
  }
  
  print(mean.a)
  print(sd.a)
}
