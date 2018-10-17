# Set the slope parameter to 5
trueA <- 5
# Set the intercept parameter to 0
trueB <- 0
# Set the standard deviation of the noise to 10
trueSd <- 10
# Set the number of data points to 31
sampleSize <- 31

# create independent x-values 
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)
# create dependent values according to ax + b + N(0,sd)
y <-  trueA * x + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd)

# Plot the dependent values against the independent values
plot(x,y, main="Test Data")


# Create the likelihood function for a given set of parameters
likelihood <- function(param){
  # Set the slope parameter to the first given parameter
  a = param[1]
  # Set the intercept parameter to the second given parameter
  b = param[2]
  # Set the standard deviation to the third given parameter
  sd = param[3]
  
  # Compute the prediction values using the given slope and intercept
  pred = a*x + b
  # For each data point, compute the log likelihood given that y ~ N(a*x + b, sd)
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
  # Add up the log likelihoods for all data points
  sumll = sum(singlelikelihoods)
  # Return this sum
  return(sumll)   
}

# Example: plot the likelihood profile of the slope a
# Create a function calculating the sum of the likelihoods for a given slope parameter x, using the true values for the intercept
# parameter and the standard deviation
slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))}
# Compute the sum of the likelihoods for various values of the slope parameter
slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues )
# Plot the sum of the likelihoods against the values of the slope parameter
plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")


# Prior distribution
prior <- function(param){
  # Set the slope parameter to the first given parameter
  a = param[1]
  # Set the intercept parameter to the second given parameter
  b = param[2]
  # Set the standard deviation to the third given parameter
  sd = param[3]
  # Get the log likelihood of the given slope parameter, using a prior uniform distribution
  aprior = dunif(a, min=0, max=10, log = T)
  # Get the log likelihood of the given intercept parameter, using a prior normal distribution
  bprior = dnorm(b, sd = 5, log = T)
  # Get the log likelihood of the given standard deviation, using a prior uniform distribution
  sdprior = dunif(sd, min=0, max=30, log = T)
  # Return the sum of the log likelihoods of the three parameters
  return(aprior+bprior+sdprior)
}


# Posterior distribution
posterior <- function(param){
  # Return the sum of the log likelihoods of both the data points and the parameters using the prior distributions
  return (likelihood(param) + prior(param))
}


######## Metropolis algorithm ################

# Set a proposal function used in the algorithm
proposalfunction <- function(param){
  # Take a normal distribution for the three parameters as the proposal functions
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
}

# The actual algorithm
run_metropolis_MCMC <- function(startvalue, iterations){
  # Create an array with the proper dimensions
  chain = array(dim = c(iterations+1,3))
  # Set the first elements of the 3 rows to be the given starting values for the parameters to be estimated
  chain[1,] = startvalue
  # Start a for loop running through each iteration
  for (i in 1:iterations){
    # Draw from the proposal function the get new parameter values as a proposal
    proposal = proposalfunction(chain[i,])
    
    # Compute the probability of accepting the proposal (which is 1 if the computed value is bigger than 1)
    probab = exp(posterior(proposal) - posterior(chain[i,]))
    # Go through if loop with this probability
    if (runif(1) < probab){
      # Accept the proposal
      chain[i+1,] = proposal
    # Go through else loop if the proposal is not accepted
    }else{
      # Keep the parameters as is, and add them to the chain
      chain[i+1,] = chain[i,]
    }
  }
  # Return the final chain of parameter values
  return(chain)
}

# Set starting values for the three parameters
startvalue = c(4,0,10)
# Apply the algorithm to these values, using 10000 iterations
chain = run_metropolis_MCMC(startvalue, 10000)

# Set the amount of values in the chain that will be discarded
burnIn = 5000
# Calculate the acceptance rate
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))


### Summary: #######################

# Show the following plots in a 2 by 3 roster
par(mfrow = c(2,3))
# Plot the histogram of the estimated posterior of the slope parameter
hist(chain[-(1:burnIn),1],nclass=30, main="Posterior of a", xlab="True value = red line" )
# Add a line showing the mean value of the estimated slope parameter
abline(v = mean(chain[-(1:burnIn),1]))
# Add a line showing the true value of the slope parameter
abline(v = trueA, col="red" )
# Plot the histogram of the estimated posterior of the intercept parameter
hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of b", xlab="True value = red line")
# Add a line showing the mean value of the estimated intercept parameter
abline(v = mean(chain[-(1:burnIn),2]))
# Add a line showing the true value of the intercept parameter
abline(v = trueB, col="red" )
# Plot the histogram of the estimated posterior of the standard deviation
hist(chain[-(1:burnIn),3],nclass=30, main="Posterior of sd", xlab="True value = red line")
# Add a line showing the mean value of the estimated standard deviation
abline(v = mean(chain[-(1:burnIn),3]) )
# Add a line showing the true value of the standard deviation
abline(v = trueSd, col="red" )
# Plot the values the estimated slope parameter has taken in the algorithm
plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a")
# Add a line showing the true value of the slope parameter
abline(h = trueA, col="red" )
# Plot the values the estimated intercept parameter has taken in the algorithm
plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b")
# Add a line showing the true value of the intercept parameter
abline(h = trueB, col="red" )
# Plot the values the estimated standard deviation has taken in the algorithm
plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of sd")
# Add a line showing the true value of the standard deviation
abline(h = trueSd, col="red" )

# for comparison:
summary(lm(y~x))