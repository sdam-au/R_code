## Draft MCMC algorithm Metropolis-Hasting
## 
## (jaro) 15-09-2020
## a Metropolis method for the standard normal dist. using innovations from a U(-eps,eps) dist.
## @starvalue, @n: iterations, @eps: epsilon
## @type: Random walk Metropolis, Updated likelihood, Logarithmic
## TODO: choosing proposal distribution


metropolis <- function(startvalue=0, n=1000, eps=0.5, type=c("RWM","ulik","log")) {

# chain vector container and initial value
        chain <- vector("numeric", n)
        x <- startvalue

# initial likelihoods to be updated (RWM does not update)
        switch(match.arg(type), RWM = NA,
        ulik = { oldlik <- dnorm(x) },
        log = {  oldlik <- dnorm(x, log=TRUE) }
        )

# start at x=0
        chain[1] <- x

# compute rest of x 
        for (i in seq(2,n)) {
# candidate value is constructed by perturbing current state of the chain with uniform innovation
                new <- runif(1,-eps,eps)
                can <- x+new

# the acceptance probability is computed depending 
        switch(match.arg(type), 
        RWM =  alpha <- min(1, dnorm(can)/dnorm(x)), 
        # updated likelihood
        ulik = { lik <- dnorm(can)
               alpha <- lik/oldlik
        }, 
        # log-acceptance ratio
        log = { loglik <- dnorm(can, log=TRUE)
               alpha <- loglik-oldlik
        }
        )


# pick up an event with probability p by checking if u<p, where u is a draw from a U(0,1) 
        u <- runif(1)

# chain is updated appropriately depending on whether the proposed new value is accepted 
switch(match.arg(type), 
        RWM = { 
            if (u < alpha) 
                    x <- can
        }, 
        ulik = { 
            if (u < alpha) { 
                    x <- can
                    oldlik <- lik
                    }
        },
        log = { 
            if (log(u) < alpha) { 
                    x <- can
                    oldlik <- loglik
                    }
        }
)

# assignment value to the Markov chain
        chain[i] <- x
    }; rm(i)
# return
    chain
}
