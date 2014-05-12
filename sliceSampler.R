sliceSampler1D <- function(f, N=5000, rInitDistr=function() rnorm(1, mean=0, sd=10)){
  while(TRUE){
    currTheta <- rInitDistr()
    if (f(currTheta)>0) break
  }
  s <- vector()
  for (i in 1:N){
    height <- f(currTheta)
    sliceLevel <- runif(1,min=0,max=height)
    halfWidth <- 0.01
    ##if either endpoint is above the slice level, try again: expand width until both are below the slice level
    while((f(currTheta-halfWidth)>sliceLevel)||(f(currTheta+halfWidth)>sliceLevel))
      halfWidth <- 2*halfWidth
    while(TRUE){ ##sample until the proposed theta is inside
      propTheta <- runif(1, min=currTheta-halfWidth, max=currTheta+halfWidth)
      if(f(propTheta)>sliceLevel) break
    }
    currTheta <- propTheta
    s[i] <- currTheta
  }
  s
}


sliceSampler <- function(logf, N=50, rInitDistr=function() rnorm(k, mean=thetaHat, sd=1)){
  k <- 2 ## number of parameters
  print("hello1")
  while(TRUE){
    currTheta <- rInitDistr()
    if (logf(currTheta)>-Inf) break
  }
  print("hello2")
  s <- matrix(nrow=N,ncol=2)
  for (i in 1:N){
    inspect(i)
    for (j in 1:k){ ##update each dimension separately
      height <- logf(currTheta)
      sliceLevel <- height - rexp(1)
      halfWidth <- rep(0,k)
      halfWidth[j] <- 0.01
      ##if either endpoint is above the slice level, try again: expand width until both are below the slice level
      while(TRUE){
        if((logf(currTheta-halfWidth) < sliceLevel) && logf(currTheta+halfWidth) < sliceLevel) break
        halfWidth <- 2*halfWidth
      }
      while(TRUE){ ##sample until the proposed theta is inside
        propThetaJ <- runif(1, min=currTheta[j]-halfWidth[j], max=currTheta[j]+halfWidth[j])
        propTheta <- currTheta; propTheta[j] <- propThetaJ
        if(logf(propTheta)>sliceLevel) break
      }
      currTheta <- propTheta
    }
    inspect(currTheta)
    s[i,] <- currTheta
  }
  s
}


## make a pedagogical animation
sliceSamplerDemo <- function(f, xlim, waitTime=1, N=5000, rInitDistr=function() rnorm(1, mean=3, sd=1), ...){
  gr <- makeGrid(xlim[1],xlim[2],1000)
  plot(gr,sapply(gr,f), type="l", xlab="theta", ylab="unnormalized density", ...)
  while(TRUE){
    currTheta <- rInitDistr()
    if (f(currTheta)>0) break
  }
  s <- vector()
  Sys.sleep(waitTime)
  for (i in 1:N){
    Sys.sleep(4*waitTime); print("Drawing current theta")
    ##abline(v=currTheta, col="black")
    height <- f(currTheta)
    points(c(currTheta,currTheta),c(0,height), type="l", col="black")
    sliceLevel <- runif(1,min=0,max=height)
    Sys.sleep(4*waitTime); print("Drawing slice")
    ##abline(h=sliceLevel, col="black")
    halfWidth <- 0.01
    ##if either endpoint is above the slice level, try again: expand width until both are below the slice level
    while((f(currTheta-halfWidth)>sliceLevel)||(f(currTheta+halfWidth)>sliceLevel))
      halfWidth <- 2*halfWidth
    Sys.sleep(2*waitTime); print("Drawing endpoints")
    points(c(currTheta-halfWidth,currTheta+halfWidth), c(sliceLevel, sliceLevel), pch=c(40,41))
    points(c(currTheta-halfWidth,currTheta+halfWidth),c(sliceLevel,sliceLevel), type="l", col="black")
    Sys.sleep(4*waitTime); print("proposed Theta")
    while(TRUE){ ##sample until the proposed theta is inside
      propTheta <- runif(1, min=currTheta-halfWidth, max=currTheta+halfWidth)
      inspect(propTheta)
      inspect(c(propTheta,sliceLevel))
      points(propTheta,sliceLevel, col="red", pch=4)  ## grey 'x'
      Sys.sleep(2*waitTime); print("proposed Theta")
      ##points(c(propTheta,sliceLevel), col="white", pch=4) ## erase it
      if(f(propTheta)>sliceLevel) break
    }
    Sys.sleep(2*waitTime); print("this Theta is accepted")
    points(propTheta,sliceLevel, col="black", pch=4) ## black 'x'
    Sys.sleep(4*waitTime); print("moving on...")
    abline(h=sliceLevel, col="white")
    points(c(currTheta,currTheta),c(0,height), type="l", col="white")
    currTheta <- propTheta
    s[i] <- currTheta
  }
  s
}


## example

f <- function(x) 0.3*dnorm(x,0,1) + 0.7*dnorm(x,3,3)

sliceSamplerDemo(f,xlim=c(-10,10), waitTime=0.5)
