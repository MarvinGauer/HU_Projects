# Cheat Sheet Statistics of Financial Markets 1
# Author: Marvin Gauer

library(ggplot2)

# SlideSet 02 - The Basics of Option Management

  # Forward Value
  ForwardValue = function(K,S_t,t,r, D_t = 0, coc = 0){
    
    # Note: Either D_t or coc should be empty
    
    # K   = Delivery Price 
    # S_t = Spot Price
    # t   = Time to Maturity in years
    # r   = Riskless Interest Rate
    # D_t = Net present Value of Dividends
    # coc = 0 if no continous cost of carry are charged
    
    V = ifelse(coc == 0,S_t - K*exp(-1*r*t) - D_t,S_t * exp((coc-r)*t) - K * exp(-1*r*t))
      return(V)
    
  }
  
  # Forward Price
  ForwardPrice = function(S_t,t,r, D_t = 0, coc = 0){
    
    # Note: Either D_t or coc should be empty
    
    # S_t = Spot Price
    # t   = Time to Maturity in years
    # r   = Riskless Interest Rate
    # D_t = Net present Value of Dividends
    # coc = 0 if no continous cost of carry are charged
    
    P = ifelse(coc == 0, (S_t-D_t)*exp(r*t), S_t * exp(coc*t))
    return(P)
    
  }

  # Payoff Call at Maturity
  PayoffCall   = function(S_T,K,Long = TRUE){
    
    # S_t  = Spot Price
    # K    = Delivery Price 
    # Long = Indicates whether there is a long or short position
    
    C = ifelse(Long == TRUE,max(S_T - K,0),-1*max(S_T - K,0))
    return(C)
  }
  
  # Payoff Put at Maturity
  PayoffPut   = function(S_T,K,Long = TRUE){
    
    # S_t  = Spot Price
    # K    = Delivery Price 
    # Long = Indicates whether there is a long or short position
    
    P = ifelse(Long == TRUE,max(K - S_T,0),-1*max(K - S_T,0))
    return(P)
  }

  # Put Call Parity
  PutCallParity = function(C_t, P_t, S_t, K, r, t, D_t = 0, coc = 0){
    
    #Note: Either C_t or P_t should be NULL. Furthermore, either D_t or coc should be empty
    
    # S_t  = Spot Price
    # K    = Delivery Price 
    # t    = Time to Maturity in years
    # r    = Riskless Interest Rate
    # D_t  = Net present Value of Dividends
    # coc  = 0 if no continous cost of carry are charged
    # C_t  = Current Call Price
    # P_t  = Current Put Price
    
    if(is.null(P_t) == TRUE & is.null(C_t) == FALSE){
      P_t = ifelse(coc == 0, C_t - S_t + K * exp(-1*r*t) + D_t, C_t-S_t*exp((coc-r)*t)+K*exp(-1*r*t))
      return(P_t)
    }
    if(is.null(C_t) == TRUE & is.null(P_t) == FALSE){
      C_t = ifelse(coc == 0,  P_t + S_t - K * exp(-1*r*t) - D_t, P_t+S_t*exp((coc-r)*t)-K*exp(-1*r*t))
      return(C_t)
    }
    else{
      print('Error -> See Note')
      return(NULL)
    }
  }
  
# SlideSet 06 - The Basics of Option Management
  
  # Geometric Brownian Motion
  GeoBM = function(mu, S_0, sig, NOd_t = 1000, NOPath = 10){
    
    # S_0   = Spot Price 
    # mu    = Drift which reflects current expectation of the returns
    # sig   = Volatility around the drift
    # NOd_t = No. of Time Steps
    
    d_t = 1/NOd_t
    
    DW = matrix(nrow=NOd_t-1,ncol=NOPath)
    S = matrix(nrow=NOd_t,ncol=NOPath)
    
    DW = matrix(rnorm((NOd_t-1)*NOPath,mean=0,sd=1),nrow=NOd_t-1)
    
    S[1,] = S_0
    
    for (i in 1:(NOd_t-1)){
      S[i+1,] = S[i,] + S[i,]*mu*d_t + sig * sqrt(d_t) * S[i,]*DW[i,]
    }
    
    # Plotting
    achse = seq(0,1-d_t,by = d_t)
    plot(achse,S[,1],
         type="l", 
         col="red",
         axes=FALSE,
         main="Geometric Brownian Motion",
         xlab ="Time Step",
         ylab ="Stock Price",
         ylim=c(min(S),max(S)))
    axis(1)
    axis(2)
    for (i in (1:NOPath)){lines(achse,S[,i],col=i)}
    return(S)
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  