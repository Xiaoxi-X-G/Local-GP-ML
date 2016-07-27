BestArimaParam <- function(x.ts, maxord)
## Input1 = x.ts
## Input2 = exog, is exogeneous variable
## Input3 = maxord, is a array, e.g. maxord= c(1,1,1,1,1,1)
  
{
  best.aic <- 1e8
  n <- length(x.ts)
  for (p in 0:maxord[1]) for(d in 0:maxord[2]) for(q in 0:maxord[3])
    for (P in 0:maxord[4]) for(D in 0:maxord[5]) for(Q in 0:maxord[6])
    {
      fit <- arima(x.ts, order = c(p,d,q),
                   seas = list(order = c(P,D,Q), period = 7),  method = "CSS")
      fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
      if (fit.aic < best.aic)
      {best.aic <- fit.aic
      best.fit <- fit
      best.model <- c(p,d,q,P,D,Q)
      }
    }
  return(list(best.aic, best.fit, best.model))
}