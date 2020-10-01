calAIC <-
function(p,sse,n,k,bias.corrected.AIC=F){
  if(bias.corrected.AIC==F){
    n*(log(2*pi*sse/n)+1)+k*p
  }
  else{
    n*(log(2*pi*sse/n)+1)+k*p*(n/(n-p-1))    
  }
}

