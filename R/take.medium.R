take.medium <-
function(vec){
  vec2 <- rep(0,length(vec)-1)
  for(i in 1:(length(vec)-1)){
    vec2[i] <- mean(c(vec[i+1],vec[i]))
  }
  vec2
}

