fibonacci_search <- function(f,a,b,n,e = 0.01){
  g_ratio <- 1.61803;
  s <- (1-sqrt(5)) / (1 + sqrt(5));  #fibonacci s
  p <- 1 / (g_ratio * (1 - (s^(n+1))) / (1 - (s^n))); #ratio
  d <- p*b + ((1 - p) * a); # distance
  yd <- f(d); # eval target func
  i <- 0;
  for(i in 1:n - 1){
    if(i == n-1){
      c <- ((e * a) + ((1-e) * b));
    }
    else{
      c <- ((p * a) + ((1-p) * b));
    }
    
    yc = f(c);
    
    if(yc < yd){
      b <- d;
      d <- c;
      yd <- yc;
    }
    else{
      a <- b;
      b <- c;
    }
    
    p <- 1 / (g_ratio *( 1 - (s^(n-i+1))) / (1 - (s^(n-i))));
  }
  if(a<b){
    print('a < b');
    print(c(a,b));
    print(f(a));
  }
  else{
    print('b < a');
    print(c(b,a));
    print(f(b));
  }
}

fibonacci_search(function(x) exp(x - 2) - x,-2,6,4);
