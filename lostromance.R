fun <- function(){
  x <- readline("What is the value of x?")
  y <- readline("What is the value of y?")
  t <- readline("What are the T values?")
  v <- readline("What are the V values?")

  x <- as.numeric(unlist(strsplit(x, ",")))
  y <- as.numeric(unlist(strsplit(y, ",")))
  t <- as.numeric(unlist(strsplit(t, ",")))
  v <- as.numeric(unlist(strsplit(v, ",")))

  out1 <- x + y
  out2 <- t + v

  return(list(out1, out2))

}

ggplotly()

URL to QR code

awk Workshop online

conda environment

add beepr
