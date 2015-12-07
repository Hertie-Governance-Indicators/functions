library(ggplot2)
library(reshape2)

mat1 <- as.matrix(read.csv(file = "data/ml.csv"))
rownames(mat1) <- NULL
colnames(mat1) <- NULL


a1 <- paste(seq_along(c("Yes", "No")), ": ", c("Yes", "No"), sep="")
a1 <-
  c("I hear you have some work to do. Do you wish to enter Michael Lacour Mode?",
    "", a1, "")

b1 <- paste(seq_along(c("Yes", "No")), ": ", c("Yes", "No"), sep="")
b1 <-
  c("Would you like to see the replication data for this?",
    "", b1, "")




writeLines(a1)
pick <- readline("Selection: ")


if (pick == "No") {
  cat("Hmm, that's not very nice of you.")
  options(prompt="I AM A HOMOPHOBE AND THIS IS MY CODE > ")
} else {
  cat("OK LET'S DO THIS. I hear you have to write a book chapter on culture and democracy.
      Sounds like hard work. But why do the hard work when the founder of
      beautifuldataviz.com can do it for you! Well have I ever got a present for you!")
  Sys.sleep(2)
  
  culture <- rnorm(50, 0, 1)
  democracy <- 0.5 * culture + rnorm(50, 0, 0.2)
  
  print(qplot(y = democracy, x = culture) + geom_smooth())
  Sys.sleep(2)
  cat("Pretty neat. My boutique statistical analysis skills and data collection
      show that culture causes democracy!")
  
  Sys.sleep(2)
  
  writeLines(b1)
  pick2 <- readline("Selection: ")
  
  if (pick2 == "Yes") {
    cat("Err, one second.")
    Sys.sleep(2)
    q(save = "no")
  } else {
    print(ggplot(melt(mat1), aes(Var1,Var2, fill=value)) + geom_raster() + scale_y_reverse() + 
      scale_fill_gradient(low="darkblue", high="white", guide = F) + ggtitle("Good Job!!") +
      theme_bw() + xlab("") + ylab(""))
  }
  
}
