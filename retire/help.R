cs <- cusum(failure_probability = 0.13, 
            patient_outcomes = gcusum_example_data$y[gcusum_example_data$year == 2017],
            limit = 2.5,
            odds_multiplier = 2,
            reset = FALSE)

signals <- cs[cs$signal == 1,]

x <- NULL
for(i in 2:nrow(signals)-1){
  
  if((signals$t[i+1] - signals$t[i]) > 1){
    x <- rbind(x, signals$t[i+1])
    print(signals$t[i])
  }
  print(c(i, signals$t[i]))
  
}
x <- rbind(x,min(signals$t))
x <- data.frame(t = x,
                ct = cs$ct[x],
                y = rep(2.5, nrow(x)))


ggplot(cs, aes(x = t, y = ct)) +
  geom_point(data = x, 
             aes(x = t, y = ct),
             col = "#f7ba02", 
             size = 4, 
             pch = 8, stroke = 2)+
  geom_line() +
  geom_point(size = 1) +
  geom_hline(aes(yintercept = 2.5), size = 1, col = "#4063bc")+
  theme_bw()

head(gcusum_example_data)

t <- seq(1,100)
y <- rbinom(100,1,0.2)
example_data <- data.frame(t, y)
write.csv(example_data, file = "data/example_data.csv")
