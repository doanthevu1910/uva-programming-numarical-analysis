x  <- seq(-2, 2, 0.05)
y1 <- pnorm(x)
y2 <- pnorm(x, 1, 1)
plot(x, y1, type="l", col = "red")
lines(x,y2,col="green")
points(x,y1,col='blue')

library(ggplot2)

ggplot() +
  geom_line(mapping = aes(x=x, y=y1), color = "red", linetype = 2) +
  geom_line(mapping = aes(x=x, y=y2), color = "blue", linetype = 4) +
  geom_point(mapping = aes(x=1:20, y=1:20), shape = 1:20, color = 1:20)

dev.off()

