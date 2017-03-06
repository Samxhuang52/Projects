
##Q4##
x <- read.csv("nobel_chocolate.txt", skip=0, header=TRUE)
y <- data.frame(x$nobel_rate,x$chocolate)
##construct scatterplot##
plot(x$chocolate, 
     x$nobel_rate, type="p", 
     main="Nobel vs Chocolate", 
     xlab="Chocolate Consumption", 
     ylab="Nobel Laureates per capita",
     col = "red", lwd = 1)
subset(x, x$country=='Sweden')
text(6, 30, labels="Sweden")
cor(y)
text(6, 25, labels="correlation=0.8010949")

###Q7###
##construct liner regression##
lm.result <- lm(y$x.nobel_rate ~ y$x.chocolate, data=y)
lm.result
##see the test result##
summary.lm(lm.result)
##add trend line to the scatterplot
abline(coef(lm.result), col="firebrick", lwd=2)

##residual and nomal Q-Q plot test##
par(mfrow=c(2,1))

#check linearity#
plot(x$chocolate, lm.result$residuals, pch=16, las=TRUE, main="Residual Plot",xlab="Chocolate Consumption (kg/yr/capita)",ylab="residual",col = "black", lwd = 1)

#check normality#
qqnorm(lm.result$residuals, pch=16, las=TRUE, main="Normal Q-Q Plot")
qqline(lm.result$residuals)

