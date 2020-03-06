#
par(mfrow=c(2,3))
curve(dchisq(x,1), 0.01,10, main="chi-deux(1)", col="pink")
curve(dchisq(x,2), 0.01,10, main="chi-deux(2)", col="red")
curve(dchisq(x,3), 0.01,10, main="chi-deux(3)", col="yellow")
curve(dchisq(x,5), 0.01,10, main="chi-deux(4)", col="black")
curve(dchisq(x,10), 0.01,20, main="chi-deux(10)", col="green")
curve(dchisq(x,20), 0.01,60, main="chi-deux(20)", col="blue")
