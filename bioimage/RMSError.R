source('~/R/my_function.R', echo=TRUE)
clearAll()


training = read.table("~/R/bioimage/training.txt")
evaluation = read.table("~/R/bioimage/evaluation.txt")
x_test = evaluation$V1
y_test = evaluation$V2

x = training$V1
y = training$V2

RMStraining = c()
RMStest = c()

for(m in 1:10){
    X = matrix(nrow = length(x), ncol = m)
    for(i in 1:m){
        X[, i] = x^(i-1)
    }
    
    beta_hat = solve((t(X) %*% X)) %*% t(X) %*% y
    
    
    f = function(x){
        ans = 0
        for(i in 1:m){
            ans = ans + beta_hat[i, 1] * x^(i-1)   
        }
        return(ans)
    }
    
    y_hat = c()
    
    for(i in 1:length(x_test)){
        y_hat[i] = f(x_test[i])
    }
    
    RMStraining[m] = ( sum( (y - y_hat)^2 ) / length(x) )^0.5
    RMStest[m] = ( sum( (y_test - y_hat)^2 ) / length(x_test) )^0.5
}

jisuu = seq(1, 10)
plot(jisuu, RMStraining, pch = 16, lwd = 3, col = "black", xlab = "", ylab = "")
lines(jisuu, RMStraining, lty = 1, lwd = 3, col = "black")

par(new = TRUE)
plot(jisuu, RMStest, pch = 16, lwd = 3, col = "red", xlab = "次数", ylab = "RMS")
lines(jisuu, RMStest, lty = 1, lwd = 3, col = "red")
grid()

legend("topright", c("訓練誤差", "テスト誤差"), lty = c(1, 1),
       pch = c(16, 16), col = c("black", "red"), lwd = 2,
       bg = "white", cex = 1.3)