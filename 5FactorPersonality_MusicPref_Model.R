library(readr)
library(psych)

hist(psych$ROCK, main = 'Minutes Spent Listening to Rock Music', xlab = 'Minutes spent listening')
hist(psych$Age, main = 'Age of Participants N=11', xlab = 'Age')
plot(as.factor(psych$Sex), main = 'Sex of Participants N=11', xlab = 'Sex')


psyO.df <- with(psych, cbind(OP1...4, OP1...5))
psyN.df <- with(psych, cbind(N1, N2))
psych.df <- data.frame(psyO.df , psyN.df)
alpha(psyO.df)
alpha(psyN.df)


psych$open_avg <- rowMeans(psyO.df, na.rm = T)
psych$neu_avg <- rowMeans(psyN.df, na.rm = T)
hist(psych$open_avg, main = 'Openness to Experience Histogram', xlab = 'average openness to experience rating')
hist(psych$neu_avg, main = 'Neuroticism Histogram', xlab = 'average neuroticism rating')

mean(psych$ROCK)
sd(psych$ROCK)
range(psych$ROCK)

mean(psych$open_avg)
sd(psych$open_avg)
range(psych$open_avg)


mean(psych$neu_avg)
sd(psych$neu_avg)
range(psych$neu_avg)

mod1 <- lm(ROCK ~ open_avg, data = psych)
plot(ROCK ~ open_avg, data = psych, main = 'Openness to experience avg vs minutes listening to rock music', ylab = 'minutes spent listening to rock music', xlab = 'Openness to Experience avg rating')
abline(mod1, col = 'blue', lwd = 5)
coef(mod1)

bucket <- array() # creates a place to store your resampled slopes.
for(i in c(1:1000)){
  boot.dat <- psych[sample(1:nrow(psych), nrow(psych), replace = T),]
  boot.mod <- lm(ROCK ~ open_avg, data = boot.dat) # 
  bucket[i] <- coef(boot.mod)[2] # saves the 1st slope (2nd term in your model)
}
bucket[bucket < -200] <- NA
bucket <- na.omit(bucket)

hist(bucket)
sd(bucket)
sum(bucket > 0)/1000

mod2 <- lm(ROCK ~ neu_avg, data = psych)
plot(ROCK ~ neu_avg, data = psych, main = 'Neuroticism avg vs minutes listening to rock music', ylab = 'minutes spent listening to rock music', xlab = 'Neuroticism avg rating')
abline(mod2, col = 'blue', lwd = 5)
coef(mod2)

bucket <- array() # creates a place to store your resampled slopes.
for(i in c(1:1000)){
  boot.dat <- psych[sample(1:nrow(psych), nrow(psych), replace = T),]
  boot.mod <- lm(ROCK ~ neu_avg, data = boot.dat) # 
  bucket[i] <- coef(boot.mod)[2] # saves the 1st slope (2nd term in your model)
}
bucket[bucket < -200] <- NA
bucket <- na.omit(bucket)


hist(bucket)
sd(bucket)
sum(bucket > 0)/1000

mod3 <- lm(ROCK ~ neu_avg) + open_avg, data = psych)
summary(mod3)

summary(mod1)
summary(mod2)

coef(mod)[2] + 1.96 * sd(bucket) # upper limit for this 95% confidence interval
coef(mod)[2] -1.96 * sd(bucket) # lower limit for this 95% confidence interval
abline(v = coef(mod)[2] + 1.96 * sd(bucket), lwd = 5, lty = 2, col = 'red')
abline(v = coef(mod)[2] - 1.96 * sd(bucket), lwd = 5, lty = 2, col = 'red')
