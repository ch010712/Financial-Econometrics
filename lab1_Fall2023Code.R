#lab1_Laboratory Exercise for Group Project I
#Working with Risk-Return Trade offs and Value-at-Risk

#1번
mu.r = 0.05
sd.r = 0.10
x.vals = seq(-2.5, 3.5, length=150)*sd.r + mu.r
plot(x.vals, dnorm(x.vals, mean=mu.r, sd=sd.r), type="l", lwd=2, 
     ylim=c(0, max(dnorm(x.vals, mean=0.025, sd=0.05))),
     col="black", xlab="x", ylab="pdf")

points(x.vals, dnorm(x.vals, mean=0.025, sd=0.05), type="l", lwd=2,
       col="blue", lty="solid")

segments(0.02, 0, 0.02, dnorm(0.02, mean=0.05, sd=0.1), lwd=2)
segments(0.01, 0, 0.01, dnorm(0.01, mean=0.025, sd=0.05), lwd=2, 
         col="blue", lty="dotted")
legend(x="topleft", legend=c("Microsoft", "Starbucks"), lwd=2,
       col=c("black", "blue"), lty=c("solid","dotted"))

##comment##
#Microsoft 주식의 수익률 분포는 평균이 0.05이고 표준편차가 0.1으로 높은 변동성을 나타냅니다.
#따라서 Microsoft 주식은 더 높은 리스크를 가지고 있지만, 더 높은 수익률을 제공합니다. 
#반면에, Starbucks 주식 수익률 분포는 평균이 0.025이고 표준편차가 0.05로 Microsoft에 비해 낮은 변동성을 가집니다.
#따라서 Starbucks 주식은 더 낮은 리스크를 가지며, 더 낮은 예상 수익률을 제공합니다.



#2번
# VaR example
mu.R = 0.04
sd.R = 0.09
w0 = 100000
q.01.R = mu.R + sd.R*qnorm(0.01)
q.05.R = mu.R + sd.R*qnorm(0.05)
VaR.01 = (q.01.R*w0)
VaR.05 = (q.05.R*w0)
VaR.01
VaR.05

#3-1번
mu.r = 0.04
sd.r = 0.09
q.01.R = exp(mu.r + sd.r*qnorm(0.01)) - 1
q.05.R = exp(mu.r + sd.r*qnorm(0.05)) - 1
VaR.01 = (q.01.R*w0)
VaR.05 = (q.05.R*w0)
VaR.01
VaR.05

#3-2번
# 주어진 매개 변수
mu_r = 0.04   #mean
sigma_r = 0.09 #sd
W0 = 100000    #w_0

# 연간 연속 복리 수익률의 평균과 표준 편차 계산
months_in_year = 12
mu_a = months_in_year * mu_r
sigma_a = sqrt(months_in_year) * sigma_r

# 연간 연속 복리 수익률의 1% 및 5% 분위수 계산
alpha_1 = 0.01
alpha_5 = 0.05
q_1 = qnorm(alpha_1, mean = mu_a, sd = sigma_a)
q_5 = qnorm(alpha_5, mean = mu_a, sd = sigma_a)

# 분위수를 단순 수익률로 변환
VaR_1_simple = exp(q_1) - 1
VaR_5_simple = exp(q_5) - 1

# 투자에 대한 연간 VaR 계산
VaR_1_yearly = VaR_1_simple * W0
VaR_5_yearly = VaR_5_simple * W0

# 결과 출력
print(paste("연간 1% VaR:", VaR_1_yearly))
print(paste("연간 5% VaR:", VaR_5_yearly))