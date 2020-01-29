
# 1
# a)
s = 6
n = 25
alpha = 1-0.95
z = -qnorm(alpha/2)
pHat = 11/25
ME = z* sqrt((pHat*(1-pHat)/n))

pHat-ME
pHat+ME

