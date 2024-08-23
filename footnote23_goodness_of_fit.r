# footnote 24 (section  4.2)
load('data/bargaining1.RData')
data = fdata2
ps = c()
coefs = c()
adj_r_sq = c()
ids = unique(data$Subject)
for (i in 1:length(unique(data$Subject))){
  temp = filter(data,Subject == ids[i])
  b = summary(lm(price2 ~ rt + price1,temp))
  ps[i] = b$coefficients[2,4]
  coefs[i] = b$coefficients[2,1]
  adj_r_sq[i] =  b$adj.r.squared
} 
mean(adj_r_sq) # 0.5273385

temp = filter(data,Subject == ids[1])
b = summary(lm(price2 ~ rt + price1,temp))
ps[i] = b$coefficients[2,4]
coefs[i] = b$coefficients[2,1]
adj_r_sq[i] =  b$adj.r.squared