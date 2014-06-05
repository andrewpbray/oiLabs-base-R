setwd("/Users/mine/Dropbox/OI Labs/lab8/data")

goog = read.csv("goog.csv", h = T)

# plot

plot(goog[,-1])

# drop outliers

which.max(goog$pop)

goog_sub = subset(goog, country != "india")

plot(goog_sub[,-1])

# relevel

table(goog_sub$dem)

goog_sub$dem = relevel(goog_sub$dem, ref = "hybrid")

# mlr

m_full = lm(complied ~ requests + pop + hdi + dem + internet + freepress, data = goog_sub)
summary(m_full)

step(m_full, direction = "backward")
step(m_full, direction = "forward")

# new model

m = lm(complied ~ pop + hdi, data = goog_sub)
summary(m)

# diagnostics

# normal residuals
qqnorm(m$residuals, main = "Normal probability plot of residuals")
qqline(m$residuals)

hist(m$residuals, main = "Histogram of residuals")

# constant variance residuals
plot(m$residuals ~ m$fitted.values, main = "Residuals vs. fitted")
abline(h = 0, lty = 3)

plot(abs(m$residuals) ~ m$fitted.values, main = "Absolute value of residuals vs. fitted")

# independent residuals

plot(m$residuals, main = "Residuals vs. order of data collection")

# linear relationship

plot(goog_sub[,-1])

# another model for requests

m_req_full = lm(requests ~  pop + hdi + dem + internet + freepress, data = goog_sub)
step(m_req_full)
m_req = lm(requests ~ pop + hdi, data = goog_sub)
summary(m_req)
