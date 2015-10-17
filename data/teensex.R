library(lme4)
library(gee)
library(geepack)
library(lubridate)

df = read.csv("teensex.csv")
df$today = as.Date(df$today, "%d-%B-%y")
df = df[!is.na(df$drgalcoh),]
df$wday = as.factor(wday(df$today))

# Logistic Regression
m_log   = glm(sx24hrs ~ drgalcoh, family = "binomial", data = df)
exp(coef(summary(m_log)))

# Mixed Effects
m_mixed = glmer(sx24hrs ~ drgalcoh + (1|eid), data = df, family = "binomial")
exp(coef(summary(m_mixed)))

# GEE w/ independence
m_gee_ind = gee(sx24hrs ~ drgalcoh, id = eid, data = df,
                corstr = "independence", family = "binomial")
exp(coef(summary(m_gee_ind)))

m_gee_ind2 = geeglm(sx24hrs ~ drgalcoh, id = eid, data = df,
                    corstr = "independence", family = "binomial")
exp(coef(summary(m_gee_ind2)))

# GEE w/ exchangeability
m_gee_exc = gee(sx24hrs ~ drgalcoh, id = eid, data = df,
                corstr = "exchangeable", family = "binomial")
exp(coef(summary(m_gee_exc)))

m_gee_exc2 = geeglm(sx24hrs ~ drgalcoh, id = eid, data = df,
                    corstr = "independence", family = "binomial")
exp(coef(summary(m_gee_ind2)))

# Add wday
m_gee_wday_exc2 = gee(sx24hrs ~ drgalcoh + wday, id = eid, data = df,
                         corstr = "exchangeable", family = "binomial")