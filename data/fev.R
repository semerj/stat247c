library(foreign)
library(geepack)
library(dplyr)

df = data.frame(read.dta("fev.dta"))

ind = geeglm(lnfev ~ initage + lnheight + agechange + lnheightchange,
             id = childid, data = df,
             corstr = "independence", family = "gaussian")
summary(ind)$coef

exc = geeglm(lnfev ~ initage + lnheight + agechange + lnheightchange,
             id = childid, data = df,
             corstr = "exchangeable", family = "gaussian")
summary(exc)$coef

# needs the time structure
df = df %>% group_by(childid) %>% mutate(time = row_number())
ar = geeglm(lnfev ~ initage + lnheight + agechange + lnheightchange,
            id = childid, data = df, waves = year,
            corstr = "ar1", family = "gaussian")
#summary(ar)$coef

data.frame("Ind"=summary(ind)$coef$Estimate,
           "Exc"=summary(exc)$coef$Estimate,
           "Ar" =summary(ar)$coef$Estimate,
           row.names=row.names(summary(ind)$coef))

#unstr = geeglm(lnfev ~ initage + lnheight + agechange + lnheightchange,
#               id = childid, data = df,
#               corstr = "unstructured", family = "gaussian")
#summary(unstr)$coef
