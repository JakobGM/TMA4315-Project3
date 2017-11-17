library(ggplot2)
library(GGally)
library(devtools)
library(MASS)
library(Matrix)
library(faraway)
library(sjPlot)
library(ggpubr)
data(jsp, package = "faraway")
sjsp <- jsp[jsp$year == 0, ]
sjsp <- sjsp[, -c(6, 7, 9)]  # removing class, id, english and year as they are not used
school= sjsp$school
class = sjsp$class
gender = sjsp$gender
social = sjsp$social
raven = sjsp$raven
math = sjsp$math

##oppgave a
ggpairs(sjsp, columns=c(4,5,6), mapping=aes(col=gender),legend=1)
fit = lm(math~raven+gender)
summary(fit)

##oppgave b
library(lme4)
fitRIa <- lmer(math ~ raven + gender + (1 | school), data = sjsp)
summary(fitRIa)
coefs = data.frame(coef(summary(fitRIa)))
pvalue_raven <- 2 * pnorm(abs(coefs$t.value[3]), lower.tail = FALSE)
confinterval = confint(fitRIa)
femaleconf = confinterval[5,]

##oppgave c
fitRI <- lmer(math ~ raven + (1 | school), data = sjsp)
summary(fitRI)
randomvar = as.data.frame(VarCorr(fitRI))[,4]
corr = randomvar[1]/(randomvar[1]+randomvar[2])



##oppgave d
fitRIb <- lmer(math ~ raven + social + (1 | school), data = sjsp)
anova(fitRI, fitRIb)

fitRIS <- lmer(math ~ raven + (1 + raven | school), data = sjsp)
gg1 <- sjp.lmer(fitRIS, type = "rs.ri", prnt.plot = FALSE)
gg2 <- sjp.lmer(fitRIS, sort.est = "(Intercept)", prnt.plot = FALSE, facet.grid = FALSE,
                title = names(ranef(fitRIS)$school), geom.size = 1.5)
ggarrange(gg1$plot[[1]], gg2$plot.list[[1]], gg2$plot.list[[2]] + labs(x = ""),
          ncol = 3, widths = c(2, 1, 1))
