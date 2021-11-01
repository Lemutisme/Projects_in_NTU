library(MixAll)
library(corrplot)
library(dplyr) 
library(pscl)
library(sandwich)
library(lmtest)
library(MASS)
library(pscl)

## Data preparing
data(DebTrivedi)
head(DebTrivedi)
summary(DebTrivedi)
glimpse(DebTrivedi)
par(mfrow=c(1,1))

## Plot
plot(table(DebTrivedi$hosp),
     xlab = "Number of hospital stay",
     ylab = "Frequency")
plot(sort(DebTrivedi$hosp))
plot(DebTrivedi$numchron, DebTrivedi$hosp)

## Cor
df <- DebTrivedi[,c(1:6, 8,11,15,16)]
M = cor(df)
corrplot(M, method = 'number')
mean(dt$hosp); var(dt$hosp)

dt <- DebTrivedi[, c(6, 7:19)]
head(dt)
table(dt$hosp)

## Pois test
m_pois <- glm(hosp ~ ., data = dt, family = poisson)
summary(m_pois)
exp(coef(m_pois))

## Quasipoisson & nb
m_qp <- glm(hosp ~ ., data = dt, family =quasipoisson)
summary(m_qp)
exp(coef(m_qp))

m_nb <- MASS::glm.nb(hosp ~ ., data = dt)
summary(m_nb)
exp(coef(m_nb))

lmtest::lrtest(m_qp, m_nb)

#zero-inflated Poisson & negative binomial regression model:
m_zi0 <- zeroinfl(hosp ~ ., data = dt)
summary(m_zi0)

m_zi_negbin <- zeroinfl(hosp ~ ., data = dt, dist="negbin")
summary(m_zi_negbin)

#Compare the AIC values of different models to choose the appropriate model.
lmtest::lrtest(m_zi0, m_zi_negbin)




