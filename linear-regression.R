library(ISLR)

# Lineaarinen regressio ja koneoppinen ----

## Yksinkertainen regressiomalli ----
fix(Auto)
lm.fit = lm(mpg~horsepower, Auto)
summary(lm.fit)

### Ennustetaan ----
predict(lm.fit, data.frame(horsepower=c(30,50,98,150)),
        interval = "confidence")

### Plotataan kaksi muuttujaa ----
plot(Auto$horsepower,Auto$mpg)
### Lis‰t‰‰n regressiok‰yr‰ ----
abline(lm.fit)

### Plotaan malli samaan kuvaan ----
par(mfrow=c(2,2))
plot(lm.fit)

### Scatterplot ----
#### Perus Scatterplot ----
pairs(Auto)

#### Fancy Scatterplot ----
#install.packages("psych")
library(psych)
pairs.panels(Auto)
pairs.panels(Auto[c("mpg", "cylinders", "displacement", "horsepower", "weight")])
pairs.panels(Auto[c("mpg", "horsepower")])

### Korrelaatio ----
df <- subset(Auto, select = -name)
cor(df)

library(dplyr)
cor(Auto %>% select(-name))

## Monimuuttujaregressio ----
m <- lm(mpg~.-name, data = Auto)
summary(m)
plot(m)

par(mfrow=c(2,2))
plot(m)

### Interaktiomuuttujia ----
m2 <- lm(mpg~, -name, data = Auto)