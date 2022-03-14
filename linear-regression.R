library(ISLR)
library(MASS)


# Lineaarinen regressio ----

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

### Testataan histogrammia kaikille muuttujille ----
par(mfrow=c(2,4))
lapply(df, hist, main = c("Test1", "Test2"))

library(dplyr)
cor(Auto %>% select(-name))

## Monimuuttujaregressio ----
m <- lm(mpg~.-name, data = Auto)
summary(m)
plot(m)

par(mfrow=c(2,2))
plot(m)

### Interaktiomuuttujia ----
m2 <- lm(mpg~. -name + cylinders*displacement, data = Auto)
summary(m2)

m3 <- lm(mpg~. -name + cylinders:displacement, data = Auto)
summary(m3)

### Transformaatio ----
df.t <- Auto %>% mutate(l_horsepower = log(horsepower))
m4 <- lm(mpg~. - name, data = df.t)
summary(m4)
pairs.panels(df.t)

## Carseats ----
View(Carseats)
m.car <- lm(Sales~Price + Urban + US, data = Carseats)
summary(m.car)

m.car.new <- lm(Sales~Price + US, data=Carseats)
summary(m.car.new)

### Anova ----
anova(m.car, m.car.new)

### Luottamusv‰li ----
confint(m.car.new)

### Outlierit ----
par(mfrow=c(2,2))
plot(m.car.new)

## Malli ilman vakiota ----
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)

m.0 <- lm(y~x+0)
par(mfrow=c(2,2))
plot(m.0)
summary(m.0)

### Simuloitua dataa ----
set.seed(1)
x <- rnorm(n = 100, mean = 0, sd = 1)
eps <- rnorm(n = 100, mean = 0, sd = 1/4)
par(mfrow=c(1,2))
plot(x)
plot(eps)

y = -1 + 0.5*x + eps
m.sim <- lm(y ~ x)
summary(m.sim)

#### Plotataan malli ----
plot(x, y)
abline(m.sim, col = "navyblue", lwd=3)
legend(-2.5, 0, legend = "Simuloitu\nmalli")

#### Mallin neliˆmuunnos ----
m.sim2 <- lm(y ~ poly(x, 2))
summary(m.sim2)

anova(m.sim, m.sim2)

## Kollineaarisuus ----
set.seed(1)
x1 = runif(100)
x2 = 0.5*x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)

cor(x1, x2)
plot(x1, x2)

m.kol <- lm(y ~ x1 + x2)
summary(m.kol)

m.kol2 <- lm(y ~ x1)
summary(m.kol2)

m.kol3 <- lm(y ~ x2)
summary(m.kol3)


## Boston-data ----
View(Boston)

m.boston <- lm(crim ~ ., data = Boston)
summary(m.boston)


# Regressio ja koneoppiminen ----
n <- dplyr::count(Boston)
set.seed(808)
t <- sample(1:n$n, 0.7*n$n)

df_train <- Boston[t,]
df_test <- Boston[-t,]

## Regression Tree ----
library(rpart)

m.tree <- rpart(crim ~ ., data = df_train)
m.tree

### Visualisoidaan puu ----
library(rpart.plot)
rpart.plot(m.tree, digits = 4, fallen.leaves = TRUE, type = 4, extra = 101)


## Model Tree ----
library(Cubist)

### Jaa data testi- ja treenidataan ----
n <- dplyr::count(Boston)
set.seed(808)
t <- sample(1:n$n, 0.7*n$n)
df_train <- Boston[t,]
df_test <- Boston[-t,]

### Treenataan malli ----
# HUOM! Treenidatasta pit‰‰ poistaa riippuva muuttuja crim,
# joka annetaan omana vektorinaan funktiolle
m.mtree <- cubist(df_train[-1], df_train$crim)

m.mtree

summary(m.mtree)

### Ennustetaan testidatalla ----
p.mtree <- predict(m.mtree, df_test)

summary(p.mtree)

cor(p.mtree, df_test$crim)
# 0,799 eli kohtuullisen korkea korrelaatio


