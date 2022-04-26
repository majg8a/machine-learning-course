df <- read.csv("C:/Users/Marcelo/machine learning course with python and R/Data Files/1. ST Academy - Crash course and Regression files/House_Price.csv",header = TRUE)
View(df)

str(df)

summary(df)
hist(df$crime_rate)
pairs(~price+crime_rate+n_hot_rooms+rainfall, data =df)
barplot(table(df$bus_ter))

# Observations
# n_hot_rooms and rainfall has outliers
# n_hos_beds has missing values
# bus_term is a useless variable


quantile(df$n_hot_rooms,0.99)
uv = quantile(df$n_hot_rooms,0.99) * 3
df$n_hot_rooms[df$n_hot_rooms>uv]<- uv

summary(df$n_hot_rooms)

lv = 0.3 * quantile(df$rainfall,0.01)
df$rainfall[df$rainfall<lv]<- lv

summary(df$rainfall)
mean(df$n_hos_beds,na.rm = TRUE)
which(is.na(df$n_hos_beds))
df$n_hos_beds[is.na(df$n_hos_beds)]<- mean(df$n_hos_beds,na.rm = TRUE)

summary(df$n_hos_beds)

pairs(~price+crime_rate,data=df)
plot(df$price,df$crime_rate)
df$crime_rate<- log(1+df$crime_rate)
plot(df$price,df$crime_rate)

df$avg_dist = (df$dist1 + df$dist2 + df$dist3 + df$dist4)/4
df2 <- df[,-7:-10]
df2 <- df2[,-14]

install.packages('dummies')

df2 <- dummy.data.frame(df2)

df2 <- df2[,-9]
df2 <- df2[,-14]
  
cor(df2)
round(cor(df2),2)

df2 <- df2[,-16]

simple_model <- lm(price~room_num,data=df2)

summary(simple_model)

plot(df$room_num,df$price)
abline(simple_model)

multiple_model <- lm(price~.,data = df2)
summary(multiple_model)

install.packages("caTools")

set.seed(0)
split = sample.split(df2,SplitRatio =  0.8)
training_set = subset(df2,split == TRUE)
test_set = subset(df2,split == FALSE)

lm_a = lm(price~.,data=training_set)

train_a = predict(lm_a,training_set)
test_a =predict(lm_a,test_set)

mean((training_set$price-train_a)^2)
mean((test_set$price-test_a)^2)

install.packages("leaps")

lm_best = regsubsets(price~.,data=df2,nvmax = 15)
summary(lm_best)

summary(lm_best)$adjr2

which.max(summary(lm_best)$adjr2)

coef(lm_best,8)

lm_forward = regsubsets(price~.,data=df2,nvmax = 15,method= "forward")
summary(lm_forward)
which.max(summary(lm_forward)$adjr2)

install.packages("glmnet")

x = model.matrix(price~.,data=df2)[,-1]
y = df2$price

grid = 10 * seq(10,0,length=270)

grid

lm_ridge = glmnet(x,y, alpha = 0, lambda = grid)
summary(lm_ridge)

cv_fit = cv.glmnet(x,y,alpha= 0,lambda = grid)
plot(cv_fit)

opt_lambda = cv_fit$lambda.min
tss =sum((y-mean(y))^2)

y_a = predict(lm_ridge,s=opt_lambda,newx = x)

rss =sum((y_a-y)^2)

rsq = 1 -rss/tss

lm_lasso = glmnet(x,y, alpha = 1, lambda = grid)

y_a_lasso = predict(lm_lasso,s=opt_lambda,newx = x)

rss_lasso = sum((y_a_lasso-y)^2)

rsq_lasso = 1 -rss_lasso/tss