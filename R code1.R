install.packages("xlsx")
library("xlsx")
data <- read.xlsx("transactions.xlsx", sheetIndex = 1)
print(data)


data <- read.csv("transactions.csv")
print(head(data))

df <- data.frame(data)
df$failure <- (df$t-df$success)/df$t
print(head(df))
library("ggpubr")
model <- lm(failure~mid+pmt+pg+bank, data = df)


res1 <-cor.test(df$failure, df$mid,  method = "spearman")
res2


m_failure <- model.matrix(~ failure - 1, df)
m_bank <- model.matrix(~ bank - 1, df)
m_mid <- model.matrix(~ mid - 1, df)
m_pmt <- model.matrix(~ pmt - 1, df)
m_pg <- model.matrix(~ pg - 1, df)
cor(m_failure, m_bank)
cor(m_failure, m_pmt)
corpg <- data.frame(cor(m_failure, m_pg))


logitMod <- glm(failure ~ pmt + pg + mid, data=df, family=logistic)

data <- read.csv("transactions.csv")
print(head(data))

df <- data.frame(data)

df$failure <- (df$t-df$success)/df$t
print(head(df))
logitMod <- glm(failure ~ pmt + pg + bank +sub_type, data=df)
print(summary(logitMod))
summ <- data.frame(summary(logitMod))
write.csv(summ, "summarytable.csv")

df$hr <- substr(df$hr, 12, 14)
df$hr

df
cor(m_V1, m_V4) 

library(corrplot)
corrplot(, method="circle")


# ggplot2 examples
library(ggplot2)
qplot(failure, hr, data=df)


qplot(failure, hr, data=df, geom=c("boxplot", "jitter"))



# Scatterplot of mpg vs. hp for each combination of gears and cylinders
# in each facet, transmittion type is represented by shape and color
qplot(hp, mpg, data=mtcars, shape=am, color=am,
   facets=gear~cyl, size=I(3),
   xlab="Horsepower", ylab="Miles per Gallon")

# Separate regressions of mpg on weight for each number of cylinders
qplot(wt, mpg, data=mtcars, geom=c("point", "smooth"),
   method="lm", formula=y~x, color=cyl,
   main="Regression of MPG on Weight",
   xlab="Weight", ylab="Miles per Gallon")

# Boxplots of mpg by number of gears
# observations (points) are overlayed and jittered
qplot(gear, mpg, data=mtcars, geom=c("boxplot", "jitter"))


avgdata=data.frame(value=apply(df,2,average))
sumdata$key=rownames(sumdata)
ggplot(data=sumdata, aes(x=key, y=value, fill=key)) +
geom_bar(colour="black", stat="identity")

plotdata <- data.frame(df$hr, df$failure)
head(plotdata)
boxplot(df.failure ~ df.hr , data = plotdata)


head(df)
merchants <- data.frame(df$mid, df$failure)
head(merchants)
library(data.table)
m_data <- setDT(merchants)[, mean(df.failure), by=df.mid]
m_data[, barplot(V1, names=df.mid, main="BARPLOT", xlab="Merchants", ylab="failures")]