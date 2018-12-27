data.df <- read.table("http://people.math.umass.edu/~anna/Stat597AFall2016/rnf6080.dat")

nrow(data.df); ncol(data.df)
colnames(data.df)
data.df[5, 7]
data.df[2, ]

names(data.df) <- c("year", "month", "day", seq(0, 23))
head(data.df)
tail(data.df)

daily = vector(length = nrow(data.df))
for (i in seq(1, nrow(data.df))) {
  daily[i] <- sum(data.df[i, 4 : 27])
}
data.df <- cbind(data.df, daily)
hist(data.df[, "daily"], main="Unfixed data", xlab="Values")

fixed.df <- read.table("http://people.math.umass.edu/~anna/Stat597AFall2016/rnf6080.dat")
fixed.df[fixed.df < 0] <- 0
for (i in seq(1, nrow(fixed.df))) {
  daily[i] <- sum(fixed.df[i, 4 : 27])
}
fixed.df <- cbind(fixed.df, daily)
hist(fixed.df[, "daily"], main="Fixed data", xlab="Values")


v <- c("4", "8", "15", "16", "23", "42")
max(v)
sort(v)
sum(v)


v2 <- c("5",7,12)
v2[2] + v2[3]

df3 <- data.frame(z1="5",z2=7,z3=12)
df3[1,2] + df3[1,3]

l4 <- list(z1="6", z2=42, z3="49", z4=126)
l4[[2]] + l4[[4]]
l4[2] + l4[4]
