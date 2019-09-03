# Last modification: 19/09/03

library(XML)
library(RCurl)
library(stringr)

# Source 1 ----------------------------------------------------------------

feed <- "<RSS feed 1>"
feedu <- getURL(feed)
feedp <- xmlParse(feedu, error = function (...) {}, useInternalNodes = T, options = NOCDATA)
titles <- xpathSApply(feedp, path = "//title", xmlValue)

# Source 2 ----------------------------------------------------------------

feed2 <- "<RSS feed 2"
feedu2 <- getURL(feed2)
feedp2 <- xmlParse(feedu2, error = function (...) {}, useInternalNodes = T, options = NOCDATA)
titles2 <- xpathSApply(feedp2, path = "//title", xmlValue)

# Integrating sources -----------------------------------------------------

titles <- c(titles, titles2)

# titles with exactly one colon
titles <- titles[str_count(titles, fixed(":")) == 1]

split.titles <- strsplit(titles, ":", fixed = T)

# only titles with more than one word on both sides
lens.left <- sapply(split.titles, FUN = function (x) {
  x <- str_trim(x[1])
  str_count(x, "[[:space:]]") + 1
})
lens.right <- sapply(split.titles, FUN = function (x) {
  x <- str_trim(x[2])
  str_count(x, "[[:space:]]") + 1
})
split.titles <- split.titles[lens.left > 1 & lens.right > 1]

left.parts <- str_trim(sapply(split.titles, FUN = function (x) x[1]))
right.parts <- str_trim(sapply(split.titles, FUN = function (x) x[2]))

df <- expand.grid(left.parts, right.parts)

# exclude originals
df$use <- c(rep(c(F, rep(T, length(left.parts))),
                length(left.parts)-1), F)

df <- df[df$use,]

df$new <- paste(df$Var1, df$Var2, sep = ": ")

# exclude already used
already.used <- scan("tweeted.txt", what = "char", sep = "\n")
already.used.left <- scan("tweeted-left.txt", what = "char", sep = "\n")
already.used.right <- scan("tweeted-right.txt", what = "char", sep = "\n")

df <- df[!(df$new %in% already.used),]
df <- df[!(df$Var1 %in% already.used.left),]
df <- df[!(df$Var2 %in% already.used.right),]

selected.row <- sample(1:nrow(df), 1)
selected.left <- df[selected.row, "Var1"]
selected.right <- df[selected.row, "Var2"]
selected.tweet <- df[selected.row, "new"]
write(selected.tweet, file = "tweeted.txt", append = T)
write(selected.left, file = "tweeted-left.txt", append = T)
write(selected.right, file = "tweeted-right.txt", append = T)

library(twitteR)
setup_twitter_oauth(consumer_key = "<key>",
                    access_token = "<token>",
                    consumer_secret = "<secret>",
                    access_secret = "<token secret>")

updateStatus(selected.tweet)