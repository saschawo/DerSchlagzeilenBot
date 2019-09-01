library(XML)
library(RCurl)
library(stringr)

feed <- "<Insert RSS feed URL>"
feedu <- getURL(feed)
feedp <- xmlParse(feedu, error = function (...) {}, useInternalNodes = T, options = NOCDATA)
titles <- xpathSApply(feedp, path = "//title", xmlValue) # the 'path' argument may have to be adapted to the RSS feed entered in line 5

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

# combine all left parts with all right parts
df <- expand.grid(left.parts, right.parts)

# exclude originals
df$use <- c(rep(c(F, rep(T, length(left.parts))),
                length(left.parts)-1), F)
df <- df[df$use,]

# create new headlines
df$new <- paste(df$Var1, df$Var2, sep = ": ")

# exclude already used (tweeted.txt has to exist)
already.used <- scan("tweeted.txt", what = "char", sep = "\n")
df <- df[!(df$new %in% already.used),]

# select one new headline and write it to tweeted.txt
selected <- sample(df$new, 1)
write(selected, file = "tweeted.txt", append = T)

# tweet (Twitter developer account needed, replace with your credentials
library(twitteR)
setup_twitter_oauth(consumer_key = "<KEY>",
                    access_token = "<TOKEN>",
                    consumer_secret = "<SECRET>",
                    access_secret = "<TOKEN SECRET>")

updateStatus(selected)

# @BotSchlagzeilen tweets every 3 hours. This is done by a simple shell script. It looks like this:
#
# while true;
# do
# Rscript recombine-SZ.R 
# sleep 10800
# done
#
# @BotSchlagzeilen runs on a Raspberry Pi 3