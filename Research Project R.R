library(curl)
data <- read.csv("https://raw.githubusercontent.com/polygraph-cool/last-two-minute-report/master/output/all_games.csv")


View(data)

unique(data$review_decision)

extra <- unique(data$review_decision)
extra

data2 <- data

for(i in 6:length(extra)){
  new <- grep(extra[i], data2$review_decision)
  data2 <- data2[-c(new),]
}
nrow(data2)
unique(data2$review_decision)

data2 <- subset(data2, review_decision != "")
