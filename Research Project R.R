library(curl)
data <- read.csv("https://raw.githubusercontent.com/polygraph-cool/last-two-minute-report/master/output/all_games.csv")


View(data)

unique(data$review_decision)

extra <- unique(data$review_decision)
extra

for(i in 6:length(extra)){
  new <- grep(extra[i], data$review_decision)
  print(new)
}
