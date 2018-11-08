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



levels(data2$home)
levels(data2$committing_team)

grep("ACL", data2$committing_team)
grep("AT)", data2$committing_team)
grep("CHi", data2$committing_team)
grep("COS", data2$committing_team)
grep("LA)", data2$committing_team)
grep("NKY", data2$committing_team)
grep("WSH", data2$committing_team)

error <- c(grep("ACL", data2$committing_team), grep("AT)", data2$committing_team), grep("CHi", data2$committing_team), grep("COS", data2$committing_team), grep("LA)", data2$committing_team), grep("NKY", data2$committing_team), grep("WSH", data2$committing_team))

error

e1 <- error[1]-5
e2 <- error[1]
data2[e1:e2,]

data2[error[1],]$committing_team <- "CLE"
data2[error[2],]$committing_team <- "ATL"

data2[error[3],]$committing_team <- "CHI"

data2[error[4],]$committing_team <- ""

data2[error[5],]$committing_team <- "LAL"

data2[error[6],]$committing_team <- "NYK"

data2[error[7],]$committing_team <- "WAS"

data2[data2 == ""] <- NA








## Statistical Testing
```{r}
data3 <- c(data2$review_decision, data2$away, data2$home, data2$committing_team, data2$disadvantaged_team)



data3 <- data2[,c("review_decision", "away", "home", "committing_team", "disadvantaged_team")]

data3 <- na.omit(data3)

levels(data3$home)
levels(data3$committing_team)

data3$committing_team <- droplevels(data3$committing_team)

levels(data3$committing_team)

data3$match <- ifelse(data3$home == data3$committing_team, 1, 0)

# Home Team commits foul
data4 <- subset(data3, match == 1)

(sum(data4$review_decision == "IC") + sum(data4$review_decision == "INC"))

(sum(data4$review_decision == "CC") + sum(data4$review_decision == "CNC"))

# Away Team commits foul
data5 <- subset(data3, match == 0)

(sum(data5$review_decision == "IC") + sum(data5$review_decision == "INC"))

(sum(data5$review_decision == "CC") + sum(data5$review_decision == "CNC"))


# Diff in prop

# make table

home.team <- (sum(data4$review_decision == "IC") + sum(data4$review_decision == "INC"))


```
