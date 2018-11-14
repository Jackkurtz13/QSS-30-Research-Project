

```{r}
library(curl)
data <- read.csv("https://raw.githubusercontent.com/polygraph-cool/last-two-minute-report/master/output/all_games.csv")


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

error[1]

data2[error[3],23]
data2[2601,23]

unique(data2$committing_team)

#e1 <- error[1]-5
#e2 <- error[1]
#data2[e1:e2,]

data2[error[1],]$committing_team <- "CLE"
data2[error[2],]$committing_team <- "ATL"

data2[error[3],]$committing_team <- "CHI"

data2[error[4],]$committing_team <- ""

data2[error[5],]$committing_team <- "LAL"

data2[error[6],]$committing_team <- "NYK"

data2[error[7],]$committing_team <- "WAS"

data2[data2 == ""] <- NA

levels(data2$committing_team)
```



## Statistical Testing
#Percent breakdown of 4 call types by home and away
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

home.ic <- sum(data4$review_decision == "IC")/nrow(data4)
home.ic*100

home.inc <- sum(data4$review_decision == "INC")/nrow(data4)
home.inc*100

home.cc <- sum(data4$review_decision == "CC")/nrow(data4)
home.cc*100

home.cnc <- sum(data4$review_decision == "CNC")/nrow(data4)
home.cnc*100


# Away Team commits foul
data5 <- subset(data3, match == 0)

away.ic <- sum(data5$review_decision == "IC")/nrow(data5)
away.ic*100

away.inc <- sum(data5$review_decision == "INC")/nrow(data5)
away.inc*100

away.cc <- sum(data5$review_decision == "CC")/nrow(data5)
away.cc*100

away.cnc <- sum(data5$review_decision == "CNC")/nrow(data5)
away.cnc*100

```

# make table for chi square & diff in prop test
# Home vs. Away for All Calls & All Games
```{r}

#home team benefits from incorrect call 
home.team <- (sum(data4$review_decision == "IC") + sum(data4$review_decision == "INC"))
home.team/nrow(data4)

#away team benefits from incorrect call
away.team <- (sum(data5$review_decision == "IC") + sum(data5$review_decision == "INC"))
away.team/nrow(data5)
nrow(data5)

test_table <- rbind(home.team, away.team) 
test_table

#chi-square test
chisq.test(test_table)

# proportion test [c(home, away)]
result <- prop.test(x = c(home.team, away.team), n = c(nrow(data4), nrow(data5)))
result
```




# Distinguishing between "Calls" and "Non-Calls" for Overall Dataset

# Home vs Away Team (in Just CALLS)
```{r}

home.team <- sum(data4$review_decision == "IC")
total.home <- sum(data4$review_decision == "IC") + sum(data4$review_decision == "CC")

away.team <- sum(data5$review_decision == "IC")
total.away <- sum(data5$review_decision == "IC") + sum(data5$review_decision == "CC")


#chi-square test
chisq.test(test_table)

# proportion test [c(home, away)]
calls <- prop.test(x = c(home.team, away.team), n = c(total.home, total.away))
calls
```


# Home vs Away Team (in Just NON-CALLS)
```{r}

home.team <- sum(data4$review_decision == "INC")
total.home <- sum(data4$review_decision == "INC") + sum(data4$review_decision == "CNC")

away.team <- sum(data5$review_decision == "INC")
total.away <- sum(data5$review_decision == "INC") + sum(data5$review_decision == "CNC")


#chi-square test
chisq.test(test_table)

# proportion test [c(home, away)]
calls <- prop.test(x = c(home.team, away.team), n = c(total.home, total.away))
calls
```




## Post-Season

#Making Post-Season Dataset
```{r}
data2$year <- substr(data2$date, start = 1, stop = 4)

data2$month <- substr(data2$date, start = 5, stop = 8)
data2$month <- as.integer(data2$month)

playoffs15 <- subset(data2, year == 2015 & month >= 418 & month < 701)
playoffs16 <- subset(data2, year == 2016 & month >= 416 & month < 701)
playoffs17 <- subset(data2, year == 2017 & month >= 415 & month < 701)
playoffs18 <- subset(data2, year == 2018 & month >= 414 & month < 701)

nrow(playoffs15)
nrow(playoffs16)
nrow(playoffs17)
nrow(playoffs18)

sum(nrow(playoffs15) + nrow(playoffs16) + nrow(playoffs17) + nrow(playoffs18))

playoffs <- rbind(playoffs15, playoffs16, playoffs17, playoffs18)




playoffs2 <- playoffs[,c("review_decision", "away", "home", "committing_team", "disadvantaged_team")]

playoffs2 <- na.omit(playoffs2)


playoffs2$committing_team <- droplevels(playoffs2$committing_team)
playoffs2$home <- droplevels(playoffs2$home)

playoffs2$match <- ifelse(as.character(playoffs2$home) == as.character(playoffs2$committing_team), 1, 0)


# Home Team commits foul
playoffs.home <- subset(playoffs2, match == 1)


# Away Team commits foul
playoffs.away <- subset(playoffs2, match == 0)

```

# Tests for Playoffs
# Home vs. Away for All Calls in Playoffs
```{r}
#home team benefits from incorrect call 
home.team <- (sum(playoffs.home$review_decision == "IC") + sum(playoffs.home$review_decision == "INC"))
home.team/nrow(playoffs.home)
nrow(playoffs.home)

#away team benefits from incorrect call
away.team <- (sum(playoffs.away$review_decision == "IC") + sum(playoffs.away$review_decision == "INC"))
away.team/nrow(playoffs.away)
nrow(playoffs.away)

test_table <- rbind(home.team, away.team) 
test_table

#chi-square test
chisq.test(test_table)

# proportion test [c(home, away)]
result.p <- prop.test(x = c(home.team, away.team), n = c(nrow(playoffs.home), nrow(playoffs.away)))
result.p
```


# Home vs. Away for (in Just CALLS) in Playoffs
```{r}

home.team <- sum(playoffs.home$review_decision == "IC")
total.home <- sum(playoffs.home$review_decision == "IC") + sum(playoffs.home$review_decision == "CC")

away.team <- sum(playoffs.away$review_decision == "IC")
total.away <- sum(playoffs.away$review_decision == "IC") + sum(playoffs.away$review_decision == "CC")


#chi-square test
chisq.test(test_table)

# proportion test [c(home, away)]
calls <- prop.test(x = c(home.team, away.team), n = c(total.home, total.away))
calls
```



# Home vs Away Team (in Just NON-CALLS) in Playoffs
```{r}

home.team <- sum(playoffs.home$review_decision == "INC")
total.home <- sum(playoffs.home$review_decision == "INC") + sum(playoffs.home$review_decision == "CNC")

away.team <- sum(playoffs.away$review_decision == "INC")
total.away <- sum(playoffs.away$review_decision == "INC") + sum(playoffs.away$review_decision == "CNC")


#chi-square test
chisq.test(test_table)

# proportion test [c(home, away)]
calls <- prop.test(x = c(home.team, away.team), n = c(total.home, total.away))
calls
```












#Making Regular Season Dataset
```{r}

regular15 <- subset(data2, year == 2015 & (month < 418 | month > 701))
regular16 <- subset(data2, year == 2016 & (month < 416 | month > 701))
regular17 <- subset(data2, year == 2017 & (month < 415 | month > 701))
regular18 <- subset(data2, year == 2018 & (month < 414 | month > 701))

nrow(regular15)
nrow(regular16)
nrow(regular17)
nrow(regular18)

sum(nrow(regular15) + nrow(regular16) + nrow(regular17) + nrow(regular18))

regular <- rbind(regular15, regular16, regular17, regular18)

#################

regular2 <- regular[,c("review_decision", "away", "home", "committing_team", "disadvantaged_team")]

regular2 <- na.omit(regular2)


regular2$committing_team <- droplevels(regular2$committing_team)
regular2$home <- droplevels(regular2$home)

regular2$match <- ifelse(as.character(regular2$home) == as.character(regular2$committing_team), 1, 0)


# Home Team commits foul
regular.home <- subset(regular2, match == 1)


# Away Team commits foul
regular.away <- subset(regular2, match == 0)






```


# Tests for Regular Season
# Home vs. Away for All Calls in Regular Season
```{r}
#home team benefits from incorrect call 
home.team <- (sum(regular.home$review_decision == "IC") + sum(regular.home$review_decision == "INC"))
home.team/nrow(regular.home)
nrow(regular.home)

#away team benefits from incorrect call
away.team <- (sum(regular.away$review_decision == "IC") + sum(regular.away$review_decision == "INC"))
away.team/nrow(regular.away)
nrow(regular.away)

test_table <- rbind(home.team, away.team) 
test_table

#chi-square test
chisq.test(test_table)

# proportion test [c(home, away)]
result.r <- prop.test(x = c(home.team, away.team), n = c(nrow(regular.home), nrow(regular.away)))
result.r
```



# Home vs. Away for (in Just CALLS) in Regular Season
```{r}

home.team <- sum(regular.home$review_decision == "IC")
total.home <- sum(regular.home$review_decision == "IC") + sum(regular.home$review_decision == "CC")

away.team <- sum(regular.away$review_decision == "IC")
total.away <- sum(regular.away$review_decision == "IC") + sum(regular.away$review_decision == "CC")


#chi-square test
chisq.test(test_table)

# proportion test [c(home, away)]
calls <- prop.test(x = c(home.team, away.team), n = c(total.home, total.away))
calls
```



# Home vs Away Team (in Just NON-CALLS) in Regular Season
```{r}

home.team <- sum(regular.home$review_decision == "INC")
total.home <- sum(regular.home$review_decision == "INC") + sum(regular.home$review_decision == "CNC")

away.team <- sum(regular.away$review_decision == "INC")
total.away <- sum(regular.away$review_decision == "INC") + sum(regular.away$review_decision == "CNC")


#chi-square test
chisq.test(test_table)

# proportion test [c(home, away)]
calls <- prop.test(x = c(home.team, away.team), n = c(total.home, total.away))
calls
```








## Comparing Playoffs to Regular Season (using chi-square test)
```{r}
# Home Team Regular Season Correct Calls
home.r.c <- (sum(regular.home$review_decision == "C") + sum(regular.home$review_decision == "CNC"))
home.r.c

# Home Team Regular Season Incorrect Calls
home.r.ic <- (sum(regular.home$review_decision == "IC") + sum(regular.home$review_decision == "INC"))
home.r.ic

# Home Team Post Season Correct Calls
home.p.c <- (sum(playoffs.home$review_decision == "C") + sum(playoffs.home$review_decision == "CNC"))
home.p.c

# Home Team Post Season Incorrect Calls
home.p.ic <- (sum(playoffs.home$review_decision == "IC") + sum(playoffs.home$review_decision == "INC"))
home.p.ic



# away Team Regular Season Correct Calls
away.r.c <- (sum(regular.away$review_decision == "C") + sum(regular.away$review_decision == "CNC"))
away.r.c

# away Team Regular Season Incorrect Calls
away.r.ic <- (sum(regular.away$review_decision == "IC") + sum(regular.away$review_decision == "INC"))
away.r.ic

# away Team Post Season Correct Calls
away.p.c <- (sum(playoffs.away$review_decision == "C") + sum(playoffs.away$review_decision == "CNC"))
away.p.c

# away Team Post Season Incorrect Calls
away.p.ic <- (sum(playoffs.away$review_decision == "IC") + sum(playoffs.away$review_decision == "INC"))
away.p.ic

correct.total <- home.r.c + home.p.c + away.r.c + away.p.c
incorrect.total <- home.r.ic + home.p.ic + away.r.ic + away.p.ic

# Make Table for Chi Square Test
table <- matrix(c(home.r.c, home.r.ic, home.r.c + home.r.ic, home.p.c, home.p.ic, home.p.c + home.p.ic, away.r.c, away.r.ic, away.r.c + away.r.ic, away.p.c, away.p.ic, away.p.c + away.p.ic, correct.total, incorrect.total, correct.total + incorrect.total), ncol = 3, byrow = T)
colnames(table) <- c("Correct", "Incorrect", "Total")
rownames(table) <- c("Home: Regular", "Home: Playoffs", "Away: Regular", "Away: Playoffs", "Total")
table

table2 <- matrix(c(home.r.c, home.r.ic, home.p.c, home.p.ic, away.r.c, away.r.ic, away.p.c, away.p.ic), ncol = 2, byrow = T)
colnames(table2) <- c("Correct", "Incorrect")
rownames(table2) <- c("Home: Regular", "Home: Playoffs", "Away: Regular", "Away: Playoffs")
table2

# Running Chi Square Test
chisq.test(table2)

```












## Breaking down between 4th Quarter and Overtime
#Creating 2 different datasets
```{r}
unique(data2$period)

nrow(data2[data2$period == "Q3",])
#data2[which(data2$period == "Q3")[1],]
#data2[which(data2$period == "Q3")[2],]

nrow(data2[data2$period == "Q4",])
nrow(data2[data2$period == "Q5",])
nrow(data2[data2$period == "Q6",])
nrow(data2[data2$period == "Q7",])
nrow(data2[data2$period == "Q8",])

nrow(data2)
data6 <- data2[data2$period != "Q3",]
nrow(data6)

fourth <- subset(data6, period == "Q4")
OT <- subset(data6, period != "Q4")
six.plus <- subset(data6, period == "Q6" | period == "Q7" | period == "Q8")
fifth <- subset(data6, period == "Q5")

nrow(fourth)
nrow(OT)
nrow(fifth)
nrow(six.plus)
```

# Clean up 4th Quarter
```{r}

fourth2 <- fourth[,c("review_decision", "away", "home", "committing_team", "disadvantaged_team")]

fourth2 <- na.omit(fourth2)


fourth2$committing_team <- droplevels(fourth2$committing_team)
fourth2$home <- droplevels(fourth2$home)

fourth2$match <- ifelse(as.character(fourth2$home) == as.character(fourth2$committing_team), 1, 0)


# Home Team commits foul
fourth.home <- subset(fourth2, match == 1)


# Away Team commits foul
fourth.away <- subset(fourth2, match == 0)
```



# Tests for 4th Quarter
# Home vs. Away for All Calls in 4th Quarter
```{r}
#home team benefits from incorrect call 
home.team <- (sum(fourth.home$review_decision == "IC") + sum(fourth.home$review_decision == "INC"))
home.team/nrow(fourth.home)
nrow(fourth.home)

#away team benefits from incorrect call
away.team <- (sum(fourth.away$review_decision == "IC") + sum(fourth.away$review_decision == "INC"))
away.team/nrow(fourth.away)
nrow(fourth.away)

test_table <- rbind(home.team, away.team) 
test_table

#chi-square test
chisq.test(test_table)

# proportion test [c(home, away)]
result.f <- prop.test(x = c(home.team, away.team), n = c(nrow(regular.home), nrow(regular.away)))
result.f
```







# Clean up Overtime
```{r}

OT2 <- OT[,c("review_decision", "away", "home", "committing_team", "disadvantaged_team")]

OT2 <- na.omit(OT2)


OT2$committing_team <- droplevels(OT2$committing_team)
OT2$home <- droplevels(OT2$home)

OT2$match <- ifelse(as.character(OT2$home) == as.character(OT2$committing_team), 1, 0)


# Home Team commits foul
OT.home <- subset(OT2, match == 1)


# Away Team commits foul
OT.away <- subset(OT2, match == 0)
```



# Tests for Overtime
# Home vs. Away for All Calls in Overtime
```{r}
#home team benefits from incorrect call 
home.team <- (sum(OT.home$review_decision == "IC") + sum(OT.home$review_decision == "INC"))
home.team/nrow(OT.home)
nrow(OT.home)

#away team benefits from incorrect call
away.team <- (sum(OT.away$review_decision == "IC") + sum(OT.away$review_decision == "INC"))
away.team/nrow(OT.away)
nrow(OT.away)

test_table <- rbind(home.team, away.team) 
test_table

#chi-square test
chisq.test(test_table)

# proportion test [c(home, away)]
result.OT <- prop.test(x = c(home.team, away.team), n = c(nrow(OT.home), nrow(OT.away)))
result.OT
```





# Clean up Single Overtime
```{r}

fifth2 <- fifth[,c("review_decision", "away", "home", "committing_team", "disadvantaged_team")]

fifth2 <- na.omit(fifth2)


fifth2$committing_team <- droplevels(fifth2$committing_team)
fifth2$home <- droplevels(fifth2$home)

fifth2$match <- ifelse(as.character(fifth2$home) == as.character(fifth2$committing_team), 1, 0)


# Home Team commits foul
fifth.home <- subset(fifth2, match == 1)


# Away Team commits foul
fifth.away <- subset(fifth2, match == 0)
```



# Tests for Single Overtime
# Home vs. Away for All Calls in Single Overtime
```{r}
#home team benefits from incorrect call 
home.team <- (sum(fifth.home$review_decision == "IC") + sum(fifth.home$review_decision == "INC"))
home.team/nrow(fifth.home)
nrow(fifth.home)

#away team benefits from incorrect call
away.team <- (sum(fifth.away$review_decision == "IC") + sum(fifth.away$review_decision == "INC"))
away.team/nrow(fifth.away)
nrow(fifth.away)

test_table <- rbind(home.team, away.team) 
test_table

#chi-square test
chisq.test(test_table)

# proportion test [c(home, away)]
result.fifth <- prop.test(x = c(home.team, away.team), n = c(nrow(fifth.home), nrow(fifth.away)))
result.fifth
```







## 6th, 7th, 8th Quarters
# Clean up Six.Plus
```{r}

six.plus2 <- six.plus[,c("review_decision", "away", "home", "committing_team", "disadvantaged_team")]

six.plus2 <- na.omit(six.plus2)


six.plus2$committing_team <- droplevels(six.plus2$committing_team)
six.plus2$home <- droplevels(six.plus2$home)

six.plus2$match <- ifelse(as.character(six.plus2$home) == as.character(six.plus2$committing_team), 1, 0)


# Home Team commits foul
six.plus.home <- subset(six.plus2, match == 1)


# Away Team commits foul
six.plus.away <- subset(six.plus2, match == 0)
```



# Tests for Six.Plus
# Home vs. Away for All Calls in Double, Triple, and Quadruple OT
```{r}
#home team benefits from incorrect call 
home.team <- (sum(six.plus.home$review_decision == "IC") + sum(six.plus.home$review_decision == "INC"))
home.team/nrow(six.plus.home)
nrow(six.plus.home)

#away team benefits from incorrect call
away.team <- (sum(six.plus.away$review_decision == "IC") + sum(six.plus.away$review_decision == "INC"))
away.team/nrow(six.plus.away)
nrow(six.plus.away)

test_table <- rbind(home.team, away.team) 
test_table

#chi-square test
chisq.test(test_table)

# proportion test [c(home, away)]
result.six <- prop.test(x = c(home.team, away.team), n = c(nrow(six.plus.home), nrow(six.plus.away)))
result.six
```











# Exploring Different types of fouls
```{r}
library(tidyr)

data7 <- data2

data7$call_type <- ifelse(data7$call_type == "Jump Ball", "Other: Jump Ball", data7$call_type) 
data7$call_type <- ifelse(data7$call_type == "Other", "Other: ", data7$call_type) 

#data7[c(169, 4676, 5162, 6685, 7912, 8455, 9897, 12040, 14130, 14729, 22608, 22723, 24802, 25027),]

data7 <- separate(data = data7, col = call_type, into = c("call_category", "call_specific"), sep = ": ", remove = F)

#unique(data7$call_category)

table(data7$call_category)

foul <- subset(data7, call_category == "Foul")
unique(foul$call_specific)

# Important Tables
table(data7$call_category, data7$review_decision)
table(foul$call_specific, foul$review_decision)

```




