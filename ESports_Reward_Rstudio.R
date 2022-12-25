#Individual project_Exploratory and data visualization
#Date: 02/07/2021
#Name: Pornkamon Sirivoravijit

# Open file/ check the summary
esports <- read.csv("Esports_earning_100k.csv", header = T)
summary(esports)
str(esports)
  
# Data cleaning in R

##    1) Reward level: High, Medium, Low
##    (Low: under 1st Quartile/ Medium: 2nd-3rd Quartile/ High: >3rd Quartile)
summary(esports)
rewardlevel <- c()
rewardlevel[esports$TotalMoney <= 186760 ] <- "Low"
rewardlevel[esports$TotalMoney > 186760 & esports$TotalMoney <= 1664337 ] <- "Medium"
rewardlevel[esports$TotalMoney > 1664337] <- "High"
esports <- cbind(esports,rewardlevel)                   # Add the new column in excel file.
esports$rewardlevel <- as.factor(esports$rewardlevel)
summary(esports$rewardlevel)
summary(esports)
head(esports)
remove(rewardlevel)

##    2) Rounding number in Total reward and Top country earning
esports$Top_Country_Earnings <- round(esports$Top_Country_Earnings, 0)
esports$TotalMoney <- round(esports$TotalMoney, 0)

##    3) convert region type to be factor
esports$Continent <- as.factor(esports$Continent)
summary(esports$Continent)

# ________________________________________________________

#1) Boxplots, Histograms, Barplots (different from HW3)
##  1.1) Boxplot (Using log to present Total Reward across Genre)

boxplot(log(TotalMoney)~Genre, data = esports, ylim = c(10,20), 
        main="Log~Total reward separated by genre",cex.axis = 0.55,
        ylab= "Log~Total reward in USD", xlab="Genre", col =rgb(0,0,1,0.15))

# MOBA gave the highest reward to players in overall, then Battle Royale


##  1.2) Overlapping histograms present total reward by the level

den1 <- density(esports$Releaseyear[esports$rewardlevel == "High"])
den2 <- density(esports$Releaseyear[esports$rewardlevel== "Medium"])
den3 <- density(esports$Releaseyear[esports$rewardlevel == "Low"])

hist(esports$Releaseyear[esports$rewardlevel == "High"],
     main = "Histograms show Level of total reward",
     xlim=c(1990,2020), breaks = seq(1990,2020,3),col=rgb(1,0,0,0.15),
     cex.axis = 0.8, freq = F, xlab = "year released", 
     ylim=c(0,0.16))
hist(esports$Releaseyear[esports$rewardlevel == "Medium"],
     xlim=c(1990,2020), breaks = seq(1990,2020,3),col=rgb(0,1,0,0.15),
     cex.axis = 0.8, freq = F, add=T)
hist(esports$Releaseyear[esports$rewardlevel == "Low"],
     xlim=c(1990,2020), breaks = seq(1990,2020,3),col=rgb(0,0,1,0.15),
     cex.axis = 0.8, freq = F, add=T)

legend("topleft", c("High", "Medium", "Low"), col=c(rgb(1,0,0,0.15),rgb(0,1,0,0.15),rgb(0,0,1,0.15)), lwd=10)

lines(den1, lwd = 2, col = "lightpink4", lty = 1)
lines(den2, lwd = 2, col = "darkseagreen4", lty = 1)
lines(den3, lwd = 2, col = "blue", lty = 1)

# After 2000 e-sport became more popular as the number is doubled.
# Among 2014-2017 have the most concentrate in high level.


# 1.3) Barplot shows regions who won top reward of each game.
# North America won the highest reward in 94 games, Europe 44 games, and Asia 37 games

mycolor <- c("#FFDB6D", "#C4961A", "#D16103", 
                "#293352", "#C3D7A4")
regiontab <- table(esports$Continent)	
barplot(regiontab)
xx <- barplot(regiontab , ylab = "Game", main = "Regions win top reward", xlab="Region",
        col = mycolor, ylim=c(0,120))
text(x = xx, y = regiontab, label = regiontab, pos = 3, 
     cex = 0.8, col = "#293352")


#2) scatter plot 
plot(esports[,7:11]) #Overall view

#  Player x Tournament might relate in somehow (ex. if more players, more tournaments)
plot(esports$PlayerNo[esports$rewardlevel == "High"], xlab="Players", ylab="Tournaments",
     esports$TournamentNo[esports$rewardlevel == "High"], pch=7, ylim=c(0,6000), xlim=c(0,13000),
     main="Number of Player and Tournament", col="pink")
     
points(esports$PlayerNo[esports$rewardlevel == "Medium"], 
       esports$TournamentNo[esports$rewardlevel == "Medium"], pch=6,
      col="darkseagreen2")

points(esports$PlayerNo[esports$rewardlevel == "Low"], 
       esports$TournamentNo[esports$rewardlevel == "Low"], pch=5,
       col="mediumpurple1")
legend("topleft", c("High", "Medium", "Low"), col=c("pink","darkseagreen2","mediumpurple1"), lwd=3)

grid(col = "gray70")
# The plot is pretty skew as I did not transform the number of Tournament and Player
# And it's weired that most high level of total reward are in the same range as Low and Medium level
# Even there are some games in high level have a lot of players and tournaments

model0 <- lm(TournamentNo~PlayerNo, data=esports)
model0                              
# Equation: Tournament = 21.9344 + 0.3922(PlayerNo)
#1 Tournament = 22.33 players.
#2 Tournamnet = 22.72 players.
#3 Tournament = 23.11 players.
#10 Tournament = 25.86 players.
#37 Tournament = 36.45 players.
#40 Tournament = 37.63 players.

# and at 37 tournament+, the number of players is lower than tournament.
# In my perspective, the more tournament they held, the less players received reward. 
# I assume that esports have the limited players and limited budget
# Even though esports became more popular nowadays, 
# the reward is limited for a few players who won the first-place reward. 


#3) Linear regression transform in term of logarithm.

# Player number x Tournament number
plot(log(esports$PlayerNo[esports$rewardlevel == "High"]), xlab="Log of Players", ylab="Log of Tournaments",
     log(esports$TournamentNo[esports$rewardlevel == "High"]), pch=7, ylim=c(0,10), xlim=c(0,10),
     main="Number of Player and Tournament", col="pink")

points(log(esports$PlayerNo[esports$rewardlevel == "Medium"]), 
       log(esports$TournamentNo[esports$rewardlevel == "Medium"]), pch=6,
       col="darkseagreen2")

points(log(esports$PlayerNo[esports$rewardlevel == "Low"]), 
       log(esports$TournamentNo[esports$rewardlevel == "Low"]), pch=5,
       col="mediumpurple1")

legend("topleft", c("High", "Medium", "Low"), col=c("pink","darkseagreen2","mediumpurple1"), lwd=3)
grid(col = "gray70")

model1 <- lm(log(TournamentNo)~log(PlayerNo), data=esports)
model1                              # Tournament = -1.2855 + 0.9913(PlayerNo)
abline(model1, col="black")         # Regressoin line

model10 <- lm(TournamentNo~PlayerNo, data=esports)

smooth <- smooth.spline(log(esports$PlayerNo), log(esports$TournamentNo), spar=1)
lines(smooth, col="#C4961A")        # Smooth line (is pretty close to the regression line.)

# This equation "Tournament = -1.2855 + 0.9913(PlayerNo)" tells me that 
# 1 tournament is predicted to have -0.29 players (In term of Log), 
# 2 tournament : 0.7 players
# 3 tournament : 1.69 players
# 5 tournament : 3.67 players
# 10 tournament : 8.63 players

# We see It is impossible that 1 tounament has -0.29 player.
# While, the player is always less than the tournament (even positive player) 
# so that means some tournament that they hold,
# they will not give the reward for winner. 
# I assume they hold tournament for entertainment and advertisement.

summary(model1)
plot(model1)

#Fitted: most of the points close to zero.
#QQ plot: overall looks great, some points are outlier.
#Scale-Location: the line is flat, overall is good.
#Leverage: a few points almost over cook's distance.


# Top country x Total money
plot(log(esports$TotalMoney), log(esports$Top_Country_Earnings), xlim=c(10,20)
     ,main="Total reward and Top country earning", xlab="Log of Total Reward",
     ylab="Log of Top Country Earning")

model6<- lm(log(Top_Country_Earnings)~log(TotalMoney), data=esports)
abline(model6)
plot(log(esports$TotalMoney), log(esports$Top_Country_Earnings))

## Compare Total money in term of Log x Total money to see the overall
summary(log(esports$TotalMoney))
summary(esports$TotalMoney)


#4) Interesting boxplot, pie graph, etc..

# 4.1) Barplot Split the continent with the level of total reward
region_rewardtab<- table(esports$rewardlevel, esports$Continent)
xy = barplot(region_rewardtab, ylab = "Count", main = "Count by region and level of reward", xlab="Region",
        col = c("mediumpurple1","darkseagreen2","pink"), ylim=c(0,60), beside=T)
legend("topright", c("High","Medium","Low"), 
       text.col=c("pink","darkseagreen2","mediumpurple1"))
text(x = xy, y = region_rewardtab, label = region_rewardtab, pos = 3, 
     cex = 0.8, col = "#293352")

# 4.2) Barplot (EUROPE): United Kingdon > Denmark
EU_tab<- table(esports$rewardlevel[esports$Continent == "Europe"], 
               esports$Country_Code[esports$Continent == "Europe"])

barplot(EU_tab, ylim=c(0,10), beside = T, 
        main="Europe",cex.axis = 0.55,
        col = c("mediumpurple1","darkseagreen2","pink"),
        xlab="Country", ylab = "Count")

legend("center", c("High","Medium","Low"), 
       text.col=c("pink","darkseagreen2","mediumpurple1"))

# 4.3) Barplot (ASIA): China > Korea > Japan
Asia_tab<- table(esports$rewardlevel[esports$Continent == "Asia"], 
                 esports$Country_Code[esports$Continent == "Asia"])

barplot(Asia_tab, ylim=c(0,10), beside = T, 
        main="Asia",cex.axis = 0.55,
        col = c("mediumpurple1","darkseagreen2","pink"),
        xlab="Country", ylab = "Count")

legend("topright", c("High","Medium","Low"), 
       text.col=c("pink","darkseagreen2","mediumpurple1"))


# 4.4) Pie chart Genre: First person shooter > Sports > Fighting Game
genretable <- table(esports$Genre)	
genretable
pie(genretable, main = "Genre")

prop.table(genretable)
round_genre_tab <- round(prop.table(genretable)*100, 2)	#Rounding 2 digit and *100 in term of percentage
genrelable <- paste(names(genretable), "\n", round_genre_tab, "%", sep ="") #Create lable to show in the pie chart
pie(round_genre_tab, label = genrelable, main = "Percentage of Genre",cex=0.6)


# 4.5) Boxplot: classify the reward level in 3 levels, High, Medium, Low
# For explaning at the beginning of the presentation what's the number I use to separate those three groups
esports$rewardlevel <- factor(esports$rewardlevel, 
                              levels=c("High", "Medium", "Low")) # reorder
summary(esports$TotalMoney) 
summary(log(esports$TotalMoney[esports$rewardlevel == "High"]))   #         / Min 14.34
summary(log(esports$TotalMoney[esports$rewardlevel == "Medium"])) #Max 14.28/ Min 12.16
summary(log(esports$TotalMoney[esports$rewardlevel == "Low"]))    #Max 12.13

summary(esports$TotalMoney[esports$rewardlevel == "High"])   #Max 227M / Min 1.68M
summary(esports$TotalMoney[esports$rewardlevel == "Medium"]) #Max 1.59M / Min 190K
summary(esports$TotalMoney[esports$rewardlevel == "Low"])    #Max 185K / Min 100K


boxplot(log(TotalMoney)~rewardlevel, data=esports, xlab="Reward level",
        ylab="$ Total money(in term of Log)", main="Classification of level",
        col=c("pink","darkseagreen2","mediumpurple1"))
abline(h=14.30, col="red")
abline(h=12.15, col="red")

#Export the dataset
write.csv(esports, "Esports_V.3.csv", row.names = F)