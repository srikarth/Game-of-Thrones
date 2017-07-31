battles <- read.csv("battles.csv")
deaths <- read.csv("character-deaths.csv")
predictions <- read.csv("character-predictions.csv")
str(battles)
temp <- as.factor(battles$year)
plot(temp)
#maximum battles in year 299, then year 300, then 298
summary(battles$battle_number)
table(battles$battle_number)
table(battles$attacker_king)
library(ggplot2)
ggplot(battles,aes(x = battles$attacker_king)) + geom_bar()+xlab("king")
# highest attacks done by joffrey/tommen baratheon
ggplot(battles,aes(x = battles$defender_king)) + geom_bar()+xlab("king")
summary(battles$defender_king)
# most defends done by robb stark followed by joffrey/tommen baratheon
table(battles$attacker_king,battles$defender_king)
#most fights between Robb stark and joffrey/tommen baratheon
table(battles$attacker_outcome,battles$attacker_king)
#maximum wins are achieved by joffrey/tommen baratheon followed by Robb stark, the most battles are won by baratheons
summary(battles$attacker_1)
table(battles$attacker_king,battles$attacker_1)
# in the fights of baratheon, baratheon being attackers we have lannisters as the their attackers in the fights while in other houses the attackers are mostly 
#hence their names themselves indicating the dependency of baratheon on lannisters
table(battles$attacker_king,battles$battle_type)
#Robb stark has taken in highest number of ambush battles followed by tommen baratheon, Baratheon has fought highest number of pitched battles and siege battles
summary(battles$major_death)
summary(battles$major_capture)
table(battles$attacker_king,battles$attacker_size)
table(battles$attacker_outcome,battles$attacker_size)
#This reveals a striking result that the attacking size having army sizes like 100000,21000 have lost in the wars rather than the armies which had less army strength 
#won the battles
#After closely analysing the data we come to know that the fight involving 100000 attackers was fought at castle black, defenders having the advantage of the wall
#and height, they won
table(battles$attacker_size,battles$defender_size)
table(battles$attacker_outcome)
battles$attacker_outcome <- as.character(battles$attacker_outcome)
battles$attacker_outcome <- ifelse((battles$attacker_outcome == ""),"loss",battles$attacker_outcome)



battles$attacker_commander
battles$attacker_commander <- as.character(battles$attacker_commander)
battles$attacker_commander <- ifelse((battles$attacker_commander == ""),"no",battles$attacker_commander)

splitdat = do.call("rbind", strsplit(battles$attacker_commander, ","))
splitdat = data.frame(apply(splitdat, 2, as.character))
colnames(splitdat) <- paste("commander", 1:6, sep = "")
splitdat
battles <- cbind(battles,splitdat)
temp <- ifelse(is.na(battles$attacker_size),0,battles$attacker_size)
avg <- mean(temp)
temp1 <- ifelse(is.na(battles$defender_size),0,battles$defender_size)
avg1 <- mean(temp1)
#filling missing values in attack size nad defend size
battles$attacker_size <- ifelse(is.na(battles$attacker_size),avg,battles$attacker_size)
battles$defender_size <- ifelse(is.na(battles$defender_size),avg,battles$defender_size)
cor(battles$defender_size,battles$attacker_size)
#this correlation comes out to be negative which seems to be a bit wierd cause this shows that in the battles, when one of the sides had an increasing army, the other side had decreasing army
#now we can see that there are a lot of categorical variables in this data set and they could be related in a specific fashion, now we will take two variables, i.e. outcome of the battle and 
#the region in which battle took place and form a tree of this
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(MASS)
install.packages("RColorBrewer")
tree <- rpart(battles$attacker_outcome~battles$region,data = battles,control=rpart.control(minsplit=5, cp=0.001),method = "class")
fancyRpartPlot(tree)

table <- table(battles$region,battles$attacker_outcome)
chisq.test(table)
#the data is very less so you will get a warning message, you can combine two or more regions so that
#the results become more efficient
#Further taking more categorical variables and applying chi-square test of independence

table1 <- table(battles$attacker_king,battles$attacker_outcome)
table1
chisq.test(table1)
#The p values shoe that the outcome of the battle is more dependent on the regions than the king
#The similar analysis could be done on the defender king and the statistical analysis could be done
