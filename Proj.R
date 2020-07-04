setwd("C:\\Users\\mladi\\Desktop\\Projekat")
EPL<-data.frame(read.csv("EPL 2000-2018.csv"))
library(ggplot2)

#Number of observatins
nrow(EPL)

#structure of our Data
str(EPL)

#Cheking is there a empty column
which(is.na(EPL$FTR))

rm(list = ls())




table(EPL$FTR)


Date<-(EPL$Date)
HomeTeam<-(EPL$HomeTeam)
AwayTeam<-(EPL$AwayTeam)
Full_Time_Home_Team_Goals<-(EPL$FTHG)
Full_Time_Away_Team_Goals<-(EPL$FTAG)
FUll_Time_Result<-(EPL$FTR)
Half_Time_Home_Team_Goals<-(EPL$HTHG)
Half_Time_Away_Team_Goals<-(EPL$HTAG)
Half_Time_Result<-(EPL$HTR)
Referee<-(EPL$Referee)
Home_Team_Shots<-(EPL$HS)
Away_Team_Shots<-(EPL$AS)
Home_Team_Shots_On_Target<-(EPL$HST)
Away_Team_Shots_On_Target<-(EPL$AST)
Home_Team_Fauls_Committed<-(EPL$HF)
Away_Team_Fauls_Committed<-(EPL$AF)
Home_Team_Corners<-(EPL$HC)
Away_Team_Corners<-(EPL$AC)
Home_Team_Red_Cards<-(EPL$HR)
Away_Team_Red_Cards<-(EPL$AR)
Season<-(EPL$Season)
n <- length(EPL$Date)



#Total sum of Home Tema Goals
SumHomeGoals<-sum(as.numeric(Full_Time_Home_Team_Goals), na.rm = TRUE)
#Summary of Home Team Goals.
summary(Full_Time_Home_Team_Goals)

#Total sum of HOmeHalfTIme Tema Goals
SumHomeHalfTimeGoals<-sum(as.numeric(Half_Time_Home_Team_Goals), na.rm = TRUE)
#Summary of Halftime goals
summary(SumHomeHalfTimeGoals)




#Total sum of Away Tema Goals
SumAwayGoals<-sum(as.numeric(Full_Time_Away_Team_Goals), na.rm = TRUE)
#Summary of Away Team Goals.
summary(Full_Time_Away_Team_Goals)

#Total sum of AwayHalfTime Tema Goals
SumAwayHalfTimeGoals<-sum(as.numeric(Half_Time_Away_Team_Goals), na.rm = TRUE)



#TotalNumberofGoals
TotalNUmberOfGoals<-(sum(as.numeric(Full_Time_Away_Team_Goals), na.rm = TRUE)+sum(as.numeric(Full_Time_Home_Team_Goals), na.rm = TRUE))


#Total Number of Goals in the first Half
TotalNUmberOfGoalsINHalfTIme<-(sum(as.numeric(Half_Time_Home_Team_Goals), na.rm = TRUE)+sum(as.numeric(Half_Time_Away_Team_Goals), na.rm = TRUE))





#Avrage number of corner by home team
AvgCornersHomeTeam<-(sum(as.numeric(Home_Team_Corners), na.rm = TRUE)/n)

#Avrage number of corner by home team
AvgCornersAwayTeama<-(sum(as.numeric(Away_Team_Corners), na.rm = TRUE)/n)

#Total Avrage corner by the game
AvgCornerPerGame<-(AvgCornersHomeTeam+AvgCornersAwayTeama)

#Avrage Red cards by the home team
AvgRedCardsHome<-(sum(as.numeric(Home_Team_Red_Cards), na.rm = TRUE)/n)

#Avrage Red cards by the home team
AvgRedCardsAway<-(sum(as.numeric(Away_Team_Red_Cards), na.rm = TRUE)/n)

#avg Number of red cards
AvgRedCarsTotal<-(AvgRedCardsHome+AvgRedCardsAway)


sum(as.numeric(Away_Team_Corners), na.rm = TRUE)


#Avg Home tema shots on target
AvgHomeTeamShotsOnTarget<-(sum(as.numeric(Home_Team_Shots_On_Target), na.rm = TRUE)/n)

#Avg Away tema shots on target
AvgAwayTeamShotsOnTarget<-(sum(as.numeric(Away_Team_Shots_On_Target), na.rm = TRUE)/n)


(sum(as.numeric(Half_Time_Home_Team_Goals), na.rm = TRUE))





#-----------------------------------------------------------------------------------------
#Season 01-02
S01_02_HomeTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="00-01",4]), na.rm = TRUE))
S01_02_AwayTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="00-01",5]), na.rm = TRUE))
S01_02_HomeTeamHalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="00-01",7]), na.rm = TRUE))
S01_02_AwayTeamFalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="00-01",8]), na.rm = TRUE))
S01_02_HomeTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="00-01",19]), na.rm = TRUE))
S01_02_AwayTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="00-01",20]), na.rm = TRUE))
S01_02_HomeTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="00-01",21]), na.rm = TRUE))
S01_02_AwayTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="00-01",22]), na.rm = TRUE))

#Season 01-02 Total
S01_02_TotalGoals<-S01_02_HomeTeamGoals+S01_02_AwayTeamGoals
S01_02_TotalGoalsinHaLfTim<-S01_02_HomeTeamHalfTimeGoals+S01_02_AwayTeamFalfTimeGoals
S01_02_TotalYelloWCards<-S01_02_HomeTeamYellowCards+S01_02_AwayTeamYellowCards
S01_02_TotalRedCards<-S01_02_HomeTeamRedCards+S01_02_AwayTeamRedCards

summary(EPL[EPL$Season=="00-01",4])
summary(EPL[EPL$Season=="00-01",5])



#-----------------------------------------------------------------------------------------

#Season 02-03
S02_03_HomeTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="02-03",4]), na.rm = TRUE))
S02_03_AwayTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="02-03",5]), na.rm = TRUE))
S02_03_HomeTeamHalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="02-03",7]), na.rm = TRUE))
S02_03_AwayTeamFalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="02-03",8]), na.rm = TRUE))
S02_03_HomeTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="02-03",19]), na.rm = TRUE))
S02_03_AwayTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="02-03",20]), na.rm = TRUE))
S02_03_HomeTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="02-03",21]), na.rm = TRUE))
S02_03_AwayTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="02-03",22]), na.rm = TRUE))

#Season 02-03 Total
S02_03_TotalGoals<-S02_03_HomeTeamGoals+S02_03_AwayTeamGoals
S02_03_TotalGoalsinHaLfTim<-S02_03_HomeTeamHalfTimeGoals+S02_03_AwayTeamFalfTimeGoals
S02_03_TotalYelloWCards<-S02_03_HomeTeamYellowCards+S02_03_AwayTeamYellowCards
S02_03_TotalRedCards<-S02_03_HomeTeamRedCards+S02_03_AwayTeamRedCards

#-----------------------------------------------------------------------------------------


#Season 03-04
S03_04_HomeTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="03-04",4]), na.rm = TRUE))
S03_04_AwayTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="03-04",5]), na.rm = TRUE))
S03_04_HomeTeamHalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="03-04",7]), na.rm = TRUE))
S03_04_AwayTeamFalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="03-04",8]), na.rm = TRUE))
S03_04_HomeTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="03-04",19]), na.rm = TRUE))
S03_04_AwayTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="03-04",20]), na.rm = TRUE))
S03_04_HomeTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="03-04",21]), na.rm = TRUE))
S03_04_AwayTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="03-04",22]), na.rm = TRUE))

#Season 03-04 Total
S03_04_TotalGoals<-S03_04_HomeTeamGoals+S03_04_AwayTeamGoals
S03_04_TotalGoalsinHaLfTim<-S03_04_HomeTeamHalfTimeGoals+S03_04_AwayTeamFalfTimeGoals
S03_04_TotalYelloWCards<-S03_04_HomeTeamYellowCards+S03_04_AwayTeamYellowCards
S03_04_TotalRedCards<-S03_04_HomeTeamRedCards+S03_04_AwayTeamRedCards

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


#Season 04-05
S04_05_HomeTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="04-05",4]), na.rm = TRUE))
S04_05_AwayTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="04-05",5]), na.rm = TRUE))
S04_05_HomeTeamHalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="04-05",7]), na.rm = TRUE))
S04_05_AwayTeamFalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="04-05",8]), na.rm = TRUE))
S04_05_HomeTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="04-05",19]), na.rm = TRUE))
S04_05_AwayTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="04-05",20]), na.rm = TRUE))
S04_05_HomeTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="04-05",21]), na.rm = TRUE))
S04_05_AwayTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="04-05",22]), na.rm = TRUE))

#Season 04-05 Total
S04_05_TotalGoals<-S04_05_HomeTeamGoals+S04_05_AwayTeamGoals
S04_05_TotalGoalsinHaLfTim<-S04_05_HomeTeamHalfTimeGoals+S04_05_AwayTeamFalfTimeGoals
S04_05_TotalYelloWCards<-S04_05_HomeTeamYellowCards+S04_05_AwayTeamYellowCards
S04_05_TotalRedCards<-S04_05_HomeTeamRedCards+S04_05_AwayTeamRedCards

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


#Season 05-06
S05_06_HomeTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="05-06",4]), na.rm = TRUE))
S05_06_AwayTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="05-06",5]), na.rm = TRUE))
S05_06_HomeTeamHalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="05-06",7]), na.rm = TRUE))
S05_06_AwayTeamFalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="05-06",8]), na.rm = TRUE))
S05_06_HomeTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="05-06",19]), na.rm = TRUE))
S05_06_AwayTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="05-06",20]), na.rm = TRUE))
S05_06_HomeTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="05-06",21]), na.rm = TRUE))
S05_06_AwayTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="05-06",22]), na.rm = TRUE))

#Season 05-06 Total
S05_06_TotalGoals<-S05_06_HomeTeamGoals+S05_06_AwayTeamGoals
S05_06_TotalGoalsinHaLfTim<-S05_06_HomeTeamHalfTimeGoals+S05_06_AwayTeamFalfTimeGoals
S05_06_TotalYelloWCards<-S05_06_HomeTeamYellowCards+S05_06_AwayTeamYellowCards
S05_06_TotalRedCards<-S05_06_HomeTeamRedCards+S05_06_AwayTeamRedCards

#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------


#Season 06-07
S06_07_HomeTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="06-07",4]), na.rm = TRUE))
S06_07_AwayTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="06-07",5]), na.rm = TRUE))
S06_07_HomeTeamHalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="06-07",7]), na.rm = TRUE))
S06_07_AwayTeamFalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="06-07",8]), na.rm = TRUE))
S06_07_HomeTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="06-07",19]), na.rm = TRUE))
S06_07_AwayTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="06-07",20]), na.rm = TRUE))
S06_07_HomeTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="06-07",21]), na.rm = TRUE))
S06_07_AwayTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="06-07",22]), na.rm = TRUE))

#Season 06-07 Total
S06_07_TotalGoals<-S06_07_HomeTeamGoals+S06_07_AwayTeamGoals
S06_07_TotalGoalsinHaLfTim<-S06_07_HomeTeamHalfTimeGoals+S06_07_AwayTeamFalfTimeGoals
S06_07_TotalYelloWCards<-S06_07_HomeTeamYellowCards+S06_07_AwayTeamYellowCards
S06_07_TotalRedCards<-S06_07_HomeTeamRedCards+S06_07_AwayTeamRedCards

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


#Season 07-08
S07_08_HomeTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="07-08",4]), na.rm = TRUE))
S07_08_AwayTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="07-08",5]), na.rm = TRUE))
S07_08_HomeTeamHalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="07-08",7]), na.rm = TRUE))
S07_08_AwayTeamFalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="07-08",8]), na.rm = TRUE))
S07_08_HomeTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="07-08",19]), na.rm = TRUE))
S07_08_AwayTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="07-08",20]), na.rm = TRUE))
S07_08_HomeTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="07-08",21]), na.rm = TRUE))
S07_08_AwayTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="07-08",22]), na.rm = TRUE))

#Season 07-08 Total
S07_08_TotalGoals<-S07_08_HomeTeamGoals+S07_08_AwayTeamGoals
S07_08_TotalGoalsinHaLfTim<-S07_08_HomeTeamHalfTimeGoals+S07_08_AwayTeamFalfTimeGoals
S07_08_TotalYelloWCards<-S07_08_HomeTeamYellowCards+S07_08_AwayTeamYellowCards
S07_08_TotalRedCards<-S07_08_HomeTeamRedCards+S07_08_AwayTeamRedCards

#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------


#Season 08-09
S08_09_HomeTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="08-09",4]), na.rm = TRUE))
S08_09_AwayTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="08-09",5]), na.rm = TRUE))
S08_09_HomeTeamHalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="08-09",7]), na.rm = TRUE))
S08_09_AwayTeamFalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="08-09",8]), na.rm = TRUE))
S08_09_HomeTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="08-09",19]), na.rm = TRUE))
S08_09_AwayTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="08-09",20]), na.rm = TRUE))
S08_09_HomeTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="08-09",21]), na.rm = TRUE))
S08_09_AwayTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="08-09",22]), na.rm = TRUE))

#Season 08-09 Total
S08_09_TotalGoals<-S08_09_HomeTeamGoals+S08_09_AwayTeamGoals
S08_09_TotalGoalsinHaLfTim<-S08_09_HomeTeamHalfTimeGoals+S08_09_AwayTeamFalfTimeGoals
S08_09_TotalYelloWCards<-S08_09_HomeTeamYellowCards+S08_09_AwayTeamYellowCards
S08_09_TotalRedCards<-S08_09_HomeTeamRedCards+S08_09_AwayTeamRedCards

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


#Season 09-10
S09_10_HomeTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="09-10",4]), na.rm = TRUE))
S09_10_AwayTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="09-10",5]), na.rm = TRUE))
S09_10_HomeTeamHalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="09-10",7]), na.rm = TRUE))
S09_10_AwayTeamFalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="09-10",8]), na.rm = TRUE))
S09_10_HomeTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="09-10",19]), na.rm = TRUE))
S09_10_AwayTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="09-10",20]), na.rm = TRUE))
S09_10_HomeTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="09-10",21]), na.rm = TRUE))
S09_10_AwayTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="09-10",22]), na.rm = TRUE))

#Season 09-10 Total
S09_10_TotalGoals<-S09_10_HomeTeamGoals+S09_10_AwayTeamGoals
S09_10_TotalGoalsinHaLfTim<-S09_10_HomeTeamHalfTimeGoals+S09_10_AwayTeamFalfTimeGoals
S09_10_TotalYelloWCards<-S09_10_HomeTeamYellowCards+S09_10_AwayTeamYellowCards
S09_10_TotalRedCards<-S09_10_HomeTeamRedCards+S09_10_AwayTeamRedCards

#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------


#Season 10-11
S10_11_HomeTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="10-11",4]), na.rm = TRUE))
S10_11_AwayTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="10-11",5]), na.rm = TRUE))
S10_11_HomeTeamHalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="10-11",7]), na.rm = TRUE))
S10_11_AwayTeamFalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="10-11",8]), na.rm = TRUE))
S10_11_HomeTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="10-11",19]), na.rm = TRUE))
S10_11_AwayTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="10-11",20]), na.rm = TRUE))
S10_11_HomeTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="10-11",21]), na.rm = TRUE))
S10_11_AwayTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="10-11",22]), na.rm = TRUE))

#Season 10-11 Total
S10_11_TotalGoals<-S10_11_HomeTeamGoals+S10_11_AwayTeamGoals
S10_11_TotalGoalsinHaLfTim<-S10_11_HomeTeamHalfTimeGoals+S10_11_AwayTeamFalfTimeGoals
S10_11_TotalYelloWCards<-S10_11_HomeTeamYellowCards+S10_11_AwayTeamYellowCards
S10_11_TotalRedCards<-S10_11_HomeTeamRedCards+S10_11_AwayTeamRedCards

#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


#Season 11-12
S11_12_HomeTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="11-12",4]), na.rm = TRUE))
S11_12_AwayTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="11-12",5]), na.rm = TRUE))
S11_12_HomeTeamHalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="11-12",7]), na.rm = TRUE))
S11_12_AwayTeamFalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="11-12",8]), na.rm = TRUE))
S11_12_HomeTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="11-12",19]), na.rm = TRUE))
S11_12_AwayTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="11-12",20]), na.rm = TRUE))
S11_12_HomeTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="11-12",21]), na.rm = TRUE))
S11_12_AwayTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="11-12",22]), na.rm = TRUE))

#Season 11-12 Total
S11_12_TotalGoals<-S11_12_HomeTeamGoals+S11_12_AwayTeamGoals
S11_12_TotalGoalsinHaLfTim<-S11_12_HomeTeamHalfTimeGoals+S11_12_AwayTeamFalfTimeGoals
S11_12_TotalYelloWCards<-S11_12_HomeTeamYellowCards+S11_12_AwayTeamYellowCards
S11_12_TotalRedCards<-S11_12_HomeTeamRedCards+S11_12_AwayTeamRedCards



#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


#Season 12-13
S12_13_HomeTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="12-13",4]), na.rm = TRUE))
S12_13_AwayTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="12-13",5]), na.rm = TRUE))
S12_13_HomeTeamHalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="12-13",7]), na.rm = TRUE))
S12_13_AwayTeamFalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="12-13",8]), na.rm = TRUE))
S12_13_HomeTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="12-13",19]), na.rm = TRUE))
S12_13_AwayTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="12-13",20]), na.rm = TRUE))
S12_13_HomeTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="12-13",21]), na.rm = TRUE))
S12_13_AwayTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="12-13",22]), na.rm = TRUE))

#Season 12-13 Total
S12_13_TotalGoals<-S12_13_HomeTeamGoals+S12_13_AwayTeamGoals
S12_13_TotalGoalsinHaLfTim<-S12_13_HomeTeamHalfTimeGoals+S12_13_AwayTeamFalfTimeGoals
S12_13_TotalYelloWCards<-S12_13_HomeTeamYellowCards+S12_13_AwayTeamYellowCards
S12_13_TotalRedCards<-S12_13_HomeTeamRedCards+S12_13_AwayTeamRedCards


#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


#Season 13-14
S13_14_HomeTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="13-14",4]), na.rm = TRUE))
S13_14_AwayTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="13-14",5]), na.rm = TRUE))
S13_14_HomeTeamHalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="13-14",7]), na.rm = TRUE))
S13_14_AwayTeamFalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="13-14",8]), na.rm = TRUE))
S13_14_HomeTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="13-14",19]), na.rm = TRUE))
S13_14_AwayTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="13-14",20]), na.rm = TRUE))
S13_14_HomeTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="13-14",21]), na.rm = TRUE))
S13_14_AwayTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="13-14",22]), na.rm = TRUE))

#Season 13-14 Total
S13_14_TotalGoals<-S13_14_HomeTeamGoals+S13_14_AwayTeamGoals
S13_14_TotalGoalsinHaLfTim<-S13_14_HomeTeamHalfTimeGoals+S13_14_AwayTeamFalfTimeGoals
S13_14_TotalYelloWCards<-S13_14_HomeTeamYellowCards+S13_14_AwayTeamYellowCards
S13_14_TotalRedCards<-S13_14_HomeTeamRedCards+S13_14_AwayTeamRedCards


#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


#Season 14-15
S14_15_HomeTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="14-15",4]), na.rm = TRUE))
S14_15_AwayTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="14-15",5]), na.rm = TRUE))
S14_15_HomeTeamHalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="14-15",7]), na.rm = TRUE))
S14_15_AwayTeamFalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="14-15",8]), na.rm = TRUE))
S14_15_HomeTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="14-15",19]), na.rm = TRUE))
S14_15_AwayTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="14-15",20]), na.rm = TRUE))
S14_15_HomeTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="14-15",21]), na.rm = TRUE))
S14_15_AwayTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="14-15",22]), na.rm = TRUE))

#Season 15-16 Total
S14_15_TotalGoals<-S14_15_HomeTeamGoals+S14_15_AwayTeamGoals
S14_15_TotalGoalsinHaLfTim<-S14_15_HomeTeamHalfTimeGoals+S14_15_AwayTeamFalfTimeGoals
S14_15_TotalYelloWCards<-S14_15_HomeTeamYellowCards+S14_15_AwayTeamYellowCards
S14_15_TotalRedCards<-S14_15_HomeTeamRedCards+S14_15_AwayTeamRedCards


#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


#Season 15-16
S15_16_HomeTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="15-16",4]), na.rm = TRUE))
S15_16_AwayTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="15-16",5]), na.rm = TRUE))
S15_16_HomeTeamHalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="15-16",7]), na.rm = TRUE))
S15_16_AwayTeamFalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="15-16",8]), na.rm = TRUE))
S15_16_HomeTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="15-16",19]), na.rm = TRUE))
S15_16_AwayTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="15-16",20]), na.rm = TRUE))
S15_16_HomeTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="15-16",21]), na.rm = TRUE))
S15_16_AwayTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="15-16",22]), na.rm = TRUE))

#Season 15-16 Total
S15_16_TotalGoals<-S15_16_HomeTeamGoals+S15_16_AwayTeamGoals
S15_16_TotalGoalsinHaLfTim<-S15_16_HomeTeamHalfTimeGoals+S15_16_AwayTeamFalfTimeGoals
S15_16_TotalYelloWCards<-S15_16_HomeTeamYellowCards+S15_16_AwayTeamYellowCards
S15_16_TotalRedCards<-S15_16_HomeTeamRedCards+S15_16_AwayTeamRedCards


#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


#Season 16-17
S16_17_HomeTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="16-17",4]), na.rm = TRUE))
S16_17_AwayTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="16-17",5]), na.rm = TRUE))
S16_17_HomeTeamHalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="16-17",7]), na.rm = TRUE))
S16_17_AwayTeamFalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="16-17",8]), na.rm = TRUE))
S16_17_HomeTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="16-17",19]), na.rm = TRUE))
S16_17_AwayTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="16-17",20]), na.rm = TRUE))
S16_17_HomeTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="16-17",21]), na.rm = TRUE))
S16_17_AwayTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="16-17",22]), na.rm = TRUE))

#Season 16-17 Total
S16_17_TotalGoals<-S16_17_HomeTeamGoals+S16_17_AwayTeamGoals
S16_17_TotalGoalsinHaLfTim<-S16_17_HomeTeamHalfTimeGoals+S16_17_AwayTeamFalfTimeGoals
S16_17_TotalYelloWCards<-S16_17_HomeTeamYellowCards+S16_17_AwayTeamYellowCards
S16_17_TotalRedCards<-S16_17_HomeTeamRedCards+S16_17_AwayTeamRedCards


#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


#Season 17-18
S17_18_HomeTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="17-18",4]), na.rm = TRUE))
S17_18_AwayTeamGoals<-(sum(as.numeric(EPL[EPL$Season=="17-18",5]), na.rm = TRUE))
S17_18_HomeTeamHalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="17-18",7]), na.rm = TRUE))
S17_18_AwayTeamFalfTimeGoals<-(sum(as.numeric(EPL[EPL$Season=="17-18",8]), na.rm = TRUE))
S17_18_HomeTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="17-18",19]), na.rm = TRUE))
S17_18_AwayTeamYellowCards<-(sum(as.numeric(EPL[EPL$Season=="17-18",20]), na.rm = TRUE))
S17_18_HomeTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="17-18",21]), na.rm = TRUE))
S17_18_AwayTeamRedCards<-(sum(as.numeric(EPL[EPL$Season=="17-18",22]), na.rm = TRUE))

#Season 17-18 Total
S17_18_TotalGoals<-S17_18_HomeTeamGoals+S17_18_AwayTeamGoals
S17_18_TotalGoalsinHaLfTim<-S17_18_HomeTeamHalfTimeGoals+S17_18_AwayTeamFalfTimeGoals
S17_18_TotalYelloWCards<-S17_18_HomeTeamYellowCards+S17_18_AwayTeamYellowCards
S17_18_TotalRedCards<-S17_18_HomeTeamRedCards+S17_18_AwayTeamRedCards




#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------


Ars<-(sum(as.numeric(EPL[EPL$HomeTeam=="Arsenal",4]), na.rm = TRUE))+(sum(as.numeric(EPL[EPL$AwayTeam=="Arsenal",5]), na.rm = TRUE))

Cfc<-(sum(as.numeric(EPL[EPL$HomeTeam=="Chelsea",4]), na.rm = TRUE))+(sum(as.numeric(EPL[EPL$AwayTeam=="Chelsea",5]), na.rm = TRUE))

ManUtd<-(sum(as.numeric(EPL[EPL$HomeTeam=="Man United",4]), na.rm = TRUE))+(sum(as.numeric(EPL[EPL$AwayTeam=="Man United",5]), na.rm = TRUE))

ManCity<-(sum(as.numeric(EPL[EPL$HomeTeam=="Man City",4]), na.rm = TRUE))+(sum(as.numeric(EPL[EPL$AwayTeam=="Man City",5]), na.rm = TRUE))

Evrt<-(sum(as.numeric(EPL[EPL$HomeTeam=="Everton",4]), na.rm = TRUE))+(sum(as.numeric(EPL[EPL$AwayTeam=="Everton",5]), na.rm = TRUE))

LFC<-(sum(as.numeric(EPL[EPL$HomeTeam=="Liverpool",4]), na.rm = TRUE))+(sum(as.numeric(EPL[EPL$AwayTeam=="Liverpool",5]), na.rm = TRUE))

tott<-(sum(as.numeric(EPL[EPL$HomeTeam=="Tottenham",4]), na.rm = TRUE))+(sum(as.numeric(EPL[EPL$AwayTeam=="Tottenham",5]), na.rm = TRUE))

Top7=(Ars+Cfc+ManUtd+ManCity+Evrt+LFC+tott)




levels(Season)





# Correlation between Points and Salary



X <- SumAwayGoals
Y <- SumHomeGoals

# calculation of the mean
mean_x = (1/n)*sum(X)
mean_y = (1/n)*sum(Y)
mean_x
mean_y

# calculation of the variance
var_X = (1/(n-1))*sum((X-mean_x)^2)
var_Y = (1/(n-1))*sum((Y-mean_y)^2)
#var_X = (1/(n-1))*sum((X-mean_x))  ------ near to 0
var_X
var_Y

# calculation of the standard deviation
sd_X = sqrt(var_X)
sd_Y = sqrt(var_Y)
sd_X
sd_Y

# calculation of the covariance
cov_XY = (1/(n-1))*sum((X-mean_x)*(Y-mean_y))
cov_XY

# calculation of the correlation
cor_XY = cov_XY/(sd_X*sd_Y)
cor_XY







#Preceting of goals by game
ggplot(data = EPL, aes(x =FTHG+FTAG)) +
  geom_histogram()+
  ggtitle("Goals per game")+
  scale_x_continuous(name = "Goals",
                     limits = c(0, 10),
                     breaks = seq(0, 10, by = 0.2)) +
  scale_y_continuous(name = "Games",
                     limits = c(0, 1500),
                     breaks = seq(0, 1500, by = 100))




#Home Team Goals
ggplot(data = EPL, aes(x = HomeTeam, y = FTHG)) +
  geom_boxplot()+
  ggtitle("Home Games")+
  xlab("Teams")+
  scale_y_continuous(name = "Goals",
                     limits = c(0, 10),
                     breaks = seq(0, 10, by = 1))+
theme(axis.text.x = element_text(angle = 90, hjust = 1))

summary(Full_Time_Home_Team_Goals)



#Away Team Goals
ggplot(data = EPL, aes(x = AwayTeam, y = FTAG)) +
  geom_boxplot()+
  ggtitle("Away Games")+
  xlab("Teams")+
  scale_y_continuous(name = "Goals",
                     limits = c(0, 10),
                     breaks = seq(0, 10, by = 1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
summary(Full_Time_Away_Team_Goals)


##Home team goals in W/D/L

ggplot(EPL, aes(x = FUll_Time_Result, y =Full_Time_Home_Team_Goals)) + 
  geom_boxplot()+
ggtitle("Result (Home team)")+
  xlab("Teams")+
  scale_y_continuous(name = "Goals",
                     limits = c(0, 10),
                     breaks = seq(0, 10, by = 1))




#Away team goals in W/D/L

ggplot(EPL, aes(x = FUll_Time_Result, y =Full_Time_Away_Team_Goals)) + 
  geom_boxplot()+
  ggtitle("Result(Away Team")+
  xlab("Teams")+
  scale_y_continuous(name = "Goals",
                     limits = c(0, 10),
                     breaks = seq(0, 10, by = 1)
                     )








#Games by squad
ggplot(EPL, aes(x = HomeTeam, fill = HomeTeam)) + 
  geom_bar()+
  ggtitle("Games Played")+
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  





# percentage  Of home team goals per season
boxplot(Full_Time_Home_Team_Goals ~ Season,
        col = Season,
        main = 'percentage  Of Home team goals per season',
        xlab = 'season', ylab = 'goals',
        border = 'gray', notch = T, cex.axis = 1,
        
)

# percentage  Of away team goals per season
boxplot(Full_Time_Away_Team_Goals ~ Season,
        col = Season,
        main = 'percentage  Of away team goals per season',
        xlab = 'season', ylab = 'goals',
        border = 'gray', notch = T, cex.axis = 1,
        
)

#  percentage  Of total goals per season
boxplot(Full_Time_Away_Team_Goals+Full_Time_Home_Team_Goals ~ Season,
        col = Season,
        main = ' percentage  Of total goals per season',
        xlab = 'season', ylab = 'goals',
        border = 'blue', notch = T, cex.axis = 1,
        
)




summary(EPL[EPL$FTR=='H',4])
summary(EPL[EPL$FTR=='H',5])
summary(EPL[EPL$FTR=='A',4])
summary(EPL[EPL$FTR=='A',5])
summary(EPL[EPL$FTR=='D',4])
summary(EPL[EPL$FTR=='D',5])





shell("cls")
rm(list = ls())

  