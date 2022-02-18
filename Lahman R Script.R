library(Lahman)
library(RSQLite)
library(sqldf)
library(dplyr)

#VIEW TABLES IN THE DATABASE
LahmanData
#LOAD TABLES
data(Parks, package="Lahman")
data(Pitching, package = "Lahman")
data(Batting, package = "Lahman")
data(People, package = "Lahman")

#TAKE A LOOK AT THE TABLE STRUCTURE
str(Parks)
str(Pitching)
str(Batting)
str(People)

#TAKE A LOOK AT THE DATA
head(Parks)
head(Pitching)
tail(Batting)
tail(People)

#Display the first 10 ballparks in the ballparks table.
sqldf("SELECT * FROM Parks Limit 10")

#Make a list of the names of all of the inactive teams in baseball history.
sqldf("SELECT FranchName FROM TeamsFranchises WHERE active = 'N'")

#ASK SOME QUESTIONS

#List the pitchers with their teamID, wins (W), and losses (L) that threw
#complete games (CG) in the 1995 season. Include their number of
#complete games as well.
realpitchers <-sqldf("SELECT playerID, teamID, W, L, CG FROM Pitching WHERE CG > 0
      AND yearID = 1995 ORDER BY W DESC")


#1a.How many players threw 3 or more complete games?
realpitchers <-sqldf("SELECT playerID, teamID, W, L, CG FROM Pitching WHERE CG > 3
      AND yearID = 1995 ORDER BY W DESC")

#1b.Find the first and last name of the players?
realpitchersbyname <-sqldf("Select a.*, b.nameFirst, b.nameLast From realpitchers a
                           Left Join People b Where a.playerID = b.playerID")

#2.How many current Major League Teams have never won the World Series?

losers <-sqldf("Select teamID, yearID, franchID, WSWin From Teams Where WSWin = 'N' And yearID > 1900")

#3.How many current and former Major League Players attended TCU? Include
#  first and last name, and teams for whom they played

hornedfrogs <-sqldf("Select distinct(playerID), schoolID from CollegePlaying Where schoolID = 'txchrist'")
hornedfrogsbyname <-sqldf("Select a.*, b.namefirst, b.namelast From hornedfrogs a
                          Left Join People b on a.playerID = b.playerID")

#4.Who is the youngest player to play in the Major Leagues?

age <-sqldf("Select playerID, birthYear, birthmonth, birthDay, debut, (debut-birthYear) As Diff
      From People Order by Diff asc")
youngest <-sqldf("Select * From age where Diff is not null")

#5.Obtain a list of all players who had more than 100 At-Bats (AB) during
#  the 2018 and 2019 seasons.  Be sure to include First and Last Name and
#  all stats from the Batting Table. Create an object for the result and 
#  write the ouput as a .csv file entitled player_stats.



players <-sqldf("Select a.nameFirst, a.nameLast, b.* From People a 
                Left Join Batting b on b.playerID = a.playerID Where yearID = 2018
                or yearID = 2019")
player_stats <-sqldf("Select * From players Where AB > 100")
players2018 <-sqldf("select * from player_stats where yearID = 2018")
players2019 <-sqldf("select * from player_stats where yearID = 2019")
write.csv(players2018, "players2018.csv")
write.csv(players2019, "players2019.csv")