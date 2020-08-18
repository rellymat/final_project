library(data.table)
library(FactoMineR)
library(gridExtra)
players = fread("c://Users/Harel/Downloads/big_data_files/final_project/NBAPlayerTotals.csv")
players = players[, c("SeasonPlayer","Tm","Age") := list(NULL,NULL,NULL)]
players = players[, c("GS","eFG%","ORB","DRB","TOV") := list(NULL,NULL,NULL,NULL,NULL)]
players = players[, c("FG","FGA","PF","FT","FTA") := list(NULL,NULL,NULL,NULL,NULL)]
players = players[, c("MPPG","TRBPG","ASTPG","STLPG","BLKPG","PTSPG") := list(MP/G, TRB/G, AST/G, STL/G,
                                                                        BLK/G, PTS/G)]
players = players[, c("MP","TRB","AST","STL","BLK","PTS") := list(NULL,NULL,NULL,NULL,NULL,NULL)]
players = transform(players, Season = as.numeric(substr(Season,1,4)))

colnames(players)[which(names(players) == "2P")] <- "twop"
colnames(players)[which(names(players) == "2PA")] <- "twoAp"
colnames(players)[which(names(players) == "3P")] <- "threep"
colnames(players)[which(names(players) == "3PA")] <- "threeAp"
players = players[, c("2PG","2APG","3PG","3APG") := list(twop/G, twoAp/G, threep/G,threeAp/G)]
players = players[, c("twop","twoAp","threep","threeAp","G") := list(NULL,NULL,NULL,NULL,NULL)]


p_2009 = players[Season == 2009, ]
c_2009 = p_2009[Pos == "C", ]
p_2018 = players[Season == 2018, ]
c_2018 = p_2018[Pos == "C", ]

x = data.table(STAT = colnames(players[,4:17]))
x = x[,p_2009_10 := cor(p_2009[,4:17], p_2009$MPPG)]
x = x[,c_2009_10 := cor(c_2009[,4:17], c_2009$MPPG)]
x = x[,p_2018_19 := cor(p_2018[,4:17], p_2018$MPPG)]
x = x[,c_2018_19 := cor(c_2018[,4:17], c_2018$MPPG)]

#PCA(p_2009[,4:17], scale.unit = TRUE)
#PCA(c_2009[,4:17], scale.unit = TRUE)
#PCA(p_2018[,4:17], scale.unit = TRUE)
PCA(c_2018[,4:17], scale.unit = TRUE)
#grid.table(x)

