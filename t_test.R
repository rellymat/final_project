library(data.table)

players = nbastats::seasons_stats
players = data.table(year = players$Year, pos = players$Pos, games = players$G, mp = players$MP)
players = na.omit(players)
players = players[, avg_min_ply := mp/games, by=year]
players = players[, avg_min_pos := mean(avg_min_ply), by=paste0(year, pos)]
players = players[, num := .N, by=year]
center = players[pos == "C",]
center = unique(center[ ,c("games","mp") := list(NULL, NULL)])

make_table <- function(center, number){

  p.value = c()

  for (i in number:(number + 10)) {
    x = center[year == i, ]$avg_min_ply
    temp = i + 2007 - number
    y = center[year == temp, ]$avg_min_ply
    p.value[i- number + 1] = t.test(x,y, mu=0)$p.value
  }
  
  center = center[, num_c := .N, by=year]
  center = center[, ratio := num_c/num]
  center = unique(center[ ,avg_min_ply := NULL])

  center_1977_1987 = center[year > number - 1 & year < number + 11, ]
  center_2007_2017 = center[year > 2006, ]

  both = data.table(YEAR_1 = center_1977_1987$year, YEAR_2 = center_2007_2017$year,
                  AVG_MIN_1 = center_1977_1987$avg_min_pos, AVG_MIN_2 = center_2007_2017$avg_min_pos)
  both = both[, c("DIFF_AVG","P.VAL") := list(AVG_MIN_1 - AVG_MIN_2, p.value)]
  both = both[, SIG := ifelse(P.VAL < 0.001 , "***", ifelse(P.VAL < 0.01, "**", ifelse(P.VAL < 0.05, "*","")))]
  both
}


print(make_table(center,1977))
