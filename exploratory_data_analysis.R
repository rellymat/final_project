library(data.table)

players = fread("c://Users/Harel/Downloads/big_data_files/final_project/players.csv")
players_1950_2018 = data.table(nbastats::player_data$year_start, nbastats::player_data$year_end,
                                                                  nbastats::player_data$position)
players_1950_2018 = players_1950_2018[V1 > 1949 & V2 > 1949,]

year_position = data.table( year = integer(), position = character())

for(i in 1:nrow(players_1950_2018)){
  temp = players_1950_2018[i, ]
  size = ifelse(temp$V2 == 2018, temp$V2, temp$V2-1)
   for (j in temp$V1:size) {
    year_position = rbind(year_position, data.table( year = j, position = temp$V3))
  }
}


psn = fread("c://Users/Harel/Downloads/big_data_files/final_project/NBA Data With Salaries.csv")
psn = data.table(year = psn$Year, salary = psn$Salary, pos = psn$Pos)
psn = transform(psn, salary = as.double(salary))
psn = na.omit(psn)
psn = psn[salary > 0,]
psn = psn[, s_p := mean(salary), by=paste0(year,pos)]
psn = psn[, s_y := mean(salary), by=year]
psn = psn[, ratio := s_p/s_y]
psn = psn[, pos_num := .N, by=paste0(pos,year)]
psn = psn[, year_num := .N, by=year]
c = psn[pos == "C",]
#plot(c$year,c$ratio, xlab="year", ylab="ratio")
temp = c[,ratioT := pos_num/year_num]
plot(temp$year, temp$s_p, xlab="year", ylab="salary_avg", yaxt="n", col = "red", pch = 19)
mark = c(0,1700000, 3400000, 5100000,6600000)
axis(2, at=mark, labels=mark)
points(temp$year, temp$s_y, col="blue", pch = 19)
legend("topleft", pch=c(19, 19), col=c("red", "blue"),legend=c("centers_avg", "players_avg"))

year_position = year_position[, y_pos := .N, by=paste0(year,position)]
year_position = year_position[, y_ply := .N, by=year]

center_forward = unique(year_position[position == "C-F" | position == "F-C" , ])
center = unique(year_position[position == "C", ])
center_forward = center_forward[, y_pos := sum(y_pos), by=year]
center = unique(center[, position := NULL])
center_forward = unique(center_forward[, position := NULL])
center = center[y_ply > 200,]
center_forward = center_forward[y_ply > 200,]
center = center[, y_pos := sum(y_pos), by=year]
center_forward = center_forward[, y_pos := sum(y_pos), by=year]
center = center[, ratio := y_pos/y_ply]
center_forward = center_forward[, ratio := y_pos/y_ply]
#plot(center_forward$year, center_forward$ratio, col="red", ylim = c(0.07,0.26), xlab="year", ylab="ratio")
#points(center$year, center$ratio, xlab= "year", ylab = "ratio", col = "blue")
#legend("topright", pch=c(1, 1), col=c("red", "blue"),legend=c("C-F", "C"))
