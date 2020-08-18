library(data.table)
library(caret)
library(FactoMineR)
library(gridExtra)
library(dplyr)

players_salary = fread("c://Users/Harel/Downloads/big_data_files/final_project/nba_final.csv")
players_salary = players_salary[, select(players_salary, Pos1, Age, G:TOV, PTS:Salary)]
players_salary = na.omit(players_salary)
p_s_c = players_salary[Pos1 == "C"]
p_s_c = p_s_c[, Pos1 := NULL]

p_s_c = p_s_c[, sa := Salary]
inTrain = createDataPartition(p_s_c$Salary, p=0.6, list=FALSE)
training = p_s_c[inTrain,]
testing = p_s_c[-inTrain,]

y = data.table(cor = cor(p_s_c[,1:ncol(p_s_c)], p_s_c$Salary), name = colnames(p_s_c))
x = y[cor.V1 > 0.52,]
x = x[!(x$name %in% list("X2P","DRB","FG")),]
training = subset(training, select = x$name)


modRF = train(Salary~., method = "ranger", data = training) 
predicSalary = predict(modRF, newdata = testing)
testing = cbind(testing, predicSalary) 
testing = testing[,  Accuracy := (1 - abs(sa - predicSalary)/sa)*100]
testing = testing[, sa := NULL]




