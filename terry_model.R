data <- data.frame(
  Winner = c("中信兄弟", "味全龍", "統一", "樂天", "台鋼","富邦"),
  中信兄弟 = c(NA, 7,8,7,8,8),
  味全龍 = c(5, NA, 6,6,6,9),
  統一 = c(4,6, NA, 5,6,8),
  樂天 = c(5,6,6, NA, 5,7),
  台鋼 = c(4,6,6,7, NA,3),
  富邦 = c(4,3,4,5,8,NA)
)
table_result <- as.matrix(data[,-1])
rownames(table_result) <- data$Winner
table_result<-t(table_result)

library(BradleyTerry2)
Head2Head <- countsToBinomial(table_result)
names(Head2Head)[3:4] <- c("Win", "Lose")
#------------------------------------------------------------
model1<-BTm(cbind(Win, Lose), player1, player2, formula=~player, id="player",
             refcat="中信兄弟", data=Head2Head)
summary(model1)
BTabilities(model1)
#-------------------------------------------------------------
model2<-BTm(cbind(Win, Lose), player1, player2, formula=~player, id="player",
            refcat="味全龍", data=Head2Head)
summary(model2)
BTabilities(model2)
#------------------------------------------------------------------
model3<-BTm(cbind(Win, Lose), player1, player2, formula=~player, id="player",
            refcat="統一", data=Head2Head)
summary(model3)
BTabilities(model3)
#-------------------------------------------------------------------------------------
model4<-BTm(cbind(Win, Lose), player1, player2, formula=~player, id="player",
            refcat="樂天", data=Head2Head)
summary(model4)
BTabilities(model4)
#-----------------------------------------------------------------------------------
model5<-BTm(cbind(Win, Lose), player1, player2, formula=~player, id="player",
            refcat="台鋼", data=Head2Head)
summary(model5)
BTabilities(model5)
#----------------------------------------------------------------------------------
model6<-BTm(cbind(Win, Lose), player1, player2, formula=~player, id="player",
            refcat="富邦", data=Head2Head)
summary(model6)
BTabilities(model6)
#-----------------------------------------------------------------------------------
library(qvcalc)
tennis.qv <- qvcalc(BTabilities(model1))
plot(tennis.qv, levelNames = c("中信兄弟", "味全龍", "統一", "樂天", "台鋼","富邦"))

