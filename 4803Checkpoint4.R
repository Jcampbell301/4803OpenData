#Reading in the data and creating the model
cbbdata21 <- read.csv("cbb21.csv", header = TRUE)

cbbdata21$MadeTournament <- cbbdata21$SEED > 0
cbbdata21$Bracket <- ifelse(cbbdata21$MadeTournament == TRUE, 1, 0)
cbbdata21[is.na(cbbdata21)] = 0

BballModel = lm(cbbdata21$Bracket ~ cbbdata21$EFG_O + cbbdata21$EFG_D + cbbdata21$TOR +
                  cbbdata21$TORD + cbbdata21$ORB + cbbdata21$DRB + cbbdata21$FTR +
                  cbbdata21$FTRD + cbbdata21$X2P_O + cbbdata21$X2P_D + cbbdata21$X3P_O +
                  cbbdata21$X3P_D)

summary(BballModel)
anova(BballModel)

#Finding the residuals and creating a plot
BballModel.res = resid(BballModel)

plot(density(BballModel.res))
qqline(BballModel.res)
qqnorm(BballModel.res)

#Finding the correlation between variables
library(corrplot)

only_variables = cbbdata21[7:20]

correlations = cor(only_variables)
corrplot(correlations, method="circle")

# Prediction of making the tournament
updated_model <- lm(cbbdata21$Bracket ~ cbbdata21$EFG_O + cbbdata21$TOR + cbbdata21$TORD + cbbdata21$ORB)

prediction_prob <- predict(updated_model, cbbdata21)
over_50 <- subset(prediction_prob, cbbdata21$Bracket == 1 & cbbdata21$SEED == 1)
show(over_50)

barplot(over_50)


# Second model to see who will win the tournament
tournament_data <- cbbdata21[1:68,]
tournament_data$TournamentWins <- rbind(3, 6, 1, 5, 1, 0, 4, 2, 1, 0, 1, 3, 
                                        2, 0, 0, 1, 2, 0, 2, 1, 1, 0, 3, 0,
                                        1, 0, 0, 2, 1, 0, 1, 2, 0, 0, 1, 0,
                                        1, 0, 1, 0, 0, 0, 2, 3, 0, 0, 0, 3,
                                        0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 2,
                                        0, 0, 0, 0, 0, 0, 0, 0)

tournamentModel <- lm(tournament_data$TournamentWins ~ tournament_data$EFG_O + tournament_data$EFG_D + tournament_data$TOR + tournament_data$TORD + tournament_data$ORB + tournament_data$DRB + tournament_data$FTR + tournament_data$FTRD + tournament_data$X2P_O + tournament_data$X2P_D + tournament_data$X3P_O + tournament_data$X3P_D)

summary(tournamentModel)
anova(tournamentModel)

TorneyModel.res = resid(tournamentModel)
plot(density(TorneyModel.res))
qqnorm(TorneyModel.res)
qqline(TorneyModel.res)

library(corrplot)
tournament_cut = tournament_data[5:20]

# Creating Correlation Matrix

correlations = cor(tournament_cut)
corrplot(correlations, method="circle")

# Updated model and looking at probability the number 1 seeds won the tournament
updated_tournament_model <- lm(tournament_data$TournamentWins ~ tournament_data$EFG_O + tournament_data$TOR + tournament_data$ORB)

pred_prob <- predict(updated_tournament_model, tournament_cut)
over_50 <- subset(pred_prob, tournament_data$SEED == 1)

show(over_50)
barplot(over_50)

