#*****************************************************************************#
# Modele Nieparametryczne                                                     #
#*****************************************************************************#



# Inicjalizacja bibliotek



library(rpart)
library(rpart.plot)
library(VIM)
library("dplyr")
library("tidyr")
library("lubridate")



# Ładowanie danych



load(file="dane_zaliczenie.RData")



# Sprawdzanie braków danych



missing_data_plot <- aggr(proba_uczaca, col=c('forestgreen','firebrick1'),
                          numbers=TRUE, sortVars=TRUE,
                          labels=names(proba_uczaca), cex.axis=.7,
                          gap=3, ylab=c("Missing data","Pattern"))



# Analizując braki danych znaleziono 4 zmienne w których występują braki danych na poziomie 86%. Wstępnie usunięte
# zostaną 3 zmienne, które wydają się nie mieć wpływu na model predykcyjny, który chcemy utworzyć.



proba_uczaca <- subset(proba_uczaca, select = -c(browseragent, screenheight, screenwidth))



# Sprawdzanie braków danych



missing_data_plot <- aggr(proba_uczaca, col=c('forestgreen','firebrick1'),
                          numbers=TRUE, sortVars=TRUE,
                          labels=names(proba_uczaca), cex.axis=.7,
                          gap=3, ylab=c("Missing data","Pattern"))



# Imputacja braków danych w zmiennej "payclickedtime"
proba_uczaca$time_difference <- proba_uczaca$payclickedtime - proba_uczaca$createtime
mean_difftime <- mean(proba_uczaca$time_difference, na.rm = T)
proba_uczaca <- proba_uczaca %>%
  mutate_at(c("time_difference"), ~replace_na(.,mean_difftime))
proba_uczaca <- proba_uczaca %>%
  mutate_at(c("payclickedtime"), ~replace_na(.,createtime + mean_difftime))
