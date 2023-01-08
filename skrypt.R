#*****************************************************************************#
# Modele Nieparametryczne                                                     #
#*****************************************************************************#

# Inicjalizacja bibliotek

library(rpart)
library(rpart.plot)
install.packages("VIM")
library(VIM)

# Ładowanie danych

load(file="dane_zaliczenie.RData")
View(proba_uczaca)
table(proba_uczaca$amount)

# Sprawdzanie braków danych

missing_data_plot <- aggr(proba_uczaca, col=c('forestgreen','firebrick1'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(proba_uczaca), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))

# Analizując braki danych znaleziono 4 zmienne w których występują braki danych na poziomie 86%.
