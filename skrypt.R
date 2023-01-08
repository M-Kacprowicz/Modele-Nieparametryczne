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


create_plot <- function(variable) {
  ggplot(proba_uczaca , aes(x=factor(variable), fill=factor(variable))) + 
    geom_bar() +
    theme(legend.position="none")
}

table(proba_uczaca$expiryyear)
create_plot(proba_uczaca$expiryyear)

table(proba_uczaca$issuer)
create_plot(proba_uczaca$issuer)

table(proba_uczaca$mccname)
create_plot(proba_uczaca$mccname)

table(proba_uczaca$recurringaction)
create_plot(proba_uczaca$recurringaction)

table(proba_uczaca$type)
create_plot(proba_uczaca$type)

table(proba_uczaca$description)
create_plot(proba_uczaca$description)

table(proba_uczaca$level)
create_plot(proba_uczaca$level)

table(proba_uczaca$status)
create_plot(proba_uczaca$status)

table(proba_uczaca$countrycode)
create_plot(proba_uczaca$countrycode)

table(proba_uczaca$expirymonth)
create_plot(proba_uczaca$expirymonth)

table(proba_uczaca$listtype)
create_plot(proba_uczaca$listtype)

table(proba_uczaca$acquirerconnectionmethod)
create_plot(proba_uczaca$acquirerconnectionmethod)


missing_data_plot <- aggr(proba_uczaca, col=c('forestgreen','firebrick1'),
                          numbers=TRUE, sortVars=TRUE,
                          labels=names(proba_uczaca), cex.axis=.7,
                          gap=3, ylab=c("Missing data","Pattern"))





# Analizując braki danych znaleziono 4 zmienne w których występują braki danych na poziomie 86%. Wstępnie usunięte
# zostaną 3 zmienne, które wydają się nie mieć wpływu na model predykcyjny, który chcemy utworzyć.

proba_uczaca <- subset(proba_uczaca, select = -c(browseragent, screenheight, screenwidth))

# Imputacja braków danych w zmiennej "payclickedtime"
proba_uczaca$time_difference <- proba_uczaca$payclickedtime - proba_uczaca$createtime
mean_difftime <- mean(proba_uczaca$time_difference, na.rm = T)
proba_uczaca$payclickedtime_imp <- proba_uczaca$createtime + mean_difftime
proba_uczaca <- proba_uczaca %>%
  mutate_at(c("time_difference"), ~replace_na(.,mean_difftime))
proba_uczaca <- proba_uczaca %>% 
  mutate(payclickedtime = coalesce(payclickedtime, payclickedtime_imp))


# Sprawdzanie braków danych

missing_data_plot <- aggr(proba_uczaca, col=c('forestgreen','firebrick1'),
                          numbers=TRUE, sortVars=TRUE,
                          labels=names(proba_uczaca), cex.axis=.7,
                          gap=3, ylab=c("Missing data","Pattern"))
  