
library(dplyr)
library(reshape2)
library(ggplot2)



temporada1 <- "https://www.football-daata.co.uk/mmz4281/1718/SP1.csv"
temporada2 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
temporada3 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

temporada1_csv <- read.csv(file = temporada1) 

temporada2_csv <- read.csv(file = temporada2)
temporada3_csv <- read.csv(file = temporada3)


lista <- list(temporada1_csv, temporada2_csv,temporada3_csv)
temporadas <- lapply(lista, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

temporadas[[1]] <- mutate(temporadas[[1]], Date = as.Date(Date, "%d/%m/%y"))
temporadas[[2]] <- mutate(temporadas[[2]], Date = as.Date(Date, "%d/%m/%Y"))
temporadas[[3]] <- mutate(temporadas[[3]], Date = as.Date(Date, "%d/%m/%Y"))


dataf <- do.call(rbind, temporadas)

##probabildades conjuntas
(golescasa <- round(table(dataf$FTHG)/dim(dataf)[1], 3)) 

(golesvisita <- round(table(dataf$FTAG)/dim(dataf)[1], 3)) 

(pconjunta <- round(table(dataf$FTHG, dataf$FTAG)/dim(dataf)[1], 3))

#cociente

table <- pconjunta/outer(golescasa, golesvisita, "*")
table
