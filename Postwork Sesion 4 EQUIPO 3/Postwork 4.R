#Postwork Sesion 4
#Equipo 3
#Integrantes

#Importar datos
temporada1718 <- read.csv('https://www.football-data.co.uk/mmz4281/1718/SP1.csv')
temporada1819 <- read.csv('https://www.football-data.co.uk/mmz4281/1819/SP1.csv')
temporada1920 <- read.csv('https://www.football-data.co.uk/mmz4281/1920/SP1.csv')

#Aplicar funciones a todos nuestros datasets
temporadas <- list(temporada1718,temporada1819,temporada1920);
temporadas2 <- lapply(temporadas, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR);

temporadas2[[1]] <- mutate(temporadas2[[1]], Date = as.Date(Date, "%d/%m/%y"))
temporadas2[[2]] <- mutate(temporadas2[[2]], Date = as.Date(Date, "%d/%m/%y"))
temporadas2[[3]] <- mutate(temporadas2[[3]], Date = as.Date(Date, "%d/%m/%y"))

#Un solo dataset
dataTemporadas <- do.call(rbind, temporadas2)

# La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x=0,1,2,)
golesCasa <- round(table(dataTemporadas$FTHG)/nrow(dataTemporadas),4)
# La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y=0,1,2,)
golesVisita <- round(table(dataTemporadas$FTAG)/nrow(dataTemporadas),4)
# La probabilidad (conjunta) de que el equipo que juega en casa anote x goles 
#y el equipo que juega como visitante anote y goles (x=0,1,2,, y=0,1,2,)
golesConjunta <- round(table(dataTemporadas$FTHG,dataTemporadas$FTAG)/nrow(dataTemporadas),4)


