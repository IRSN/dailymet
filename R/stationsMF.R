##' @name stationsMF
##' @title Stations Météo-France
##' @description  A data frame with the following columns
##' \itemize{
##'      \item{Code, Name }{Code and name.}
##'      \item{Lat, Lon, Alt }{
##' 
##'         Latitude, Longitude (decimal degrees) and Altitude
##'         (metres)
##'
##'       }
##'       \item{LongName }{
##'
##'          The long "exact" name in correct French. Mind spaces,
##'          accents and other special characters. Also compound names
##'          such can be a combination of two names, which is classical
##'          for airports.
##' 
##'       }
##'       \item{Clim }{A factor giving the climatic class, with the followin codes. 
##'           \itemize{
##'              \item{\code{"C"} }{\emph{Modified Oceanic}, mainly for Central zone
##'                located at the North of Clermont-Ferrand.}
##'              \item{\code{"Med"} }{\emph{Mediterranean}. This coevers the whole
##'                    French Mediterranean coastline and at the South of Valence,
##'                    including Toulouse.}
##'              \item{\code{"NW"} }{\emph{North-West Oceanic}, mainy for
##'                 North-West. This covers the Atlantic coastline from les Sables
##'                 d'Olonne (South) to Dunkerque (North) and covers
##'                 French Brittany, Normandy and Picardie.}
##'              \item{\code{"SW"} }{\emph{Aquitain Oceanic}, mainy for South-West.
##'                 This covers the Atlantic coasline from les Sables d'Olonnes (North)
##'                 to Hendaye (South).}
##'              \item{\code{"NE"} }{\emph{Semi-Continental}, mainy for North-East.}
##'              \item{\code{"Mont"} }{\emph{Montaneaous}. The Massif Central and
##'                 the French Alpes, and the French part of the Pyrenees.}
##'              \item{\code{""} }{Special: Tropical, Artic, ...} 
##'           }
##'       }
##'
##' }
##' 
##' @importFrom utils read.table
##' @export
##' @docType data
##' 
stationsMF <- 
    read.table(file = system.file("extdata", "postesSynop.csv",
                                      package = "dailymet"),
               header = TRUE, sep = ";",
               colClasses = c("Code" = "character",
                              "Name" = "character",
                              "Lat" = "numeric",
                              "Lon" = "numeric",
                              "Alt" = "numeric",
                              "LongName" = "character",
                              "ShortName" = "character",
                              "Zone" = "factor",
                              "Desc" = "character",
                              "OldCode" = "character",
                              "Resol40" = "integer",
                              "Clim"= "factor"))

