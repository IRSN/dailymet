## *****************************************************************************
##' @name stationsMF
##' @title Stations Météo-France
##' @description  A data frame with the following columns
##' \itemize{
##'      \item{\code{Code}, \code{Name} }{
##'         Code and name.
##'      }
##'      \item{\code{Lat}, \code{Lon}, \code{Alt} }{
##'         Latitude, Longitude (decimal degrees) and Altitude
##'         (metres)
##'       }
##'       \item{\code{LongName} }{
##'          The long "exact" name in correct French. Mind spaces,
##'          accents and other special characters. Also compound names
##'          such can be a combination of two names, which is classical
##'          for airports.
##'       }
##'       \item{\code{Clim} }{
##'
##'           A factor giving the climatic class, with the following
##'           codes.
##' 
##'           \itemize{
##'              \item{\code{"C"} }{\emph{Modified Oceanic}, mainly
##'                  for a Central zone located at the North of
##'                  Clermont-Ferrand.}
##'              \item{\code{"Med"} }{\emph{Mediterranean}. This covers
##'                  the whole French Mediterranean coastline up to the
##'                  South of Valence, including Toulouse.}
##'              \item{\code{"NW"} }{\emph{North-West Oceanic}, mainy
##'                  for North-West. This covers the Atlantic coastline
##'                  from les Sables d'Olonne (South) to Dunkerque (North)
##'                  and covers French Brittany, Normandy and Picardie.}
##'              \item{\code{"SW"} }{\emph{Aquitain Oceanic}, mainy for
##'                  South-West. This covers the Atlantic coasline from
##'                  les Sables d'Olonnes (North) to Hendaye (South).}
##'              \item{\code{"NE"} }{\emph{Semi-Continental}, mainy for
##'                  North-East.}
##'              \item{\code{"Mont"} }{\emph{Montaneaous}. The Massif
##'                  Central and the French Alpes, and the French part
##'                  of the Pyrenees.}
##'              \item{\code{""} }{Special: Tropical, Artic, ...} 
##'           }
##'       }
##'
##'   The values given in \code{Clim} are derived from the Wikipedia
##'   page
##'   \url{https://fr.wikipedia.org/wiki/Climat_de_la_France}. Note
##'   however that the classification of stations is often done on the
##'   basis of \emph{yearly} variables, hence can not fully account
##'   for the variations that are seen at the daily level.
##' 
##' }
##' 
##' @importFrom utils read.table
##' @export
##' @docType data
##' 
##' @examples
##' \dontrun{
##'     library(leaflet)
##'     m <- leaflet() %>% addTiles;
##'     m <- m %>% fitBounds(lng1 = -4.5, lat1 = 42.0, lng2 = 7.9, lat2 = 50.8)
##'     m <- m %>% setView(2, 47.4, zoom = 6)
##'     m <- m %>% addMarkers(lng = stationsMF[ , "Lon"],
##'                           lat = stationsMF[ , "Lat"],
##'                           popup = stationsMF[ , "ShortName"])
##'     m
##' }
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

##******************************************************************************
##' Find a "Météo France" station using a description, code or name.
##' 
##' @title Find a "Meteo France" Station using a Description, Code or
##'     Name
##' 
##' @param desc A character string to be matched against the
##'     description of the station.
##'
##' @param code A character string to be matched against the code of
##'     the station.
##'
##' @param name A character string to be matched against the name of
##'     the station. Character case and accents are likely to generate
##'     problems.
##'
##' @param one Logical. If \code{TRUE} the selection must correspond
##'     to exactly one station.
##'
##' @return A one-row data frame with several elements giving the
##'     code, the name, the directory name for the station.  The
##'     column \code{Dir} gives the (a possible) name of for a
##'     directory containing the data.
##'
##' @note By construction, this function can only find \emph{one}
##'     function in order to allow automated treatments. When several
##'     stations match, an error with a suitable message is thrown.
##'
##' @author Yves Deville
##'
##' @export
##' 
##' @examples
##'
##' findStationMF(desc = "rennes-st")
##' findStationMF(desc = "troy")
##' ## error
##' findStationMF(desc = "tro")
findStationMF <- function(desc = NULL,
                          code = NULL,
                          name = NULL,
                          one = TRUE) {
    
    nS <- nrow(stationsMF)
    if (!is.null(code)) {
        ind <- (1:nS)[stationsMF$Code == code]
    } else {
        if (is.null(desc)) {
            stop("'station' or 'code' must be given")
        } else {
            ind <- grep(tolower(desc), tolower(stationsMF$Desc))
        }
    }
    
    if (length(ind) == 0) {
        stop("station not found")
    }
    
    if (one && (length(ind) > 1)) {
        cat("Several matches found:\n")
        cat(paste(paste(1:length(ind),
                        stationsMF$Desc[ind],
                        stationsMF$Code[ind]), collapse = "\n"))
        cat("\n")
        stop("several stations match. Hint. use 'code ='")
    }
    
    desc <- stationsMF$Desc[ind]
    code <- stationsMF$Code[ind]
   
    stationWeb <- paste(desc, code, sep = "/")
    localDir <- paste(code, desc, sep = "_")

    data.frame(Desc = desc,
               Code = code,
               Name = stationsMF$Name[ind],
               Lat = stationsMF$Lat[ind],
               Lon = stationsMF$Lon[ind],
               Alt = stationsMF$Alt[ind],
               Dir = localDir)
    
}

