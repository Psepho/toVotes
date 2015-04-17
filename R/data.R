#' Data from the mayoral elections in Toronto.
#'
#' Cleaned and packaged poll by poll results from the 2003, 2006,
#' 2010, and 2014 mayoral elections in Toronto.
#'
#' \itemize{
#'   \item year. The year of the election as a factor
#'   \item candidate. The name of the candidate as last name first name
#'   \item votes. The number of votes received
#'   \item ward. The ward for the polling station
#'   \item area. The area for the polling station
#'   \item type. The type of election, currently only Mayor
#'   ...
#' }
#'
#' @format A data frame with 388586 rows and 12 variables
#' @source \url{http://www1.toronto.ca/wps/portal/contentonly?vgnextoid=834689fe9c18b210VgnVCM1000003dd60f89RCRD}
#' @name toVotes
NULL
#' Geographic data for the polling stations in Toronto municipal elections.
#'
#' Geocoded poll locations from the 2003, 2006,
#' 2010, and 2014 municipal elections in Toronto.
#'
#' \itemize{
#'   \item long. The longitude of the polling station
#'   \item lat. The latitude of the polling station
#'   \item order. Geocoding of the polling station
#'   \item hole. Geocoding of the polling station
#'   \item piece. Geocoding of the polling station
#'   \item group. Geocoding of the polling station
#'   \item id. A character string with two characters for ward and three for area
#'   \item year. The year in which the polling station was used
#'   \item ward. The ward of the polling station
#'   \item area. The area of the polling station
#'   \item ward name. The name of the ward containing the polling station
#'   ...
#' }
#'
#' @format A data frame with 387422 rows and 12 variables
#' @name toPollGeo
NULL
#' Data from the 2008 and 2011 federal elections in Toronto.
#'
#' Cleaned and packaged poll by poll results from the 2008 and 2011 general federal elections for electoral districts in Toronto.
#'
#' \itemize{
#'   \item year. The year of the election as a factor
#'   \item candidate. The name of the candidate as last name first name
#'   \item votes. The number of votes received
#'   \item district. electoral district for the polling station
#'   \item poll. poll number for the polling station
#'   \item type. type of election as federal
#'   \item party. the party affiliation of the candidate as a factor
#'   \item incumbent. a logical value indicating if the candidate was the incumbent
#'   ...
#' }
#'
#' @format A data frame with 39730 rows and 8 variables
#' @source \url{http://www.elections.ca/}
#' @name toFederalVotes
NULL
#' Turnout data from the mayoral elections in Toronto.
#'
#' Cleaned and packaged turnout statistics from from the 2003, 2006,
#' 2010, and 2014 mayoral elections in Toronto.
#'
#' \itemize{
#'   \item year. The year of the election as a factor
#'   \item ward. The ward of the polling station
#'   \item area. The area of the polling station
#'   \item prop_voted. The proportion of eligible votes that voted
#'   \item type. type of election as mayoral
#'   ...
#' }
#'
#' @format A data frame with 6494 rows and 5 variables
#' @source \url{http://www1.toronto.ca/wps/portal/contentonly?vgnextoid=9979040a8d88d310VgnVCM10000071d60f89RCRD}
#' @name toTurnout
NULL
