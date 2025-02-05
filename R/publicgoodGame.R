##' Tabulate public good game results.
##'
##' Tabulates and assigns points for the results of a simple in-class public good game.
##'
##' @details \code{publicgoodGame} tabulates the results of a simple public good game based on Leuthold (1993) and Holt and Laury (1997). Each student starts with an endowment of 5 points that they can either keep or anonymously and voluntarily contribute to a public good. For each point a student keeps, they receive only that point. For each point contributed to the public good, \emph{everyone} receives R/N points, where N is the number of participants and R > 1 is the return the group earns on public good contributions. In other words, students have the opportunity to boost their points if everyone contributes all of them. But, if only a few participants contribute, their net return could diminish.
##'
##'
##' @param sheet (required) is a character string url corresponding to the Google Sheets location containing the individual submissions.
##' @param endowment is the number of gifted points students start the game with (default is 5).
##' @param value is the amount that each point contributed to the public good returns to *each member of the class* (default is 0.1).
##' @param names character list of the column names in `sheet`.
##' @param auth is a logical indicating whether to use an authentication token to access the Sheet containing the individual submissions.
##' @param email is an email address that matches the user account containing the Sheet with the individual submissions (if `auth == TRUE`).
##'
##' @return \code{type} returns the type of activity (publicgoodGame).
##' @return \code{results} returns the original submissions.
##' @return \code{blinded esults} returns the submissions without student names.
##' @return \code{grades} returns the aggregated, stacked points "won" by each student for the entire activity.
##'
##' @references Holt, Charles A. and Laury, Susan K. (1997). Classroom Games: Voluntary Provision of a Public Good.
##' \emph{Journal of Economic Perspectives} 11(4), pp. 209-215.
##'
##' @export

publicgoodGame <- function(sheet,
                           endowment = 0,
                           value = 0.1,
                           auth = FALSE,
                           names = NULL,
                           email = NULL,
                           ...) {
  # Set up the Google Sheets, read responses, and initialize output objects.
  if(auth == TRUE) {
    options(gargle_oauth_cache = ".secrets")
    googlesheets4::gs4_auth()
    googlesheets4::gs4_deauth()
    googlesheets4::gs4_auth(cache = ".secrets", email = email)
  } else {
    googlesheets4::gs4_deauth()
  }
  if (is.null(names)) {
    names <- list(
      first = "First.Name",
      last = "Last.Name",
      contribution = "Contribution"
    )
  } else {
    names <- lapply(names, make.names)
  }
  results <- read_sheet(sheet)
  colnames(results) <- make.names(colnames(results))
  results <-
    replace_na(results,
               list(First.Name = "John",
                    Last.Name = "Doe"))
  results$First.Name <-
    str_to_title(results$First.Name)
  results$Last.Name <-
    str_to_title(results$Last.Name)
  N <- nrow(results)
  totalContributions <- sum(results$Contribution)
  individualReallocaitons <- totalContributions * value
  results$Reallocation <- individualReallocaitons
  results$Score <-
    endowment + results$Reallocation - results$Contribution
  blindedResults <-
    data.frame(results[,-which(names(results) %in% c("First.Name", "Last.Name", "Timestamp"))])
  grades <-
    with(results,
         as.data.frame(
           cbind(Last.Name, First.Name, Contribution, Reallocation, Score)
         ))
  out <- list(
    type = "publicgoodGame",
    results = results[order(results$Last.Name, results$First.Name), ],
    blindedResults = blindedResults[order(blindedResults$Contribution), ],
    grades = grades[order(grades$Last.Name, grades$First.Name), ]
  )
  class(out) <- c('econGame', class(out))
  out
}
