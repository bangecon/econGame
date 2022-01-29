##' Tabulate ultimatum game results.
##'
##' Tabulates and assigns points for the results of a simple in-class ultimatum game.
##'
##' @details \code{ultimatumGame} tabulates the results of a simple ultimatum game based on Thaler (1988). Students form pairs and determine a "proposer" and a "responder" by flipping a coin (or some other fair mechanism). The pair receives 5 "extra credit" points to share. The proposer offers to give a number of points to the responder (between 0 and 5) and the responder decides whether to accept or reject. If the responder accepts, then the pair shares the points according to the aggreement; if the responder rejects, both players get nothing.
##'
##' @param sheet  (required) is a character string url corresponding to the Google Sheets location containing the individual submissions.
##' @return \code{type} returns the type of activity (ultimatumGame).
##' @return \code{results} returns the original submissions.
##' @return \code{grades} returns the aggregated, stacked points "won" by each student for the entire activity.
##'
##' @references Holt, Charles A. (1996). Classroom games: Trading in a pit market. \emph{Journal of Economic Perspectives,} 10(1), pp.193-203.
##'
##' @export

ultimatumGame <- function(sheet, ...) {
  # Set up the Google Sheets, read responses, and initialize output objects.
  results <- read_sheet(sheet)
  colnames(results) <- make.names(colnames(results))
  results <-
    replace_na(
      results,
      list(
        Proposer.First.Name = "John",
        Proposer.Last.Name = "Doe",
        Responder.First.Name = "John",
        Responder.Last.Name = "Doe"
      )
    )
  results$Proposer.First.Name <-
    str_to_title(results$Proposer.First.Name)
  results$Proposer.Last.Name <-
    str_to_title(results$Proposer.Last.Name)
  results$Responder.First.Name <-
    str_to_title(results$Responder.First.Name)
  results$Responder.Last.Name <-
    str_to_title(results$Responder.Last.Name)
  # Calculate the proposers' grades
  grades.proposers <-
    results[, which(
      names(results) %in% c(
        "Proposer.Last.Name",
        "Proposer.First.Name",
        "Offer",
        "Response"
      )
    )]
  colnames(grades.proposers)[1:2] <- c("Last.Name", "First.Name")
  grades.proposers$points <-
    ifelse(grades.proposers$Response == "Rejected",
           0,
           5 - grades.proposers$Offer)
  # Calculate the responders' grades
  grades.responders <-
    results[, which(
      names(results) %in% c(
        "Responder.Last.Name",
        "Responder.First.Name",
        "Offer",
        "Response"
      )
    )]
  colnames(grades.responders)[1:2] <- c("Last.Name", "First.Name")
  grades.responders$points <-
    ifelse(grades.proposers$Response == "Rejected",
           0,
           grades.proposers$Offer)
  grades <- rbind(grades.proposers, grades.responders)
  out <- list(type = "ultimatumGame",
              results = results[order(results$Proposer.Last.Name, results$Proposer.First.Name),],
              grades = grades[order(grades$Last.Name, grades$First.Name),])
  class(out) <- c('econGame', class(out))
  out
}
