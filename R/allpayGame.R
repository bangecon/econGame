##' Tabulate entry and exit game results.
##'
##' Tabulates and assigns points for the results of a simple in-class entry and exit game using random values.
##'
##' @details \code{allpayGame} tabulates the results of a simple lobbying game based on a modified version of Goeree and Holt (1999). By default students receive an "endowment" of "extra credit" points (default is 5) at the beginning of a round of bidding. In one iteration, students "bid" on a monopoly license "prize" that is (default is 4 points) by making an anonymous number of "contributions" to politicians in order to influence the decision by the government. Each contribution costs 1 point, and is tabulated as the number of times a student submits the Google Form. The winner of the auction is determined by a lottery from a random row sampled from the responses.
##'
##' @param sheet  (required) is a character string url corresponding to the Google Sheets location containing the individual submissions.
##' @param endowment is the size of the initial endowment of points the instructor wishes to give each student.
##' @param prize is the value of the license or prize each interest group (student) is bidding on.
##' @return \code{type} returns the type of activity (allpayGame).
##' @return \code{results} returns the original submissions (with market prices and points per round added).
##' @return \code{winner} returns the name of the winner and the size of the prize.
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @references Goeree, Jacob K. & Charles A. Holt (1999). Classroom Games: Rent-Seeking and the Inefficiency of Non-market Allocations. \emph{Journal of Economic Perspectives,} 13(3), pp.217-226.
##'
##' @export

allpayGame <- function(sheet = NULL,
                       endowment = 5,
                       prize = 4,
                       ...) {
  # Read responses and initialize output objects.
  googlesheets4::gs4_deauth()
  googlesheets4::results <- read_sheet(sheet)
  colnames(results) <- make.names(colnames(results))
  results <-
    replace_na(results, list(First.Name = "John", Last.Name = "Doe"))
  results$First.Name <- stringr::str_to_title(results$First.Name6)
  results$Last.Name <- stringr::str_to_title(results$Last.Name)
  results <- results[order(results$Bid, decreasing = TRUE), ]
  results <- results[!duplicated(results[, c(2:3)]), ]
  winner <- results[which.max(results$Bid), 2:3]
  winner <- cbind(winner, prize)
  colnames(winner)[3] <- "Prize"
  colnames(results)[4] <- "Contributions"
  grades <- cbind(results, endowment)
  colnames(grades)[5] <- "Endowment"
  grades <-
    merge(grades,
          winner,
          by = c("Last.Name", "First.Name"),
          all = TRUE)
  grades <-
    replace_na(grades, list(Prize = 0))
  grades$Points <-
    grades$Endowment + grades$Prize - grades$Contributions
  revenue <- sum(grades$Contributions)
  out <- list(
    type = "allpayGame",
    results = results,
    winner = winner,
    revenue = revenue,
    grades = grades[order(grades$Last.Name, grades$First.Name),]
  )
  class(out) <- c('econGame', class(out))
  out
}
