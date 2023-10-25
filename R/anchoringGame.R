3##' Tabulate anchoring game results.
##'
##' Tabulates and assigns points for the results of a simple in-class anchoring game.
##'
##' @details \code{anchoringGame} tabulates the results of a simple anchoring game based on the classroom adaptation of Tversky and Kahneman (1974) presented in Gelman and Glickman (2000). Students form receive a random signal (10 or 65 with probabilities {0.5, 0.5}) from a Google Sheet. They enter their name and their signal in a Google Form. Then they are asked to make two guesses about an unknown quantity: in the original example the unknown quantity is the percentage of countries in the world that are located on the continent of Africa. The first question is whether they believe this quantity is higher or lower than their signal. The second question asks them to guess the quantity. This function tabulates the guesses by group in a \code{gtsummay} table with the corresponding p-value for the difference-in-means test. Plotting the output produces a box plot of the responses by group.
##'
##' @param sheet (required) is a character string url corresponding to the Google Sheets location containing the individual submissions.
##' @param names character list of the column names in `sheet`.
##' @param auth is a logical indicating whether to use an authentication token to access the Sheet containing the individual submissions.
##' @param email is an email address that matches the user account containing the Sheet with the individual submissions (if `auth == TRUE`).
##'
##' @return \code{type} returns the type of activity (anchoringGame).
##' @return \code{results} returns the original submissions.
##' @return \code{summaryTable} returns the summary table (with difference in means test statistics).
##'
##' @references Gelman, A. and Nolan, D., 2002. Some Class-Participation Demonstrations for Introductory Probability and Statistics \emph{Journal of Educational and Behavioral Statistics.} 25(1): 84-100.
##' Tversky, A. and Kahneman, D., 1974. Judgement under Uncertainty: Heuristics and Biases \emph{Science, New Series.} 1985(4175): 1124-1131.
##'
##' @export

anchoringGame <- function(sheet,
                          names = NULL,
                          auth = FALSE,
                          email = NULL,
                          ...) {
  # Set up the Google Sheets, read responses, and initialize output objects.
  googlesheets4::gs4_deauth()
  results <- googlesheets4::read_sheet(sheet)
  colnames(results) <- make.names(colnames(results))
  results <-
    replace_na(results, list(First.Name = "John", Last.Name = "Doe"))
  results$First.Name <- stringr::str_to_title(results$First.Name)
  results$Last.Name <- stringr::str_to_title(results$Last.Name)
  results$Value <- as.factor(results$Value)
  results$Higher <- ifelse(results$Higher.Lower == "Higher", 1, 0)
  results$Percent <- as.numeric(results$Percent)
  summaryTable <- gtsummary::tbl_summary(
    results,
    by = Value,
    digits = list(all_continuous() ~ c(2, 2)),
    include = c(Higher, Percent),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_dichotomous() ~ "{n} ({p}%)"
    )
  )
  summaryTable <- suppressMessages(add_difference(summaryTable))
  out <- list(type = "anchoringGame",
              results = results[order(results$Value, results$Last.Name, results$First.Name), ],
              summaryTable = summaryTable)
  class(out) <- c('econGame', class(out))
  return(out)
}
