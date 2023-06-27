##' Tabulate public good game results.
##'
##' Tabulates and assigns points for the results of a simple in-class public good game.
##'
##' @details \code{publicgoodGame} tabulates the results of a simple public good game based on Holt and Laury (1997). Each student starts with an endowment of 5 points that they can either keep or anonymously and voluntarily contribute to a public good. For each point a student keeps, they receive only that point. For each point contributed to the public good, \emph{everyone} receives R/N points, where N is the number of participants and R > 1 is the return the group earns on public good contributions. In other words, students have the opportunity to boost their points if everyone contributes all of them. But, if only a few participants contribute, their net return could diminish.
##'
##'
##' @param sheet (required) is a character string url corresponding to the Google Sheets location containing the individual submissions.
##' @param endowment is the number of gifted points students start the game with (default is 5).
##' @param return is the scaling factor for points contributed to the public good (default is 2).
##' @return \code{type} returns the type of activity (publicgoodGame).
##' @return \code{results} returns the original submissions.
##' @return \code{grades} returns the aggregated, stacked points "won" by each student for the entire activity.
##'
##' @references
##'
##' @export

publicgoodGame <- function(sheet, endowment = 0, return = 1.2, ...) {
  # Set up the Google Sheets, read responses, and initialize output objects.
  results <- read_sheet(sheet)
  colnames(results) <- make.names(colnames(results))
  results <-
    replace_na(results,
               list(First.Name = "John",
                    Last.Name = "Doe"))
  results <- results[!duplicated(cbind(results$First.Name, results$Last.Name)),]
  results$First.Name <-
    str_to_title(results$First.Name)
  results$Last.Name <-
    str_to_title(results$Last.Name)
  N <- nrow(results)
  totalContributions <- sum(results$Contribution)
  totalReallocations <- totalContributions * return
  individualReallocaitons <-
    ceiling(totalReallocations / nrow(results))
  results$Reallocation <- individualReallocaitons
  results$Score <- results$Reallocation - results$Contribution
  blindedResults <-
    data.frame(results[, -which(names(results) %in% c("First.Name", "Last.Name", "Timestamp"))])
  rownames(blindedResults) <-
    paste(results$Last.Name, results$First.Name, sep = ".")
  grades <-
    with(results,
         as.data.frame(
           cbind(Last.Name, First.Name, Contribution, Reallocation, Score)
         ))
  out <- list(
    type = "publicgoodGame",
    results = results[order(results$Last.Name, results$First.Name),],
    blindedResults = blindedResults[order(blindedResults$Contribution),],
    grades = grades[order(grades$Last.Name, grades$First.Name),]
  )
  class(out) <- c('econGame', class(out))
  out
}
