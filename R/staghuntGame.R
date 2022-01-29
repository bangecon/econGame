##' Tabulate results for a simple in-class stag hunt game.
##'
##' @details \code{staghuntGame} tabulates the results of a simple stag hunt game in which students' points depend on their strategy and the strategy chosen by their randomly-assigned partners.
##'
##' @param sheet (required) is a character string sheet ID corresponding to the Google Sheets location containing the individual submissions.
##' @param payoff is a vector indicating the interdependent payoffs from the different pairs of {Student, Partner} strategies: \code{c({Compete, Compete}, {Collude, Compete}, {Compete, Collude}, {Collude, Collude})}
##' @param outfile is a character string giving a name to the new Google Sheet where the instructor wants to store the scores.
##'
##' @return \code{type} returns the type of activity (staghungGame).
##' @return \code{results} returns the original submissions (with equilibria and points per round added).
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @export

staghuntGame <-
  function(sheet,
           payoff = c(0.5, -0.5, 1, 2),
           ...) {
    # Set up the Google Sheets, read responses, and initialize output objects.
    if (length(payoff) != 4)
      stop("Payoff must have length == 4")
    results <- read_sheet(sheet)
    colnames(results) <- make.names(colnames(results))
    results <-
      replace_na(results, list(First.Name = "John", Last.Name = "Doe"))
    results$First.Name <- str_to_title(results$First.Name)
    results$Last.Name <- str_to_title(results$Last.Name)
    results$Partner.First.Name <-
      str_to_title(results$Partner.First.Name)
    results$Partner.Last.Name <-
      str_to_title(results$Partner.Last.Name)
    partnerResults <- results[, 4:7]
    colnames(partnerResults)[4] <- "Partner.Strategy"
    results <- merge(
      results,
      partnerResults,
      all = TRUE,
      by.x = c("First.Name", "Last.Name", "Round"),
      by.y = c("Partner.First.Name", "Partner.Last.Name", "Round")
    )
    results <- within(results, {
      Score <- ifelse(
        Partner.Strategy == "Rabbit",
        ifelse(Strategy == "Rabbit", payoff[1], payoff[2]),
        ifelse(Strategy == "Rabbit", payoff[3], payoff[4])
      )
    })
    results$Outcome <-
      paste0(results$Strategy, "-", results$Partner.Strategy)
    results$Outcome <-
      ifelse(results$Outcome == "Rabbit-Stag",
             "Stag-Rabbit",
             results$Outcome)
    payoffMatrix <- matrix(payoff, nrow = 2, ncol = 2)
    colnames(payoffMatrix) <-
      c("Partner = Rabbit", "Partner = Stag")
    rownames(payoffMatrix) <-
      c("Strategy = Rabbit", "Strategy = Stag")
    grades <-
      aggregate(Score ~ First.Name + Last.Name,
                data = results,
                FUN = sum)
    out <- list(
      type = "staghuntGame",
      payoff = payoffMatrix,
      results = results[order(results$Round,
                              results$Last.Name,
                              results$First.Name),-which(names(results) %in% 'Timestamp')],
      grades = grades[order(grades$Last.Name, grades$First.Name),]
    )
    class(out) <- c('econGame', class(out))
    out
  }
