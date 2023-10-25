##' Tabulate results for an all-pay auction game.
##'
##' Tabulates and assigns points for the results of a simple in-class entry and exit game using random values.
##'
##' @details \code{lobbyGame} tabulates the results of a simple lobbying game based on a simplified version of Goeree and Holt (1999). By default students receive an "endowment" of "extra credit" points (default is 5) at the beginning of a round of bidding. In the first iteration, students "bid" on a monopoly license "prize" (default is 4 points) by making an anonymous number of "contributions" to politicians in order to influence the decision by the government. Each contribution costs 1 point, and is tabulated as the number of times a student submits the Google Form. The winner of the auction is determined by a lottery from a random row sampled from the responses. In this iteration, students submit bids orally or electronically in real time according to an all-pay auction.
##'
##' @param sheet  (required) is a character string sheet ID corresponding to the Google Sheet containing the individual submissions.
##' @param endowment is the size of the initial endowment of points the instructor wishes to give each student.
##' @param prize is the value of the license or prize each interest group (student) is bidding on.
##' @param seed is the value for the numeric seed in the randomization for selecting the "winner" (default is 8675309).
##' @param names character list of the column names in `sheet`.
##' @param auth is a logical indicating whether to use an authentication token to access the Sheet containing the individual submissions.
##' @param email is an email address that matches the user account containing the Sheet with the individual submissions (if `auth == TRUE`).
##'
##' @return \code{type} returns the type of activity (allpayGame).
##' @return \code{results} returns the original submissions (with market prices and points per round added).
##' @return \code{winner} returns the name of the winner and the size of the prize.
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @references Goeree, Jacob K. & Charles A. Holt (1999). Classroom Games: Rent-Seeking and the Inefficiency of Non-market Allocations. \emph{Journal of Economic Perspectives,} 13(3), pp.217-226.
##'
##' @export

lobbyGame <-
  function(sheet,
           endowment = 5,
           prize = 4,
           seed = 8675309,
           auth = FALSE,
           names = NULL,
           email = FALSE,
           ...)
  {
    # Set up the Google Sheets, read responses, and initialize output objects.
    if (auth == TRUE) {
      options(gargle_oauth_cache = ".secrets")
      googlesheets4::gs4_auth()
      googlesheets4::gs4_deauth()
      googlesheets4::gs4_auth(cache = ".secrets", email = email)
    }
    else {
      googlesheets4::gs4_deauth()
    }
    if (is.null(names)) {
      names <- list(
        first = "First.Name",
        last = "Last.Name",
        round = "Round",
        value = "Value",
        bid = "Bid",
        ask = "Ask"
      )
    } else {
      names <- lapply(names, make.names)
    }
    results <- read_sheet(sheet)
    if (!is.null(seed))
      set.seed(seed)
    colnames(results) <- make.names(colnames(results))
    results <-
      replace_na(results, list(First.Name = "John", Last.Name = "Doe"))
    results$First.Name <- str_to_title(results$First.Name)
    results$Last.Name <- str_to_title(results$Last.Name)
    winner <- results[sample(nrow(results), 1), 2:3]
    results <-
      results[order(results$Last.Name, results$First.Name),-1]
    results$Contributions <- 1
    winner <- cbind(winner, prize)
    colnames(winner)[3] <- "Prize"
    grades <-
      aggregate(
        Contributions ~ Last.Name + First.Name,
        data = results,
        FUN = sum,
        na.action = na.pass
      )
    grades <- cbind(grades, endowment)
    colnames(grades)[4] <- "Endowment"
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
      type = "lobbyGame",
      results = results[order(results$Last.Name,
                              results$First.Name), ],
      winner = winner,
      revenue = revenue,
      grades = grades[order(grades$Last.Name, grades$First.Name), ]
    )
    class(out) <- c('econGame', class(out))
    out
  }
