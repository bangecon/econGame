##' Tabulate results of a prisonor's dilemma game.
##'
##' Tabulates and assigns points for the results of a simple in-class prisoner's dilemma game.
##'
##' @details \code{multipdGame} tabulates the results of a simple prisoner's dilemma game in which students' points depend on their strategy and the strategy chosen by the majority of the rest of the class.
##'
##' @param sheet (required) is a character string ID corresponding to the Google Sheet containing the individual submissions.
##' @param payoff (required) is an atomic vector indicating the interdependent payoffs from the different strategy pairs {Respondent, Class}: `c({Compete, Compete}, {Collude, Compete}, {Compete, Collude}, {Collude, Collude})`. Default is `c(0, -0.5, 1, 0.5)`.
##'
##' @return \code{type} returns the type of activity (multipdGame).
##' @return \code{equliibrium} returns the equilibrium strategy chosen by the majority of the class.
##' @return \code{payoff} returns the payoff matrix.
##' @return \code{results} returns the original submissions (with equilibria and points per round added).
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @export

multipdGame <-
  function(sheet,
           payoff = c(0,-0.5, 1, 0.5),
           auth = FALSE,
           names = NULL,
           ...) {
    # Set up the Google Sheets, read responses, and initialize output objects.
    if (length(payoff) != 4)
      stop("Payoff must have length == 4")
    # Set up the Google Sheets, read responses, and initialize output objects.
    if(auth == TRUE) {
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
        strategy = "Stragegy"
      )
    } else {
      names <- lapply(names, make.names)
    }
    results <- read_sheet(sheet)
    colnames(results) <- make.names(colnames(results))
    results <-
      replace_na(results, list(First.Name = "John", Last.Name = "Doe"))
    results$First.Name <- str_to_title(results$First.Name)
    results$Last.Name <- str_to_title(results$Last.Name)
    payoffMatrix <- matrix(payoff, nrow = 2, ncol = 2)
    colnames(payoffMatrix) <-
      c("Majority = Compete", "Majority = Collude")
    rownames(payoffMatrix) <-
      c("Strategy = Compete", "Strategy = Collude")
    mode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    equilibria <- results %>%
      group_by(Round) %>%
      summarize(Equilibrium = str_to_title(mode(Strategy)))
    results <- merge(results, equilibria)
    results <- within(results, {
      Score <- ifelse(
        Equilibrium == "Compete",
        ifelse(Strategy == "Compete", payoff[1], payoff[2]),
        ifelse(Strategy == "Compete", payoff[3], payoff[4])
      )
    })
    grades <-
      aggregate(Score ~ First.Name + Last.Name,
                data = results,
                FUN = sum)
    out <- list(
      type = "multipdGame",
      payoff = payoffMatrix,
      equilibria = equilibria,
      results = results[order(results$Round,
                              results$Last.Name,
                              results$First.Name),],
      grades = grades[order(grades$Last.Name, grades$First.Name),]
    )
    class(out) <- c('econGame', class(out))
    out
  }
