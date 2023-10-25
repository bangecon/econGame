##' Tabulate entry and exit game results.
##'
##' Tabulates and assigns points for the results of a simple in-class entry and exit game using random values.
##'
##' @details \code{equilibriumGame} tabulates the results of a simple entry and exit game based on a simplified version of Garratt (2000) with two crops (corn and soybeans). The instructor informs the students that they will choose to plant corn, soybeans, or nothing. Producing corn incurs a cost of four points, while producing soybeans incurs a cost of 10 points. Selling a unit of corn brings revenue equal to \eqn{P_c = (N/2) + 6 - Q_c}, where N equals the number of students participating and Q_c equals the number of students choosing to produce corn. Selling a unit of soybeans brings revenue equal to \eqn{P_s = (N/2) + 10 - Q_s}. These parameters allow for there to be a "normal profit" of one point per student in each market, and lessens the chances that students might win negative points. Students choosing to produce nothing sell their labor in the labor market and break even (less the "normal profit").
##'
##' @param sheet (required) is a character string corresponding to the Google Sheets location (url or ID key) containing the individual submissions.
##' @param names character list of the column names in `sheet`.
##' @param auth is a logical indicating whether to use an authentication token to access the Sheet containing the individual submissions.
##' @param email is an email address that matches the user account containing the Sheet with the individual submissions.
##' @return \code{type} returns the type of activity (entryGame).
##' @return \code{results} returns the original submissions (with market prices and points per round added).
##' @return \code{rounds} returns the number of rounds in "results"
##' @return \code{equilibria} returns a list containing the equilibria for each round.
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @references Garratt (2000). A Free Entry and Exit Experiment. \emph{Journal of Economic Education,} 31(3), pp.237-243.
##'
##' @export

entryGame <-
  function(sheet, names = NULL, auth = FALSE, ...) {
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
        market = "Market"
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
    rounds <- max(results$Round)
    equilibria <-
      list(
        N = list(NULL),
        Dinv_c = list(NULL),
        Q_c = list(NULL),
        P_c = list(NULL),
        Dinv_s = list(NULL),
        Q_s = list(NULL),
        P_s = list(NULL)
      )
    for (i in 1:rounds) {
      # Calculate the equilibrium for each round
      roundresult <- subset(results, Round == i)
      equilibria$N[[i]] <- nrow(roundresult)
      equilibria$Dinv_c[[i]] <-
        function(x) {
          (equilibria$N[[i]] / 2) +  6 - x
        }
      equilibria$Dinv_s[[i]] <-
        function(x) {
          (equilibria$N[[i]] / 2) + 10 - x
        }
      equilibria$Q_c[[i]] <-
        nrow(subset(roundresult, Market == "Corn"))
      equilibria$Q_s[[i]] <-
        nrow(subset(roundresult, Market == "Soybeans"))
      equilibria$P_c[[i]] <-
        equilibria$Dinv_c[[i]](equilibria$Q_c[[i]])
      equilibria$P_s[[i]] <-
        equilibria$Dinv_s[[i]](equilibria$Q_s[[i]])
    }
    # Calculate student points.
    results$Price <- NA
    results$Cost <- NA
    results$Points <- NA
    for (i in 1:nrow(results)) {
      results$Price[i] <- ifelse(
        results$Market[[i]] == "Soybeans",
        equilibria$P_s[[results$Round[[i]]]],
        ifelse(results$Market[[i]] == "Corn",
               equilibria$P_c[[results$Round[[i]]]], 0)
      )
      results$Cost[i] <- ifelse(results$Market[[i]] == "Soybeans",
                                10,
                                ifelse(results$Market[[i]] == "Corn", 4, 0))
      results$Points[i] <- results$Price[i] - results$Cost[i]
    }
    grades <-
      aggregate(
        Points ~ Last.Name + First.Name,
        data = results,
        FUN = sum,
        na.action = na.pass
      )
    out <- list(
      type = "entryGame",
      equilibria = equilibria,
      results = results[order(results$Round,
                              results$Market,
                              results$Last.Name,
                              results$First.Name),],
      rounds = rounds,
      grades = grades[order(grades$Last.Name, grades$First.Name),]
    )
    class(out) <- c('econGame', class(out))
    out
  }
