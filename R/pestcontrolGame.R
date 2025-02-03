##' Tabulate results for a simple in-class prisoner's dilemma game.
##'
##' @details \code{pestcontrolGame} tabulates the results of a simple stag hunt game in which students' points depend on their strategy and the strategy chosen by their randomly-assigned partners.
##'
##' @param sheet (required) is a character string sheet ID corresponding to the Google Sheets location containing the individual submissions.
##' @param partners is a character string indicating whether students were randomly assigned partners or chose their own partners. Default is 'random'.
##' @param roleLabs is a character vector specifying the labels for the two roles in the game. Default is c("Anil", "Bala").
##' @param payoff is a vector indicating the interdependent payoffs from the different pairs of {Student, Partner} strategies: \code{c({Compete, Compete}, {Collude, Compete}, {Compete, Collude}, {Collude, Collude})}
##' @param seed is an integer that sets the seed for the random number generator. Default is 8675309.
##' @param auth is a logical indicating whether to authenticate with Google Sheets. Default is FALSE.
##' @param names is a list of character strings that specify the column names for the first name, last name, partner first name, partner last name, round, and strategy. Default is NULL.
##'
##' @return \code{type} returns the type of activity (pestcontrolGame).
##' @return \code{payoff} returns the payoff matrix for the game.
##' @return \code{results} returns the original submissions (with equilibria and points per round added).
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @export

pestcontrolGame <-
  function(sheet,
           partners = 'random',
           roleLabs = c("Anil", "Bala"),
           payoff = c(3, 4, 1, 2),
           seed = 8675309,
           auth = FALSE,
           names = NULL,
           ...) {
    # Set up the Google Sheets, read responses, and initialize output objects.
    if (length(payoff) != 4)
      stop("Payoff must have length == 4")
    if(auth == TRUE) {
      options(gargle_oauth_cache = ".secrets")
      googlesheets4::gs4_auth()
      googlesheets4::gs4_deauth()
      googlesheets4::gs4_auth(cache = ".secrets", email = email)
    }     else {
      googlesheets4::gs4_deauth()
    }
    if (is.null(names)) {
      names <- list()
      if (partners == 'students') {
        names <- list(
          first = "First.Name",
          last = "Last.Name",
          partnerFirst = "Partner.First.Name",
          partnerLast = "Partner.Last.Name",
          round = "Round",
          strategy = "Strategy"
        )
      } else {
        names <- list(
          first = "First.Name",
          last = "Last.Name",
          round = "Round",
          strategy = "Strategy"
        )
      }
    } else {
      names <- lapply(names, make.names)
    }
    if(is.null(sheet)) {
      results <- data.frame(
        First.Name.1 = c("Ahmed", "Al", "Amanda", "Anita", "Bea", "Heywood U.", "IP", "Ollie"),
        Last.Name.1 = c("Adoudi", "Coholic", "Huggenkiss", "Bath", "O'Problem", "Kuddelmee", "Freely", "Tabooger"),
        Strategy.1 = c("Terminator", "Terminator", "IPC", "IPC", "IPC", "Terminator", "IPC", "Terminator"),
        First.Name.2 = c("Ollie", "IP", "Heywood U.", "Bea", "Anita", "Amanda", "Al", "Ahmed"),
        Last.Name.2 = c("Tabooger", "Freely", "Kuddelmee", "O'Problem", "Bath", "Huggenkiss", "Coholic", "Adouddi"),
        Strategy.2 = c("Terminator", "IPC", "Terminator", "IPC", "IPC", "IPC", "Terminator", "Terminator"),
        Round = rep(1, 8)
      )
    } else  {
      if (partners == 'random') {
        results <- randomGroups(sheet)
        results <- results[,-which(names(results) %in% "Timestamp")]
        results[[c(names$first)]] <- tidyr::replace_na(results[[c(names$first)]], "John")
        results[[c(names$last)]] <- tidyr::replace_na(results[[c(names$last)]], "Doe")
        results[[c(names$first)]] <- stringr::str_to_title(results[[c(names$first)]])
        results[[c(names$last)]] <- stringr::str_to_title(results[[c(names$last)]])
        results <- as.data.frame(results)
        colnames(results)[which(colnames(results) == names$first)] <-
          'First.Name'
        colnames(results)[which(colnames(results) == names$last)] <-
          'Last.Name'
        colnames(results)[which(colnames(results) == names$strategy)] <-
          'Strategy'
        results <-
          reshape(
            results,
            direction = 'wide',
            idvar = c('Round', 'group'),
            timevar = 'member'
          )
        partnerResults <- results
        colnames(partnerResults)[which(
          colnames(partnerResults) %in% c(
            'First.Name.1',
            'Last.Name.1',
            'Strategy.1',
            'First.Name.2',
            'Last.Name.2',
            'Strategy.2'
          )
        )] <-
          c(
            'First.Name.2',
            'Last.Name.2',
            'Strategy.2',
            'First.Name.1',
            'Last.Name.1',
            'Strategy.1'
          )
        results <- rbind(results, partnerResults)
      } else {
        results <- as.data.frame(googlesheets4::read_sheet(sheet))
        results <- results[,-which(names(results) %in% "Timestamp")]
        colnames(results) <- make.names(colnames(results))
        results[[c(names$first)]] <- tidyr::replace_na(results[[c(names$first)]], "John")
        results[[c(names$last)]] <- tidyr::replace_na(results[[c(names$last)]], "Doe")
        results[, c(names$first)] <- ifelse(!is.na(results[, c(names$first)]), results[, c(names$first)], "John")
        results[, c(names$last)] <- ifelse(!is.na(results[, c(names$last)]), results[, c(names$last)], "Doe")
        colnames(results)[which(colnames(results) == names$first)] <-
          'First.Name.1'
        colnames(results)[which(colnames(results) == names$last)] <-
          'Last.Name.1'
        colnames(results)[which(colnames(results) == names$partnerFirst)] <-
          'First.Name.2'
        colnames(results)[which(colnames(results) == names$partnerLast)] <-
          'Last.Name.2'
        colnames(results)[which(colnames(results) == names$strategy)] <-
          'Strategy.1'
        partnerResults <-
          results[, which(names(results) %in% c('First.Name.2', 'Last.Name.2', 'Round', 'Strategy.1'))]
        colnames(partnerResults)[which(colnames(partnerResults) == "Strategy.1")] <-
          "Strategy.2"
        results <- merge(
          results,
          partnerResults,
          all = TRUE,
          by.x = c("First.Name.1", "Last.Name.1", "Round"),
          by.y = c("First.Name.2", "Last.Name.2", "Round")
        )
      }
    }
    results$Score.1 <- ifelse(
      results$Strategy.1 == "IPC",
        ifelse(results$Strategy.2 == "IPC", payoff[1], payoff[3]),
        ifelse(results$Strategy.2 == "Terminator", payoff[4], payoff[2])
      )
    results$Score.2 <- ifelse(
      results$Strategy.1 == "IPC",
        ifelse(results$Strategy.2 == "IPC", payoff[1], payoff[2]),
        ifelse(results$Strategy.2 == "Terminator", payoff[4], payoff[3])
      )
    results <- data.frame(
      First.Name = c(results$First.Name.1, results$First.Name.2),
      Last.Name = c(results$Last.Name.1, results$Last.Name.2),
      Partner.First.Name = c(results$First.Name.2, results$First.Name.1),
      Partner.Last.Name = c(results$Last.Name.2, results$Last.Name.1),
      Round = c(results$Round, results$Round),
      Role = c(results$Role.1, results$Role.2),
      Strategy = c(results$Strategy.1, results$Strategy.2),
      Partner.Strategy = c(results$Strategy.2, results$Strategy.1),
      Score = c(results$Score.1, results$Score.2)
    )
    results$Outcome <-
      paste0(results$Strategy.1, "-", results$Strategy.2)
    payoffMatrix <- matrix(
      c(paste0("(",payoff[1], ",", payoff[1], ")"),
        paste0("(",payoff[2], ",", payoff[3], ")"),
        paste0("(",payoff[3], ",", payoff[2], ")"),
        paste0("(",payoff[4], ",", payoff[4], ")")),
      nrow = 2, ncol = 2)
    colnames(payoffMatrix) <-
      c("Bala = IPC", "Bala = Terminator")
    rownames(payoffMatrix) <-
      c("Anil = IPC", "Anil = Terminator")
    grades <-
      aggregate(Score ~ First.Name + Last.Name,
                data = results,
                FUN = sum)
    grades  <- grades[order(grades$Last.Name, grades$First.Name),]
    colnames(grades) <- c("First Name", "Last Name", "Score")
    out <- list(
      type = "pestcontrolGame",
      payoff = payoffMatrix,
      results = results[order(results$Round,
                              results$Last.Name,
                              results$First.Name), ],
      grades = grades
    )
    class(out) <- c('econGame', class(out))
    out
  }
