##' Tabulate results for a simple in-class stag hunt game.
##'
##' @details \code{staghuntGame} tabulates the results of a simple stag hunt game in which students' points depend on their strategy and the strategy chosen by their randomly-assigned partners.
##'
##' @param sheet (required) is a character string sheet ID corresponding to the Google Sheets location containing the individual submissions.
##' @param payoff is a vector indicating the interdependent payoffs from the different pairs of {Student, Partner} strategies: \code{c({Compete, Compete}, {Collude, Compete}, {Compete, Collude}, {Collude, Collude})}
##' @param outfile is a character string giving a name to the new Google Sheet where the instructor wants to store the scores.
##'
##' @return \code{type} returns the type of activity (codeGame).
##' @return \code{results} returns the original submissions (with equilibria and points per round added).
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @export

codeGame <-
  function(resultsSheet,
           partners = 'random',
           roleSheet = NULL,
           roleLabs = c("Astrid", "Bettina"),
           payoff.A = c(4, 0, 2, 3),
           payoff.B = c(3, 0, 2, 6),
           seed = 8675309,
           auth = FALSE,
           names = NULL,
           ...) {
    # Set up the Google Sheets, read responses, and initialize output objects.
    if (length(payoff.A) != 4 | length(payoff.B) != 4)
      stop("Payoffs must have length == 4")
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
          role = "Role",
          partnerFirst = "Partner.First.Name",
          partnerLast = "Partner.Last.Name",
          round = "Round",
          strategy = "Strategy"
        )
      } else {
        names <- list(
          first = "First.Name",
          last = "Last.Name",
          role = "Role",
          round = "Round",
          strategy = "Strategy"
        )
      }
    } else {
      names <- lapply(names, make.names)
    }
    if(is.null(resultsSheet)) {
      resultsWide <- data.frame(
        First.Name.1 = c("Ahmed", "Al", "Amanda", "Anita", "Bea", "Heywood U.", "Hugh", "IP"),
        Last.Name.1 = c("Adoudi", "Coholic", "Huggenkiss", "Bath", "O'Problem", "Kuddelmee", "Jass", "Freely"),
        Role.1 = c(rep("Astrid", 8)),
        Strategy.1 = c(rep("C++", 8)),
        First.Name.2 = c("Ivana", "Jacques", "Jim", "Mary", "Maya", "Oliver", "Ollie", "Seymour"),
        Last.Name.2 = c("Tinkle", "Strap", "Bang", "Jane", "Normusbut", "Klozoff", "Tabooger", "Butz"),
        Role.2 = c(rep("Bettina", 8)),
        Strategy.2 = c(rep("C++", 8)),
        Round = rep(1, 8)
      )
      AstridResults <- resultsWide[, c(
        "Round", "First.Name.1", "Last.Name.1", "Role.1", "Strategy.1")]
      BettinaResults <- resultsWide[, c(
        "Round", "First.Name.2", "Last.Name.2", "Role.2", "Strategy.2")]
      results <- data.frame(
        Round = c(AstridResults$Round, BettinaResults$Round),
        First.Name.1 = c(AstridResults$First.Name.1, BettinaResults$First.Name.2),
        Last.Name.1 = c(AstridResults$Last.Name.1, BettinaResults$Last.Name.2),
        Role.1 = c(AstridResults$Role.1, BettinaResults$Role.2),
        First.Name.2 = c(BettinaResults$First.Name.2, AstridResults$First.Name.1),
        Last.Name.2 = c(BettinaResults$Last.Name.2, AstridResults$Last.Name.1),
        Strategy.1 = c(AstridResults$Strategy.1, BettinaResults$Strategy.2)
      )
      results <- results[order(
        results$Round, results$Role.1, results$Last.Name.1),]
    } else  {
      if(partners == "random") {
        roles <- randomRoles(roleSheet, roleLabs = roleLabs, seed = 8675309)
        rolesLong <- roles$long
        roles <- roles$wide
        results <- googlesheets4::read_sheet(resultsSheet)
        colnames(results) <- make.names(colnames(results))
        results <- results[,-which(names(results) %in% "Timestamp")]
        results[[c(names$first)]] <- tidyr::replace_na(results[[c(names$first)]], "John")
        results[[c(names$last)]] <- tidyr::replace_na(results[[c(names$last)]], "Doe")
        results[[c(names$first)]] <- stringr::str_to_title(results[[c(names$first)]])
        results[[c(names$last)]] <- stringr::str_to_title(results[[c(names$last)]])
        results <- as.data.frame(results)
        colnames(results)[which(colnames(results) == names$first)] <- 'First.Name'
        colnames(results)[which(colnames(results) == names$last)] <- 'Last.Name'
        colnames(results)[which(colnames(results) == names$round)] <- 'Round'
        colnames(results)[which(colnames(results) == names$role)] <- 'Role'
        colnames(results)[which(colnames(results) == names$strategy)] <- 'Strategy'
        results <- results[order(results$Round, results$First.Name, results$Last.Name), ]
        AstridResults <- subset(results, Role == "Astrid")[, c(
          "Round", "First.Name", "Last.Name", "Role", "Strategy")]
        colnames(AstridResults) <- c(
          "Round", "First.Name.1", "Last.Name.1", "Role.1", "Strategy.1")
        BettinaResults <- subset(results, Role == "Bettina")[, c(
          "Round", "First.Name", "Last.Name", "Role", "Strategy")]
        colnames(BettinaResults) <- c(
          "Round", "First.Name.2", "Last.Name.2", "Role.2", "Strategy.2")
        resultsWide <- merge(roles, results,
                             by.x = c("Round", "First.Name.1", "Last.Name.1", "Role.1"),
                             by.y = c("Round", "First.Name", "Last.Name", "Role"))
        colnames(resultsWide)[which(colnames(resultsWide) == "Strategy")] <-
          "Strategy.1"
        resultsWide <- merge(resultsWide, results,
                             by.x = c("Round", "First.Name.2", "Last.Name.2", "Role.2"),
                             by.y = c("Round", "First.Name", "Last.Name", "Role"))
        colnames(resultsWide)[which(colnames(resultsWide) == "Strategy")] <-
          "Strategy.2"
        results <- data.frame(
          Round = c(AstridResults$Round, BettinaResults$Round),
          First.Name.1 = c(AstridResults$First.Name.1, BettinaResults$First.Name.2),
          Last.Name.1 = c(AstridResults$Last.Name.1, BettinaResults$Last.Name.2),
          Role.1 = c(AstridResults$Role.1, BettinaResults$Role.2),
          First.Name.2 = c(BettinaResults$First.Name.2, AstridResults$First.Name.1),
          Last.Name.2 = c(BettinaResults$Last.Name.2, AstridResults$Last.Name.1),
          Strategy.1 = c(AstridResults$Strategy.1, BettinaResults$Strategy.2)
        )
        results <- results[order(
          results$Round, results$Role.1, results$Last.Name.1),]
        } else {
          googlesheets4::gs4_deauth()
          results <- as.data.frame(googlesheets4::read_sheet(resultsSheet))
          results <- results[,-which(names(results) %in% "Timestamp")]
          colnames(results) <- make.names(colnames(results))
          results[[c(names$first)]] <- tidyr::replace_na(results[[c(names$first)]], "John")
          results[[c(names$last)]] <- tidyr::replace_na(results[[c(names$last)]], "Doe")
          results[, c(names$first)] <- ifelse(!is.na(results[, c(names$first)]), results[, c(names$first)], "John")
          results[, c(names$last)] <- ifelse(!is.na(results[, c(names$last)]), results[, c(names$last)], "Doe")
          AstridResults <- subset(results, Role == "Astrid")
          BettinaResults <- subset(results[, which(
            names(results) %in% c(
              names$first, names$last, names$round, names$role, names$strategy))],
            Role == "Bettina")
          colnames(results)[which(colnames(results) == names$first)] <- 'First.Name.1'
          colnames(results)[which(colnames(results) == names$last)] <- 'Last.Name.1'
          colnames(results)[which(colnames(results) == names$partnerFirst)] <- 'First.Name.2'
          colnames(results)[which(colnames(results) == names$partnerLast)] <- 'Last.Name.2'
          colnames(results)[which(colnames(results) == names$role)] <- 'Role.1'
          colnames(results)[which(colnames(results) == names$strategy)] <- 'Strategy.1'
          colnames(AstridResults)[which(colnames(AstridResults) == names$first)] <- 'First.Name.1'
          colnames(AstridResults)[which(colnames(AstridResults) == names$last)] <- 'Last.Name.1'
          colnames(AstridResults)[which(colnames(AstridResults) == names$partnerFirst)] <- 'First.Name.2'
          colnames(AstridResults)[which(colnames(AstridResults) == names$partnerLast)] <- 'Last.Name.2'
          colnames(AstridResults)[which(colnames(AstridResults) == names$role)] <- 'Role.1'
          colnames(AstridResults)[which(colnames(AstridResults) == names$strategy)] <- 'Strategy.1'
          colnames(BettinaResults)[which(colnames(BettinaResults) == names$first)] <- 'First.Name.2'
          colnames(BettinaResults)[which(colnames(BettinaResults) == names$last)] <- 'Last.Name.2'
          colnames(BettinaResults)[which(colnames(BettinaResults) == names$role)] <- 'Role.2'
          colnames(BettinaResults)[which(colnames(BettinaResults) == names$strategy)] <- 'Strategy.2'
          resultsWide <- merge(AstridResults, BettinaResults, all = TRUE,
                               by = c("First.Name.2", "Last.Name.2", "Round"))
          AstridResults <- resultsWide[, c(
            "Round", "First.Name.1", "Last.Name.1", "Role.1", "Strategy.1")]
          BettinaResults <- resultsWide[, c(
            "Round", "First.Name.2", "Last.Name.2", "Role.2", "Strategy.2")]
          roles <- resultsWide[, c(
            "Round", "First.Name.1", "Last.Name.1", "Role.1", "First.Name.2", "Last.Name.2", "Role.2")]
          roles <- roles[order(roles$Round, roles$Last.Name.1, roles$First.Name.1),]
        }
    }

    resultsWide$Score.1 <- ifelse(
      resultsWide$Strategy.1 == "Java",
        ifelse(resultsWide$Strategy.2 == "Java", payoff.A[1], payoff.A[3]),
        ifelse(resultsWide$Strategy.2 == "C++", payoff.A[4], payoff.A[2])
      )
    resultsWide$Score.2 <- ifelse(
      resultsWide$Strategy.1 == "Java",
        ifelse(resultsWide$Strategy.2 == "Java", payoff.B[1], payoff.B[3]),
        ifelse(resultsWide$Strategy.2 == "C++", payoff.B[4], payoff.B[2])
      )
    resultsWide$Outcome <-
      paste0(resultsWide$Strategy.1, "-", resultsWide$Strategy.2)
    results <- data.frame(
      First.Name = c(resultsWide$First.Name.1, resultsWide$First.Name.2),
      Last.Name = c(resultsWide$Last.Name.1, resultsWide$Last.Name.2),
      Partner.First.Name = c(resultsWide$First.Name.2, resultsWide$First.Name.1),
      Partner.Last.Name = c(resultsWide$Last.Name.2, resultsWide$Last.Name.1),
      Round = c(resultsWide$Round, resultsWide$Round),
      Role = c(resultsWide$Role.1, resultsWide$Role.2),
      Strategy = c(resultsWide$Strategy.1, resultsWide$Strategy.2),
      Partner.Strategy = c(resultsWide$Strategy.2, resultsWide$Strategy.1),
      Score = c(resultsWide$Score.1, resultsWide$Score.2)
    )
    payoffMatrix <- matrix(
      c(paste0("(",payoff.A[1], ",", payoff.B[1], ")"),
        paste0("(",payoff.A[2], ",", payoff.B[2], ")"),
        paste0("(",payoff.A[3], ",", payoff.B[3], ")"),
        paste0("(",payoff.A[4], ",", payoff.B[4], ")")),
      nrow = 2, ncol = 2)
    colnames(payoffMatrix) <-
      c("Bettina = Java", "Bettina = C++")
    rownames(payoffMatrix) <-
      c("Astrid = Java", "Astrid = C++")
    grades <-
      aggregate(Score ~ First.Name + Last.Name,
                data = results,
                FUN = sum)
    grades  <- grades[order(grades$Last.Name, grades$First.Name),]
    colnames(grades) <- c("First Name", "Last Name", "Score")
    out <- list(
      type = "codeGame",
      roles = roles,
      AstridResults = AstridResults,
      BettinaResults = BettinaResults,
      payoff = payoffMatrix,
      results = results[order(results$Round,
                              results$Last.Name,
                              results$First.Name), ],
      resultsWide = resultsWide,
      grades = grades
    )
    class(out) <- c('econGame', class(out))
    out
  }
