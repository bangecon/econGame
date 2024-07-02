##' Tabulate results for a simple in-class Stackelberg duopoly game.
##'
##' @details \code{cournotGame} tabulates the results of a simple Stackelberg duopoly game in which students' points depend on their strategy and the strategy chosen by their randomly-assigned partners.
##'
##' @param resultsSheet (required) is a character string sheet ID corresponding to the Google Sheets location containing the individual submissions.
##' @param partners is a character string equal to \code{'students'} or \code{'random'} indicating whether students choose their own partners (default) or whether the function should generate them randomly after the students have decided their strategies. If \code{partners = 'students'}, then the form must include fields for the student's own first and last names and fields for their partners' first and last names. If partners are random, then the columns in `sheet` should either contain columns named 'First Name' and 'Last Name', or the user needs to specify the columns in `sheet` corresponding to this information to pass to `randomGroups()`.
##' @param roleSheet is a character string sheet ID corresponding to the Google Sheets location containing the list of leaders (if `partners == 'random'`).
##' @param a is the value of the intercept of the linear inverse-demand function (default is 10).
##' @param b is the value of the slope of the linear inverse-demand function (default is -1).
##' @param c is the value of the firm's marginal cost (default is 6).
##' @param f is the value of the firm's fixed cost (default is 0).
##' @param names character list of the column names in `sheet`.
##' @param auth is a logical indicating whether to use an authentication token to access the Sheet containing the individual submissions.
##' @param email is an email address that matches the user account containing the Sheet with the individual submissions (if `auth == TRUE`).
##'
##' @return \code{type} returns the type of activity (`bertrandGame`).
##' @return \code{payoff} returns the payoff matrix.
##' @return \code{output} returns the matrix of quantities corresponding to the strategies.
##' @return \code{price} returns the matrix of prices corresponding to the total merket output for each combination of strategies.
##' @return \code{results} returns the original submissions (with equilibria and points per round added).
##' @return \code{grades} returns the aggregated points 'won' by each student for the entire activity.
##'
##' @export

stackelbergGame <-
  function(resultsSheet = NULL,
           partners = 'random',
           roleSheet = NULL,
           a = 10,
           b = -1,
           c = 6,
           f = 0,
           names = NULL,
           auth = FALSE,
           email = NULL,
           ...) {
    # Set up the Google Sheets, read responses, and initialize output objects.
    if (a <= 0)
      stop("The intercept of the demand function needs to be positive.")
    if (b >= 0)
      stop("Demand curves are downward-sloping!")
    if (c <= 0)
      stop("There ain't no such thing as a free lunch (TANSTAAFL)!")
    if (f <  0)
      stop("Fixed costs must be non-negative.")
    if(auth == TRUE) {
      options(gargle_oauth_cache = ".secrets")
      googlesheets4::gs4_auth()
      googlesheets4::gs4_deauth()
      googlesheets4::gs4_auth(cache = ".secrets", email = email)
    } else {
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
          role = "Role",
          strategy = "Strategy"
        )
      } else {
        names <- list(
          first = "First.Name",
          last = "Last.Name",
          round = "Round",
          role = "Role",
          strategy = "Strategy"
        )
      }
    } else {
      names <- lapply(names, make.names)
    }
    if(partners == "random") {
      roles <- randomRoles(roleSheet)
      rolesLong <- roles$long
      roles <- roles$wide
      if(is.null(resultsSheet)) {
        resultsWide <- roles
        resultsWide$Strategy.1 <- "Defect"
        resultsWide$Strategy.2 <- "Defect"
        leaderResults <- resultsWide[, c(
          "Round", "First.Name.1", "Last.Name.1", "Role.1", "Strategy.1")]
        followResults <- resultsWide[, c(
          "Round", "First.Name.2", "Last.Name.2", "Role.2", "Strategy.2")]
        results <- data.frame(
          Round = c(leaderResults$Round, followResults$Round),
          First.Name.1 = c(leaderResults$First.Name.1, followResults$First.Name.2),
          Last.Name.1 = c(leaderResults$Last.Name.1, followResults$Last.Name.2),
          Role.1 = c(leaderResults$Role.1, followResults$Role.2),
          First.Name.2 = c(followResults$First.Name.2, leaderResults$First.Name.1),
          Last.Name.2 = c(followResults$Last.Name.2, leaderResults$Last.Name.1),
          Strategy.1 = c(leaderResults$Strategy.1, followResults$Strategy.2)
        )
        results <- results[order(
          results$Round, results$Role.1, results$Last.Name.1),]
      } else {
        results <- googlesheets4::read_sheet(resultsSheet)
        colnames(results) <- make.names(colnames(results))
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
        colnames(results)[which(colnames(results) == names$round)] <-
          'Round'
        colnames(results)[which(colnames(results) == names$strategy)] <-
          'Strategy'
        results <- merge(rolesLong, results, by = c("First.Name", "Last.Name", "Round"))
        results <- results[order(results$Round, results$First.Name, results$Last.Name), ]
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
        leaderResults <- resultsWide[, c(
          "Round", "First.Name.1", "Last.Name.1", "Role.1", "Strategy.1")]
        followResults <- resultsWide[, c(
          "Round", "First.Name.2", "Last.Name.2", "Role.2", "Strategy.2")]
        results <- data.frame(
          Round = c(leaderResults$Round, followResults$Round),
          First.Name.1 = c(leaderResults$First.Name.1, followResults$First.Name.2),
          Last.Name.1 = c(leaderResults$Last.Name.1, followResults$Last.Name.2),
          Role.1 = c(leaderResults$Role.1, followResults$Role.2),
          First.Name.2 = c(followResults$First.Name.2, leaderResults$First.Name.1),
          Last.Name.2 = c(followResults$Last.Name.2, leaderResults$Last.Name.1),
          Strategy.1 = c(leaderResults$Strategy.1, followResults$Strategy.2)
        )
        results <- results[order(
          results$Round, results$Role.1, results$Last.Name.1),]
      }
    } else {
      googlesheets4::gs4_deauth()
      results <- as.data.frame(googlesheets4::read_sheet(resultsSheet))
      results <- results[,-which(names(results) %in% "Timestamp")]
      colnames(results) <- make.names(colnames(results))
      results[[c(names$first)]] <- tidyr::replace_na(results[[c(names$first)]], "John")
      results[[c(names$last)]] <- tidyr::replace_na(results[[c(names$last)]], "Doe")
      results[, c(names$first)] <- ifelse(!is.na(results[, c(names$first)]), results[, c(names$first)], "John")
      results[, c(names$last)] <- ifelse(!is.na(results[, c(names$last)]), results[, c(names$last)], "Doe")
      leaderResults <- subset(results, Role == "Leader")
      followResults <- subset(results[, which(
        names(results) %in% c(
          names$first,
          names$last,
          names$round,
          names$role,
          names$strategy
        ))],
        Role == "Follower")
      colnames(results)[which(colnames(results) == names$first)] <-
        'First.Name.1'
      colnames(results)[which(colnames(results) == names$last)] <-
        'Last.Name.1'
      colnames(results)[which(colnames(results) == names$partnerFirst)] <-
        'First.Name.2'
      colnames(results)[which(colnames(results) == names$partnerLast)] <-
        'Last.Name.2'
      colnames(results)[which(colnames(results) == names$role)] <-
        'Role.1'
      colnames(results)[which(colnames(results) == names$strategy)] <-
        'Strategy.1'
      colnames(leaderResults)[which(colnames(leaderResults) == names$first)] <-
        'First.Name.1'
      colnames(leaderResults)[which(colnames(leaderResults) == names$last)] <-
        'Last.Name.1'
      colnames(leaderResults)[which(colnames(leaderResults) == names$partnerFirst)] <-
        'First.Name.2'
      colnames(leaderResults)[which(colnames(leaderResults) == names$partnerLast)] <-
        'Last.Name.2'
      colnames(leaderResults)[which(colnames(leaderResults) == names$role)] <-
        'Role.1'
      colnames(leaderResults)[which(colnames(leaderResults) == names$strategy)] <-
        'Strategy.1'
      colnames(followResults)[which(colnames(followResults) == names$first)] <-
        'First.Name.2'
      colnames(followResults)[which(colnames(followResults) == names$last)] <-
        'Last.Name.2'
      colnames(followResults)[which(colnames(followResults) == names$role)] <-
        'Role.2'
      colnames(followResults)[which(colnames(followResults) == names$strategy)] <-
        'Strategy.2'
      resultsWide <- merge(
        leaderResults,
        followResults,
        all = TRUE,
        by = c("First.Name.2", "Last.Name.2", "Round")
      )
      leaderResults <- leaderResults[, which(
        names(results) %in% c(
          "First.Name.1",
          "Last.Name.1",
          "Round",
          "Role.1",
          "Strategy.1"
        ))]
      roles <- resultsWide[, c("Round", "First.Name.1", "Last.Name.1", "Role.1", "First.Name.2", "Last.Name.2", "Role.2")]
      roles <- roles[order(roles$Round, roles$Last.Name.1, roles$First.Name.1),]
    }
    q.monopoly = (a - c) / (-2 * b)
    qi.c = q.monopoly / 2
    ql.d = (a - c) / (-2 * b)
    qf.dc = (a - c) / (-2 * b) - qi.c / 2 # if ql = ql.collude
    qf.dd = (a - c) / (-2 * b) - ql.d / 2  # if ql = ql.defect
    price <- function(x)
      a + b * (x[1] + x[2])
    p.cc <- price(c(qi.c, qi.c))
    p.cd <- price(c(qi.c, qf.dc))
    p.dc <- price(c(ql.d, qi.c))
    p.dd <- price(c(ql.d, qf.dd))
    profit <- function(x)
      (price(x) - c) * x - f
    leaderPayoffs <-
      c(profit(c(qi.c, qi.c))[1],
        profit(c(qi.c, qf.dc))[1],
        profit(c(ql.d, qi.c))[1],
        profit(c(ql.d, qf.dd))[1])
    followerPayoffs <-
      c(profit(c(qi.c, qi.c))[2],
        profit(c(qi.c, qf.dc))[2],
        profit(c(ql.d, qi.c))[2],
        profit(c(ql.d, qf.dd))[2])
    resultsWide <- within(resultsWide, {
      Q.1 <- ifelse(Strategy.1 == "Collude", qi.c, ql.d)
      Q.2 <- ifelse(
        is.na(Strategy.2), 0,
        ifelse(
          Strategy.2 == "Collude",
          qi.c,
          ifelse(Strategy.1 == "Collude", qf.dc, qf.dd)
          )
        )
      Pi.1 <- ifelse(
        Strategy.1 == "Collude",
        ifelse(
          is.na(Strategy.2),
          profit(c(qi.c, 0))[1],
          ifelse(
            Strategy.2 == "Collude",
            profit(c(qi.c, qi.c))[1],
            profit(c(qi.c, qf.dc))[1])
        ),
        ifelse(
          is.na(Strategy.2),
          profit(c(ql.d, 0))[1],
          ifelse(
            Strategy.2 == "Collude",
            profit(c(ql.d, qi.c))[1],
            profit(c(ql.d, qf.dd))[1])
        )
      )
      Pi.2 <- ifelse(
        Strategy.1 == "Collude",
        ifelse(
          is.na(Strategy.2),
          profit(c(qi.c, 0))[2],
          ifelse(
            Strategy.2 == "Collude",
            profit(c(qi.c, qi.c))[2],
            profit(c(qi.c, qf.dc))[2])
        ),
        ifelse(
          is.na(Strategy.2),
          profit(c(ql.d, 0))[2],
          ifelse(
            Strategy.2 == "Collude",
            profit(c(ql.d, qi.c))[2],
            profit(c(ql.d, qf.dd))[2])
        )
      )
    })
    results <- data.frame(
      First.Name = c(resultsWide$First.Name.1, resultsWide$First.Name.2),
      Last.Name = c(resultsWide$Last.Name.1, resultsWide$Last.Name.2),
      Partner.First.Name = c(resultsWide$First.Name.2, resultsWide$First.Name.1),
      Partner.Last.Name = c(resultsWide$Last.Name.2, resultsWide$Last.Name.1),
      Round = c(resultsWide$Round, resultsWide$Round),
      Role = c(resultsWide$Role.1, resultsWide$Role.2),
      Strategy = c(resultsWide$Strategy.1, resultsWide$Strategy.2),
      Partner.Strategy = c(resultsWide$Strategy.2, resultsWide$Strategy.1),
      Quantity = c(resultsWide$Q.1, resultsWide$Q.2),
      Partner.Quantity = c(resultsWide$Q.2, resultsWide$Q.1),
      Profit = c(resultsWide$Pi.1, resultsWide$Pi.2)
    )
    outcomes <- as.data.frame(cbind(
      First.Name = results$First.Name,
      Last.Name = results$Last.Name,
      outcomes = paste0(
        results$Strategy,
        "-",
        results$Partner.Strategy
      )
    ))
    payoffMatrix <- data.frame(
      Follower.Defect = c(
        paste0("(", leaderPayoffs[4], ", ", followerPayoffs[4], ")"),
        paste0("(", leaderPayoffs[2], ", ", followerPayoffs[2], ")")),
      Follower.Collude = c(
        paste0("(", leaderPayoffs[3], ", ", followerPayoffs[3], ")"),
        paste0("(", leaderPayoffs[1], ", ", followerPayoffs[1], ")")),
      row.names = c("Leader.Defect", "Leader.Collude"))
    outputMatrix <- data.frame(
      Follower.Defect = c(
        paste0("(", round(ql.d, 4), ", ", round(qf.dd, 4), ")"),
        paste0("(", round(qi.c, 4), ", ", round(qf.dc, 4), ")")),
      Follower.Collude = c(
        paste0("(", round(ql.d, 4), ", ", round(qi.c,  4), ")"),
        paste0("(", round(qi.c, 4), ", ", round(qi.c,  4), ")")),
      row.names = c("Leader.Defect", "Leader.Collude"))
    priceMatrix <- data.frame(
      Follower.Defect = c(round(p.dd, 4), round(p.cd, 4)),
      Follower.Collude = c(round(p.dc, 4), round(p.cc, 4)),
      row.names = c("Leader.Defect", "Leader.Collude"))
    grades <-
      aggregate(Profit ~ First.Name + Last.Name, data = results, FUN = sum)
    colnames(grades) <- c("First Name", "Last Name", "Score")
    tree <- gameTree(players = c("Leader", "Follower"),
      payoffs1 = leaderPayoffs, payoffs2 = followerPayoffs)
    out <- list(
      type = "stackelbergGame",
      roles = roles,
      leaderResults = leaderResults,
      followResults = followResults,
      tree = tree,
      outcomes = outcomes,
      payoff = payoffMatrix,
      output = outputMatrix,
      price = priceMatrix,
      results = results[order(results$Round,
                              results$Last.Name,
                              results$First.Name), ],
      grades = grades[order(grades$`Last Name`, grades$`First Name`), ]
    )
    class(out) <- c('econGame', class(out))
    out
  }
