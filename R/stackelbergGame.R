##' Tabulate results for a simple in-class Stackelberg duopoly game.
##'
##' @details \code{cournotGame} tabulates the results of a simple Stackelberg duopoly game in which students' points depend on their strategy and the strategy chosen by their randomly-assigned partners.
##'
##' @param sheet (required) is a character string sheet ID corresponding to the Google Sheets location containing the individual submissions.
##' @param a is the value of the intercept of the linear inverse-demand function (default is 10).
##' @param b is the value of the slope of the linear inverse-demand function (default is -1).
##' @param c is the value of the firm's marginal cost (default is 6).
##' @param f is the value of the firm's fixed cost (default is 0).
##'
##' @return \code{type} returns the type of activity (cournotGame).
##' @return \code{results} returns the original submissions (with equilibria and points per round added).
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @export

stackelbergGame <-
  function(sheet,
           a = 10,
           b = -1,
           c = 6,
           f = 0,
           ...) {
    # Set up the Google Sheets, read responses, and initialize output objects.
    results <- read_sheet(sheet)
    if (a <= 0)
      stop("The intercept of the demand function needs to be positive.")
    if (b >= 0)
      stop("Demand curves are downward-sloping!")
    if (c <= 0)
      stop("There ain't no such thing as a free lunch (TANSTAAFL)!")
    if (f <  0)
      stop("Fixed costs must be non-negative.")
    colnames(results) <- make.names(colnames(results))
    results <-
      replace_na(results, list(First.Name = "John", Last.Name = "Doe"))
    results$First.Name <- str_to_title(results$First.Name)
    results$Last.Name <- str_to_title(results$Last.Name)
    results$Partner.First.Name <-
      str_to_title(results$Partner.First.Name)
    results$Partner.Last.Name <-
      str_to_title(results$Partner.Last.Name)
    partnerResults <-
      results[, which(
        names(results) %in% c(
          "Partner.First.Name",
          "Partner.Last.Name",
          "Round",
          "Strategy"
        )
      )]
    colnames(partnerResults)[4] <- "Partner.Strategy"
    results <- merge(
      results,
      partnerResults,
      all = TRUE,
      by.x = c("First.Name", "Last.Name", "Round"),
      by.y = c("Partner.First.Name", "Partner.Last.Name", "Round"),
      suffixes = c("", ".Partner")
    )
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
    leaderResults <- subset(results, Role == "Leader")
    followerResults <- subset(results, Role == "Follower")
    leaderResults <- within(leaderResults, {
      Q.Student <- ifelse(Strategy == "Collude", qi.c, ql.d)
      Q.Partner <- ifelse(is.na(Partner.Strategy),
                          0,
                          ifelse(
                            Partner.Strategy == "Collude",
                            qi.c,
                            ifelse(Strategy == "Collude", qf.dc, qf.dd)
                          ))
      Profit <- ifelse(
        Strategy == "Collude",
        ifelse(
          is.na(Partner.Strategy),
          profit(c(qi.c, 0)),
          ifelse(Partner.Strategy == "Collude",
                 profit(c(qi.c, qi.c)),
                 profit(c(qi.c, qf.dc)))
        ),
        ifelse(
          is.na(Partner.Strategy),
          profit(c(ql.d, 0)),
          ifelse(Partner.Strategy == "Collude",
                 profit(c(ql.d, qi.c)),
                 profit(c(ql.d, qf.dd)))
        )
      )
    })
    if (nrow(followerResults) > 0) {
      followerResults <- within(followerResults, {
        Q.Partner <- ifelse(Partner.Strategy == "Collude", qi.c, ql.d)
        Q.Student <- ifelse(Strategy == "Collude",
                            qi.c,
                            ifelse(Partner.Strategy == "Collude", qf.dc, qf.dd))
        Profit <- ifelse(
          Partner.Strategy == "Collude",
          ifelse(Strategy == "Collude",
                 profit(c(qi.c, qi.c)),
                 profit(c(qi.c, qf.dc))),
          ifelse(Strategy == "Collude",
                 profit(c(ql.d, qi.c)),
                 profit(c(ql.d, qf.dd)))
        )
      })
    }
    results <- rbind(leaderResults, followerResults)
    outcomes <- as.data.frame(cbind(
      First.Name = leaderResults$First.Name,
      Last.Name = leaderResults$Last.Name,
      outcomes = paste0(
        leaderResults$Strategy,
        "-",
        leaderResults$Partner.Strategy
      )
    ))
    payoffMatrix <- matrix(c(
      paste0("(", leaderPayoffs[4], ", ", followerPayoffs[4], ")"),
      paste0("(", leaderPayoffs[2], ", ", followerPayoffs[2], ")"),
      paste0("(", leaderPayoffs[3], ", ", followerPayoffs[3], ")"),
      paste0("(", leaderPayoffs[1], ", ", followerPayoffs[1], ")")
    ),
    nrow = 2,
    ncol = 2)
    colnames(payoffMatrix) <-
      c("Partner = Defect", "Partner = Collude")
    rownames(payoffMatrix) <-
      c("Strategy = Defect", "Strategy = Collude")
    outputMatrix <- matrix(c(
      paste0("(", round(ql.d, 4), ", ", round(qf.dd, 4), ")"),
      paste0("(", round(qi.c, 4), ", ", round(qf.dc, 4), ")"),
      paste0("(", round(ql.d, 4), ", ", round(qi.c,  4), ")"),
      paste0("(", round(qi.c, 4), ", ", round(qi.c,  4), ")")
    ),
    nrow = 2,
    ncol = 2)
    colnames(outputMatrix) <-
      c("Partner = Defect", "Partner = Collude")
    rownames(outputMatrix) <-
      c("Strategy = Defect", "Strategy = Collude")
    priceMatrix <- matrix(c(round(p.dd, 4),
                            round(p.cd, 4),
                            round(p.dc, 4),
                            round(p.cc, 4)),
                          nrow = 2,
                          ncol = 2)
    colnames(priceMatrix) <-
      c("Partner = Defect", "Partner = Collude")
    rownames(priceMatrix) <-
      c("Strategy = Defect", "Strategy = Collude")
    grades <-
      aggregate(Profit ~ First.Name + Last.Name,
                data = results,
                FUN = sum)
    tree <-
      gameTree(
        players = c("Leader", "Follower"),
        payoffs1 = leaderPayoffs,
        payoffs2 = followerPayoffs
      )
    colnames(grades) <- c("Last Name", "First Name", "Score")
    out <- list(
      type = "stackelbergGame",
      payoff = cbind(leaderPayoffs, followerPayoffs),
      tree = tree,
      leaderResults = leaderResults,
      followerResults = followerResults,
      outcomes = outcomes,
      payoff = payoffMatrix,
      output = outputMatrix,
      price = priceMatrix,
      results = results[order(results$Round,
                              results$Last.Name,
                              results$First.Name), -which(names(results) %in% 'Timestamp')],
      grades = grades[order(grades$`Last Name`, grades$`First Name`), ]
    )
    class(out) <- c('econGame', class(out))
    out
  }
