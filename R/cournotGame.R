##' Tabulate results for a simple in-class Cournot duopoly game.
##'
##' @details \code{cournotGame} tabulates the results of a simple Cournot duopoly game in which students' points depend on their strategy and the strategy chosen by their randomly-assigned partners.
##'
##' @param sheet (required) is a character string sheet ID corresponding to the Google Sheets location containing the individual submissions.
##' @param partners is a character string equal to \code{'students'} or \code{'random'} indicating whether students choose their own partners (default) or whether the function should generate them randomly after the students have decided their strategies. If \code{partners = 'students'}, then the form must include fields for the student's own first and last names and fields for their partners' first and last names. If partners are random, then the columns in `sheet` should either contain columns named 'First Name' and 'Last Name', or the user needs to specify the columns in `sheet` corresponding to this information to pass to `randomGroups()`.
##' @param a is the value of the intercept of the linear inverse-demand function (default is 10).
##' @param b is the value of the slope of the linear inverse-demand function (default is -1).
##' @param c is the value of the firm's marginal cost (default is 6).
##' @param f is the value of the firm's fixed cost (default is 0).
##' @param names character list of the column names in `sheet`.
##' @param auth is a logical indicating whether to use an authentication token to access the Sheet containing the individual submissions.
##' @param email is an email address that matches the user account containing the Sheet with the individual submissions (if `auth == TRUE`).
##'
##' @return \code{type} returns the type of activity (`cournotGame`).
##' @return \code{payoff} returns the payoff matrix.
##' @return \code{output} returns the matrix of quantities corresponding to the strategies.
##' @return \code{price} returns the matrix of prices corresponding to the total merket output for each combination of strategies.
##' @return \code{results} returns the original submissions (with equilibria and points per round added).
##' @return \code{grades} returns the aggregated points 'won' by each student for the entire activity.
##'
##' @export

cournotGame <-
  function(sheet = NULL,
           a = 10,
           b = -1,
           c = 6,
           f = 0,
           auth = FALSE,
           partners = 'random',
           names = NULL,
           ...) {
    # Set up the Google Sheets, read responses, and initialize output objects.
    if (a  <= 0)
      stop("The intercept of the demand function needs to be positive.")
    if (b  >= 0)
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
    }
    else {
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
    if (is.null(sheet)) {
      results <- data.frame(
        First.Name.1 = c("John", "Jane"),
        Last.Name.1 = c("Smith", "Doe"),
        First.Name.2 = c("Jane", "John"),
        Last.Name.2 = c("Doe", "Smith"),
        Round = c(1, 1),
        Strategy.1 = c("Collude", "Defect"),
        Strategy.2 = c("Defect", "Collude")
        )
    } else {
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
    Q.M = (a - c) / (-2 * b)
    Q.C = Q.M / 2
    Q.D = (a - c) / (-3 * b)
    Q.C.off = Q.M / 2
    Q.D.off = (a - c + b * Q.C.off) / (-2 * b)
    price <- function(x)
      a + b * (x[1] + x[2])
    results <- within(results, {
      Q.Student <- ifelse(
        Strategy.2 == "Collude",
        ifelse(Strategy.1 == "Collude", Q.C, Q.D.off),
        ifelse(Strategy.1 == "Collude", Q.C.off, Q.D)
      )
      Q.Partner <- ifelse(
        Strategy.2 == "Collude",
        ifelse(Strategy.1 == "Collude", Q.C, Q.C.off),
        ifelse(Strategy.1 == "Collude", Q.D.off, Q.D)
      )
      Price <- ifelse(
        Strategy.2 == "Collude",
        ifelse(Strategy.1 == "Collude", price(c(Q.C, Q.C)), price(c(
          Q.D.off, Q.C.off
        ))),
        ifelse(Strategy.1 == "Collude", price(c(
          Q.D.off, Q.C.off
        )), price(c(Q.D, Q.D)))
      )
    })
    results$Profit <- results$Q.Student * (results$Price - c) - f
    payoffMatrix <- matrix(c(
      paste0("(", round(Q.D * (price(
        c(Q.D, Q.D)
      ) - c) - f, 4), ", ",
      round(Q.D * (price(
        c(Q.D, Q.D)
      ) - c) - f, 4), ")"),
      paste0("(", round(Q.C.off * (price(
        c(Q.C.off, Q.D.off)
      ) - c) - f, 4), ", ",
      round(Q.D.off * (price(
        c(Q.C.off, Q.D.off)
      ) - c) - f, 4), ")"),
      paste0("(", round(Q.C.off * (price(
        c(Q.C.off, Q.D.off)
      ) - c) - f, 4), ", ",
      round(Q.D.off * (price(
        c(Q.C.off, Q.D.off)
      ) - c) - f, 4), ")"),
      paste0("(", round(Q.C * (price(
        c(Q.C, Q.C)
      ) - c) - f, 4), ", ",
      round(Q.C * (price(
        c(Q.C, Q.C)
      ) - c) - f, 4), ")")
    ),
    nrow = 2,
    ncol = 2)
    colnames(payoffMatrix) <-
      c("Partner = Defect", "Partner = Collude")
    rownames(payoffMatrix) <-
      c("Strategy = Defect", "Strategy = Collude")
    outputMatrix <- matrix(c(
      paste0("(", round(Q.D, 4), ", ", round(Q.D, 4), ")"),
      paste0("(", round(Q.C.off, 4), ", ", round(Q.D.off, 4), ")"),
      paste0("(", round(Q.D.off, 4), ", ", round(Q.C.off, 4), ")"),
      paste0("(", Q.C, ", ", Q.C, ")")
    ),
    nrow = 2,
    ncol = 2)
    colnames(outputMatrix) <-
      c("Partner = Defect", "Partner = Collude")
    rownames(outputMatrix) <-
      c("Strategy = Defect", "Strategy = Collude")
    priceMatrix <- matrix(c(
      round(price(c(Q.D, Q.D)), 4),
      round(price(c(Q.C.off, Q.D.off)), 4),
      round(price(c(Q.C.off, Q.D.off)), 4),
      round(price(c(Q.C, Q.C)), 4)
    ),
    nrow = 2,
    ncol = 2)
    colnames(priceMatrix) <-
      c("Partner = Defect", "Partner = Collude")
    rownames(priceMatrix) <-
      c("Strategy = Defect", "Strategy = Collude")
    grades <-
      aggregate(Profit ~ First.Name.1 + Last.Name.1,
                data = results,
                FUN = sum)
    colnames(grades) <- c("First Name", "Last Name", "Score")
    results$Outcome <-
      paste0(results$Strategy.1, "-", results$Strategy.2)
    results$Outcome <-
      ifelse(results$Outcome == "Collude-Defect",
             "Defect-Collude",
             results$Outcome)
    out <- list(
      type = "cournotGame",
      payoff = payoffMatrix,
      output = outputMatrix,
      price = priceMatrix,
      results = results[order(results$Last.Name.1,
                              results$First.Name.1),],
      grades = grades[order(grades$`Last Name`, grades$`First Name`), ]
    )
    class(out) <- c('econGame', class(out))
    out
  }
