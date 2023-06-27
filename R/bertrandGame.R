##' Tabulate results for a simple in-class stag hunt game.
##'
##' @details \code{bertrandGame} tabulates the results of a simple stag hunt game in which students' points depend on their strategy and the strategy chosen by their randomly-assigned partners.
##'
##' @param sheet (required) is a character string sheet ID corresponding to the Google Sheets location containing the individual submissions.
##' @param a is the value of the intercept of the linear inverse-demand function (default is 10).
##' @param b is the value of the slope of the linear inverse-demand function (default is -1).
##' @param c is the value of the firm's marginal cost (default is 6).
##' @param f is the value of the firm's fixed cost (default is 0).
##' @param partners is a character string equal to \code{'students'} or \code{'random'} indicating whether students choose their own partners or whether the function should generate them randomly after the students have decided their strategies.
##'
##' @return \code{type} returns the type of activity (bertrandGame).
##' @return \code{results} returns the original submissions (with equilibria and points per round added).
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @export

bertrandGame <-
  function(sheet,
           a = 10,
           b = -1,
           c = 6,
           f = 0,
           partners = 'random',
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
    if (partners == 'random') {
      results <- randomGroups(sheet)
      colnames(results) <- make.names(colnames(results))
      results <-
        tidyr::replace_na(results, list(First.Name = "John", Last.Name = "Doe"))
      results$First.Name <-
        stringr::str_to_title(results$First.Name)
      results$Last.Name <- stringr::str_to_title(results$Last.Name)
      results <-
        results[,-which(names(results) %in% c('Timestamp'))]
      results <-
        reshape(results,
                direction = 'wide',
                idvar = 'group',
                timevar = 'member')[,-1]
      partnerResults <- results
      colnames(partnerResults) <-
        c(
          'First.Name.2',
          'Last.Name.2',
          'Round.2',
          'Price.2',
          'First.Name.1',
          'Last.Name.1',
          'Round.1',
          'Price.1'
        )
      results <- rbind(results, partnerResults)[,-which(names(results) %in% 'Round.2')]
    } else {
      results <- as.data.frame(googlesheets4::read_sheet(sheet))[-1]
      colnames(results) <-
        c(
          'First.Name.1',
          'Last.Name.1',
          'First.Name.2',
          'Last.Name.2',
          'Round.1',
          'Price.1'
        )
      results <-
        tidyr::replace_na(results, list(First.Name = "John", Last.Name = "Doe"))
      results$First.Name.1 <-
        stringr::str_to_title(results$First.Name.1)
      results$Last.Name.1 <-
        stringr::str_to_title(results$Last.Name.1)
      results$First.Name.2 <-
        stringr::str_to_title(results$First.Name.2)
      results$Last.Name.2 <-
        stringr::str_to_title(results$Last.Name.2)
      partnerResults <-
        results[, which(names(results) %in% c('First.Name.2', 'Last.Name.2', 'Round.1', 'Price.1'))]
      colnames(partnerResults)[3:4] <- c("Round.2", "Price.2")
      results <- merge(
        results,
        partnerResults,
        all = TRUE,
        by.x = c("First.Name.1", "Last.Name.1", "Round.1"),
        by.y = c("First.Name.2", "Last.Name.2", "Round.2")
      )
    }
    results$Price.M <-
      apply(results[, which(names(results) %in% c('Price.1', 'Price.2'))], 1, min)
    results$Q.M <- (a - results$Price.M) / (-b)
    results$Q.1 <- ifelse(
      results$Price.1 > results$Price.M,
      0,
      ifelse(
        results$Price.1 == results$Price.2,
        results$Q.M / 2,
        results$Q.M
      )
    )
    Price.C <- (a + c)/2
    Q.C <- (a - c)/(-2*b)
    Profit.C <- (a - c)^2/(-4*b)
    results$Profit <- results$Q.1 * (results$Price.M - c) - f
    grades <-
      aggregate(Profit ~ First.Name.1 + Last.Name.1,
                data = results,
                FUN = sum)
    colnames(grades) <- c("First Name", "Last Name", "Score")

    payoffMatrix <- matrix(c(
      paste0("(", 0, ", ", 0, ")"),
      paste0("(", 0, ", ", round((Price.C - 0.01 - c)*(a + b*(Price.C - 0.01)) - f, 4), ")"),
      paste0("(", round((Price.C - 0.01 - c)*(a + b*(Price.C - 0.01)) - f, 4), ", ", 0, ")"),
      paste0("(", round(Profit.C/2, 2), ", ", round(Profit.C/2, 2), ")")),
      nrow = 2, ncol = 2)
    colnames(payoffMatrix) <-
      c("Partner = Defect", "Partner = Collude")
    rownames(payoffMatrix) <-
      c("Strategy = Defect", "Strategy = Collude")
    outputMatrix <- matrix(c(
      paste0("(", (a + b*c)/2, ", ", (a + b*c)/2, ")"),
      paste0("(", 0, ", ", a + b*(Price.C - 0.01), ")"),
      paste0("(", a + b*(Price.C - 0.01), ", ", 0, ")"),
      paste0("(", Q.C, ", ", Q.C, ")")),
      nrow = 2, ncol = 2)
    colnames(outputMatrix) <-
      c("Partner = Defect", "Partner = Collude")
    rownames(outputMatrix) <-
      c("Strategy = Defect", "Strategy = Collude")
    priceMatrix <- matrix(c(c, Price.C - 0.01, Price.C - 0.01, Price.C),
                          nrow = 2, ncol = 2)
    colnames(priceMatrix) <-
      c("Partner = Defect", "Partner = Collude")
    rownames(priceMatrix) <-
      c("Strategy = Defect", "Strategy = Collude")

    out <- list(type = "bertrandGame",
                payoff = payoffMatrix,
                output = outputMatrix,
                price = priceMatrix,
                results = results[order(results$Last.Name.1,
                                        results$First.Name.1),],
                grades = grades[order(grades$`Last Name`, grades$`First Name`), ])
    class(out) <- c('econGame', class(out))
    out
  }
