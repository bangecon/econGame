##' Tabulate results for a simple in-class Cournot duopoly game.
##'
##' @details \code{cournotGame} tabulates the results of a simple Cournot duopoly game in which students' points depend on their strategy and the strategy chosen by their randomly-assigned partners.
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

cournotGame <-
  function(sheet,
           a = 10,
           b = -1,
           c = 6,
           f = 0,
           partners = 'random',
           ...) {
    # Set up the Google Sheets, read responses, and initialize output objects.
    if(a  <= 0) stop("The intercept of the demand function needs to be positive.")
    if(b  >= 0) stop("Demand curves are downward-sloping!")
    if(c <= 0) stop("There ain't no such thing as a free lunch (TANSTAAFL)!")
    if(f <  0) stop("Fixed costs must be non-negative.")
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
          'Strategy.2',
          'First.Name.1',
          'Last.Name.1',
          'Strategy.1'
        )
      #      results <- rbind(results, partnerResults)
    } else {
      results <- as.data.frame(googlesheets4::read_sheet(sheet))[-1]
      colnames(results) <-
        c(
          'First.Name.1',
          'Last.Name.1',
          'First.Name.2',
          'Last.Name.2',
          'Round.1',
          'Strategy.1'
        )
      results <-
        tidyr::replace_na(results, list(First.Name.1 = "John", Last.Name.1 = "Doe",
                                        First.Name.2 = "Jane", Last.Name.2 = "Doe"))
      results$First.Name.1 <-
        stringr::str_to_title(results$First.Name.1)
      results$Last.Name.1 <-
        stringr::str_to_title(results$Last.Name.1)
      results$First.Name.2 <-
        stringr::str_to_title(results$First.Name.2)
      results$Last.Name.2 <-
        stringr::str_to_title(results$Last.Name.2)
      partnerResults <-
        results[, which(names(results) %in% c('First.Name.2', 'Last.Name.2', 'Round.1', 'Strategy.1'))]
      colnames(partnerResults)[3:4] <- c("Round.2", "Strategy.2")
      results <- merge(
        results,
        partnerResults,
        all = TRUE,
        by.x = c("First.Name.1", "Last.Name.1", "Round.1"),
        by.y = c("First.Name.2", "Last.Name.2", "Round.2")
      )
    }
    Q.M = (a - c)/(-2*b)
    Q.C = Q.M/2
    Q.D = (a - c)/(-3*b)
    Q.C.off = Q.M/2
    Q.D.off = (a - c + b*Q.C.off)/(-2*b)
    price <- function(x) a + b*(x[1] + x[2])
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
        ifelse(Strategy.1 == "Collude", price(c(Q.C, Q.C)), price(c(Q.D.off, Q.C.off))),
        ifelse(Strategy.1 == "Collude", price(c(Q.D.off, Q.C.off)), price(c(Q.D, Q.D)))
      )
    })
    results$Profit <- results$Q.Student*(results$Price - c) - f
    payoffMatrix <- matrix(c(
      paste0("(", round(Q.D*(price(c(Q.D, Q.D)) - c) - f, 4), ", ",
             round(Q.D*(price(c(Q.D, Q.D)) - c) - f, 4), ")"),
      paste0("(", round(Q.C.off*(price(c(Q.C.off, Q.D.off)) - c) - f, 4), ", ",
             round(Q.D.off*(price(c(Q.C.off, Q.D.off)) - c) - f, 4), ")"),
      paste0("(", round(Q.C.off*(price(c(Q.C.off, Q.D.off)) - c) - f, 4), ", ",
             round(Q.D.off*(price(c(Q.C.off, Q.D.off)) - c) - f, 4), ")"),
      paste0("(", round(Q.C*(price(c(Q.C, Q.C)) - c) - f, 4), ", ",
             round(Q.C*(price(c(Q.C, Q.C)) - c) - f, 4), ")")),
      nrow = 2, ncol = 2)
    colnames(payoffMatrix) <-
      c("Partner = Defect", "Partner = Collude")
    rownames(payoffMatrix) <-
      c("Strategy = Defect", "Strategy = Collude")
    outputMatrix <- matrix(c(
      paste0("(", round(Q.D, 4), ", ", round(Q.D, 4), ")"),
      paste0("(", round(Q.C.off, 4), ", ", round(Q.D.off, 4), ")"),
      paste0("(", round(Q.D.off, 4), ", ", round(Q.C.off, 4), ")"),
      paste0("(", Q.C, ", ", Q.C, ")")),
      nrow = 2, ncol = 2)
    colnames(outputMatrix) <-
      c("Partner = Defect", "Partner = Collude")
    rownames(outputMatrix) <-
      c("Strategy = Defect", "Strategy = Collude")
    priceMatrix <- matrix(c(
      round(price(c(Q.D, Q.D)), 4),
      round(price(c(Q.C.off, Q.D.off)), 4),
      round(price(c(Q.C.off, Q.D.off)), 4),
      round(price(c(Q.C, Q.C)), 4)),
      nrow = 2, ncol = 2)
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
                              results$First.Name.1), ],
      grades = grades[order(grades$`Last Name`, grades$`First Name`),]
    )
    class(out) <- c('econGame', class(out))
    out
  }
