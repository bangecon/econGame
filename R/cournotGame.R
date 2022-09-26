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
           ...) {
    # Set up the Google Sheets, read responses, and initialize output objects.
    results <- read_sheet(sheet)
    if(a  <= 0) stop("The intercept of the demand function needs to be positive.")
    if(b  >= 0) stop("Demand curves are downward-sloping!")
    if(c <= 0) stop("There ain't no such thing as a free lunch (TANSTAAFL)!")
    if(f <  0) stop("Fixed costs must be non-negative.")
    colnames(results) <- make.names(colnames(results))
    results <-
      replace_na(results, list(First.Name = "John", Last.Name = "Doe"))
    results$First.Name <- str_to_title(results$First.Name)
    results$Last.Name <- str_to_title(results$Last.Name)
    results$Partner.First.Name <-
      str_to_title(results$Partner.First.Name)
    results$Partner.Last.Name <-
      str_to_title(results$Partner.Last.Name)
    partnerResults <- results[, 4:7]
    colnames(partnerResults)[4] <- "Partner.Strategy"
    results <- merge(
      results,
      partnerResults,
      all = TRUE,
      by.x = c("First.Name", "Last.Name", "Round"),
      by.y = c("Partner.First.Name", "Partner.Last.Name", "Round")
    )
    q.monopoly = (a - c)/(-2*b)
    qi.collude = q.monopoly/2
    qi.defects = (a - c)/(-3*b)
    qc.offdiag = q.monopoly/2
    qd.offdiag = (a - c + b*qc.offdiag)/(-2*b)
    price <- function(x) a + b*(x[1] + x[2])
    results <- within(results, {
      Q.Student <- ifelse(
        Partner.Strategy == "Collude",
        ifelse(Strategy == "Collude", qi.collude, qd.offdiag),
        ifelse(Strategy == "Collude", qc.offdiag, qi.defects)
      )
      Q.Partner <- ifelse(
        Partner.Strategy == "Collude",
        ifelse(Strategy == "Collude", qi.collude, qc.offdiag),
        ifelse(Strategy == "Collude", qd.offdiag, qi.defects)
      )
      Price <- ifelse(
        Partner.Strategy == "Collude",
        ifelse(Strategy == "Collude", price(c(qi.collude, qi.collude)), price(c(qd.offdiag, qc.offdiag))),
        ifelse(Strategy == "Collude", price(c(qd.offdiag, qc.offdiag)), price(c(qi.defects, qi.defects)))
      )
    })
    results$Profit <- results$Q.Student*(results$Price - c) - f
    payoffMatrix <- matrix(c(
      paste0("(", round(qi.defects*(price(c(qi.defects, qi.defects)) - c) - f, 4), ", ",
             round(qi.defects*(price(c(qi.defects, qi.defects)) - c) - f, 4), ")"),
      paste0("(", round(qc.offdiag*(price(c(qc.offdiag, qd.offdiag)) - c) - f, 4), ", ",
             round(qd.offdiag*(price(c(qc.offdiag, qd.offdiag)) - c) - f, 4), ")"),
      paste0("(", round(qc.offdiag*(price(c(qc.offdiag, qd.offdiag)) - c) - f, 4), ", ",
             round(qd.offdiag*(price(c(qc.offdiag, qd.offdiag)) - c) - f, 4), ")"),
      paste0("(", round(qi.collude*(price(c(qi.collude, qi.collude)) - c) - f, 4), ", ",
             round(qi.collude*(price(c(qi.collude, qi.collude)) - c) - f, 4), ")")),
      nrow = 2, ncol = 2)
    colnames(payoffMatrix) <-
      c("Partner = Defect", "Partner = Collude")
    rownames(payoffMatrix) <-
      c("Strategy = Defect", "Strategy = Collude")
    outputMatrix <- matrix(c(
      paste0("(", round(qi.defects, 4), ", ", round(qi.defects, 4), ")"),
      paste0("(", round(qc.offdiag, 4), ", ", round(qd.offdiag, 4), ")"),
      paste0("(", round(qd.offdiag, 4), ", ", round(qc.offdiag, 4), ")"),
      paste0("(", qi.collude, ", ", qi.collude, ")")),
      nrow = 2, ncol = 2)
    colnames(outputMatrix) <-
      c("Partner = Defect", "Partner = Collude")
    rownames(outputMatrix) <-
      c("Strategy = Defect", "Strategy = Collude")
    priceMatrix <- matrix(c(
      round(price(c(qi.defects, qi.defects)), 4),
      round(price(c(qc.offdiag, qd.offdiag)), 4),
      round(price(c(qc.offdiag, qd.offdiag)), 4),
      round(price(c(qi.collude, qi.collude)), 4)),
      nrow = 2, ncol = 2)
    colnames(priceMatrix) <-
      c("Partner = Defect", "Partner = Collude")
    rownames(priceMatrix) <-
      c("Strategy = Defect", "Strategy = Collude")
    grades <-
      aggregate(Profit ~ First.Name + Last.Name,
                data = results,
                FUN = sum)
    colnames(grades) <- c("First Name", "Last Name", "Score")
    results$Outcome <-
      paste0(results$Strategy, "-", results$Partner.Strategy)
    results$Outcome <-
      ifelse(results$Outcome == "Collude-Defect",
             "Defect-Collude",
             results$Outcome)
    out <- list(
      type = "cournotGame",
      payoff = payoffMatrix,
      output = outputMatrix,
      price = priceMatrix,
      results = results[order(results$Round,
                              results$Last.Name,
                              results$First.Name),-which(names(results) %in% 'Timestamp')],
      grades = grades[order(grades$`Last Name`, grades$`First Name`),]
    )
    class(out) <- c('econGame', class(out))
    out
  }
