##' Tabulate results for a simple in-class stag hunt game.
##'
##' @details \code{bertrandGame} tabulates the results of a simple stag hunt game in which students' points depend on their strategy and the strategy chosen by their randomly-assigned partners.
##'
##' @param sheet (required) is a character string sheet ID corresponding to the Google Sheets location containing the individual submissions.
##' @param a is the value of the intercept of the linear inverse-demand function (default is 10).
##' @param b is the value of the slope of the linear inverse-demand function (default is -1).
##' @param c is the value of the firm's marginal cost (default is 6).
##' @param f is the value of the firm's fixed cost (default is 0).
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
           ...) {
    # Set up the Google Sheets, read responses, and initialize output objects.
    results <- read_sheet(sheet)
    if (a  <= 0)
      stop("The intercept of the demand function needs to be positive.")
    if (b  >= 0)
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
    partnerResults <- results[, 4:7]
    colnames(partnerResults)[4] <- "Partner.Price"
    results <- merge(
      results,
      partnerResults,
      all = TRUE,
      by.x = c("First.Name", "Last.Name", "Round"),
      by.y = c("Partner.First.Name", "Partner.Last.Name", "Round")
    )
    results <- within(results, {
      Price.Market <-
        ifelse(Price < Partner.Price,
               Price,
               Partner.Price)
      Q.Market <- (a - Price.Market) / (-b)
      Q.Student <- ifelse(Price < Partner.Price, Q.Market,
                          ifelse(Price == Partner.Price, Q.Market/2,0))
      Profit <- Q.Student*(Price.Market - c) - f
    })
    grades <-
      aggregate(Profit ~ First.Name + Last.Name,
                data = results,
                FUN = sum)
    colnames(grades) <- c("Last Name", "First Name", "Score")
    out <- list(
      type = "bertrandGame",
      results = results[order(results$Round,
                              results$Last.Name,
                              results$First.Name),-which(names(results) %in% 'Timestamp')],
      grades = grades[order(grades$`Last Name`, grades$`First Name`),]
    )
    class(out) <- c('econGame', class(out))
    out
  }
