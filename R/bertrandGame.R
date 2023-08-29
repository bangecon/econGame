##" Tabulate results for a simple in-class bertrand duopoly game.
##"
##" @details \code{bertrandGame} tabulates the results of a simple bertrand duopoly game in which students" points depend on their strategy and the strategy chosen by their randomly-assigned partners.
##"
##" @param sheet (required) is a character string sheet ID corresponding to the Google Sheets location containing the individual submissions.
##" @param a is the value of the intercept of the linear inverse-demand function (default is 10).
##" @param b is the value of the slope of the linear inverse-demand function (default is -1).
##" @param c is the value of the firm"s marginal cost (default is 6).
##" @param f is the value of the firm"s fixed cost (default is 0).
##" @param partners is a character string equal to \code{"students"} or \code{"random"} indicating whether students choose their own partners (default) or whether the function should generate them randomly after the students have decided their strategies. If \code{partners = "students"}, then the form must include fields for the student"s own first and last names and fields for their partners" first and last names. If partners are random, then the columns in `sheet` should either contain columns named "First Name" and "Last Name", or the user needs to specify the columns in `sheet` corresponding to this information to pass to `randomGroups()`.
##"
##" @return \code{type} returns the type of activity (`bertrandGame`).
##" @return \code{results} returns the original submissions (with equilibria and points per round added).
##" @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##"
##" @export

bertrandGame <-
  function(sheet,
           a = 10,
           b = -1,
           c = 6,
           f = 0,
           partners = "random",
           auth = FALSE, 
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
    if (auth == TRUE) {
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
      if (partners == "students") {
        names <- list(
          first = "First.Name",
          last = "Last.Name",
          partnerFirst = "Partner.First.Name",
          partnerLast = "Partner.Last.Name",
          round = "Round",
          price = "Price")
      } else {
        names <- list(
          first = "First.Name",
          last = "Last.Name",
          round = "Round",
          price = "Price")
      }
    } else {
      names <- lapply(names, make.names)
    }
    if (partners == "random") {
      results <- randomGroups(sheet)
      results <- results[, -which(names(results) %in% "Timestamp")]
      results[[c(names$first)]] <- tidyr::replace_na(
        results[[c(names$first)]], "John")
      results[[c(names$last)]] <- tidyr::replace_na(
        results[[c(names$last)]], "Doe")
      results[[c(names$first)]] <- stringr::str_to_title(
        results[[c(names$first)]])
      results[[c(names$last)]] <- stringr::str_to_title(
        results[[c(names$last)]])
      results <-as.data.frame(results)
      colnames(results)[which(colnames(results) == names$first)] <- "First.Name"
      colnames(results)[which(colnames(results) == names$last)] <- "Last.Name"
      colnames(results)[which(colnames(results) == names$price)] <- "Price"
      results <-
        reshape(results,
                direction = "wide",
                idvar = c("Round", "group"),
                timevar = "member")
      partnerResults <- results
      colnames(partnerResults)[which(
        colnames(partnerResults) %in% c(
          "First.Name.1",
          "Last.Name.1",
          "Price.1",
          "First.Name.2",
          "Last.Name.2",
          "Price.2"
        )
      )] <-
        c("First.Name.2",
          "Last.Name.2",
          "Price.2",
          "First.Name.1",
          "Last.Name.1",
          "Price.1")
      results <- rbind(results, partnerResults)
    } else {
      results <- as.data.frame(googlesheets4::read_sheet(sheet))
      results <- results[, -which(names(results) %in% "Timestamp")]
      colnames(results) <- make.names(colnames(results))
      results[[c(names$first)]] <- tidyr::replace_na(
        results[[c(names$first)]], "John")
      results[[c(names$last)]] <- tidyr::replace_na(
        results[[c(names$last)]], "Doe")
      results[, c(names$first)] <- ifelse(
        !is.na(results[, c(names$first)]), results[, c(names$first)], "John")
      results[, c(names$last)] <- ifelse(
        !is.na(results[, c(names$last)]), results[, c(names$last)], "Doe")
      colnames(results)[which(colnames(results) == names$first)] <-
        "First.Name.1"
      colnames(results)[which(colnames(results) == names$last)] <-
        "Last.Name.1"
      colnames(results)[which(colnames(results) == names$partnerFirst)] <-
        "First.Name.2"
      colnames(results)[which(colnames(results) == names$partnerLast)] <-
        "Last.Name.2"
      colnames(results)[which(colnames(results) == names$price)] <-
        "Price.1"
      partnerResults <-
        results[, which(names(results) %in% c(
          "First.Name.2", "Last.Name.2", "Round", "Price.1"
          ))]
      colnames(partnerResults)[which(colnames(partnerResults) == "Price.1")] <-
        "Price.2"
      results <- merge(
        results,
        partnerResults,
        all = TRUE,
        by.x = c("First.Name.1", "Last.Name.1", "Round"),
        by.y = c("First.Name.2", "Last.Name.2", "Round")
      )
    }
    results$Price.M <-
      apply(results[, which(names(results) %in% c("Price.1", "Price.2"))], 1, min)
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
    class(out) <- c("econGame", class(out))
    out
  }
