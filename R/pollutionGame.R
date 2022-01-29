##' Tabulate pollution game results.
##'
##' Tabulates and assigns points for the results of a four in-class pollution policy simulations using random cleanup-cost parameters. The four policies the results compare include: (1) no regulation; (2) command and control regulation, in which firms may only permit a fixed amount; (3) a pollution tax, in which firms must pay a tax equal to their external costs; and (4) cap and trade, in which firms receive free initial pollution permits and may trade in order to buy or sell additional permits.
##'
##' @details \code{pollutionGame} tabulates the results of a pollution game based on Nugent (1997) and some modifications to it. At the beginning of the game, each "firm" (student) "draws" two independent random cleanup costs from a discrete uniform distribution through a link to a Google Sheet. The lower cleanup cost represents the cost of cleaning up the first unit of pollution and the higher number represents the cost of cleaning up the second unit of pollution (if applicable), so that cleanup follows the Law of Increasing Costs. In the default example, the cleanup costs are distributed on a uniform discrete distribution from 1 to 4. During each round of the game, firms choose how much to produce (0, 1, or 2 units) and how much pollution from their production to clean up (0, 1, or 2).
##' In the first round (which may be designated as practice to not count for points), there is no regulation. Students receive the price of their output as "extra credit" points (default is 3), internal production costs are set to zero (for simplicity), and impose a "social cost" for which they do not pay (default is 2). In this round, profit-maximizing firm would produce 2 units of output, pay no cost for either production or the externalities they impose, and earn a profit of 6 points.
##' In the second round, firms face command and control regulation that limits them to 1 unit of pollution. This means that students may produce the first unit of output at zero cost, but producing a second unit of output automatically requires them to clean up the pollution using and pay a cost equal to the lower cleanup cost parameter.
##' In the third round, firms face a pollution tax equal to any external costs they impose on society (by default equal to 2). In the default settings, the price (3) is greater than the externality (2) and any profit-maximizing firm chooses to produce both units of output. Firms must still choose how much to clean up: If a firm's \emph{lower} cleanup cost is less than the tax, then that firm will maximize profits by cleaning up the first unit of pollution; if their \emph{higher} cleanup cost is also less than the tax, then it will clean up the second unit as well.
##' In the fourth and final round, firms face a cap and trade mechanism that allocates one "free" permit-to-pollute to each firm (much like the command and control round), except this time firms may trade their pollution rights in a market. Firms cannot immediately choose their output in this round (although most would end up choosing 2 units). Instead, students must decide how much they would \emph{bid} to buy an additional permit and how much they would \emph{ask} to sell their "free" permit. The \code{pollutionGame} code calculates the optimal output and cleanup decisions, and computes the profits net of revenues or costs from permit trades.
##'
##' @param sheet (required) is a sheet ID corresponding to the Google Sheets location containing the individual submissions.
##' @param price is the price of the output each "firm" produces (default is 3).
##' @param externality is the external cost of pollution for each unit of output firms produce (default is 2).
##' @param practice is a logical indicating whether round 1 is practice (default is \code{TRUE}).
##'
##' @return \code{type} returns the type of activity (pollutionGame).
##' @return \code{results} returns the original submissions (with market price and points added).
##' @return \code{schedules} returns a list containing the supply and demand schedules for each round.
##' @return \code{equilibria} returns a list containing the equilibria for each round.
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @references Nugent, R. A. (1997). Teaching Tools: A Pollution Rights Trading Game \emph{Economic Inquiry,} 35(3), 679-685.
##'
##' @export

pollutionGame <-
  function(sheet,
           price = 3,
           externality = 2,
           practice = TRUE,
           ...) {
    Rcpp::sourceCpp(
      code = '
      #include <Rcpp.h>
      #include <map>
      // [[Rcpp::export]]
      Rcpp::List find_optimum(Rcpp::NumericVector price_supply,
        Rcpp::NumericVector quant_supply,
        Rcpp::NumericVector price_demand,
        Rcpp::NumericVector quant_demand) {
          std::map<double, double> supply;
          std::map<double, double> demand;
          // fill the maps
          for (int i = 0; i < price_supply.size(); ++i) {
            supply[price_supply[i]] += quant_supply[i];
          }
          for (int i = 0; i < price_demand.size(); ++i) {
            demand[price_demand[i]] += quant_demand[i];
          }
          if (supply.empty() || demand.empty())
            return Rcpp::List::create(Rcpp::Named("price") = 0,
              Rcpp::Named("quantity") = 0);
          auto sIt = supply.begin(), nextS = std::next(sIt, 1);
          const auto endS = supply.end();
          auto dIt = demand.rbegin(), nextD = std::next(dIt, 1);
          const auto endD = demand.rend();\
          // quantity and prices at either side
          double pS = sIt->first, pD = dIt->first;
          double qS = 0, qD = 0;
          // next prices
          double nextPS = nextS->first, nextPD = nextD->first;
          if (pD < pS)
            return Rcpp::List::create(Rcpp::Named("price") = 0, Rcpp::Named("quantity") = 0);
          // add the best price from each side!
          qS += sIt->second;
          qD += dIt->second;
          while (pS < pD) {
            if (nextS == endS && nextD == endD) {
              pD = qD < qS ? pS : pD;
              break;
            }
            while (qS <= qD && sIt != endS && nextS->first <= pD) {
              ++sIt;
              ++nextS;
              pS = sIt->first;
              qS += sIt->second;
            }
            if (sIt == endS) break;
            if (nextD->first < pS) {
              pD = qD < qS ? pS : pD;
              break;
            }
            while (qD < qS && dIt != endD && nextD->first >= pS) {
              ++dIt;
              ++nextD;
              pD = dIt->first;
              qD += dIt->second;
            }
            if (dIt == endD) break;
          }
          double price = pD;
          double vol = qS < qD ? qS : qD;
          return Rcpp::List::create(Rcpp::Named("price") = price,
            Rcpp::Named("quantity") = vol);
    }
                    '
    )
    # Set up the Google Sheets, read responses, and initialize output objects.
    results <- read_sheet(sheet)
    colnames(results) <- make.names(colnames(results))
    results <-
      replace_na(results, list(First.Name = "John", Last.Name = "Doe"))
    results$First.Name <- str_to_title(results$First.Name)
    results$Last.Name <- str_to_title(results$Last.Name)
    responses <- nrow(results)
    results1 <-
      results[, which(names(results) %in% c("First.Name", "Last.Name", "Output1"))]
    results1$Revenue <- results$Output1 * price
    results1$FirmCost <-
      I(results$Cleanup1 >= 1) * results$Cost1 + I(results$Cleanup1 >= 2) * results$Cost2
    results1$SocialCost <-
      externality * (results$Output1 - results$Cleanup1)
    results1$TaxRevenue <- 0
    results1$Profit <- results1$Revenue - results1$FirmCost
    results1$NetBenefit <-
      results1$Profit + results1$TaxRevenue - results1$SocialCost
    summary1 <-
      c(
        sum(results1$Output1),
        sum(results1$FirmCost),
        sum(results1$SocialCost),
        sum(results1$TaxRevenue),
        sum(results1$Profit),
        sum(results1$NetBenefit)
      )
    grades1 <-
      results1[, which(names(results1) %in% c("First.Name", "Last.Name", "Profit"))]
    if (practice)
      grades1$Profit <- 0
    colnames(results1) <-
      c(
        "First Name",
        "Last Name",
        "Output",
        "Firm Revenue",
        "Firm Cost",
        "Social Cost",
        "Tax Revenue",
        "Profit",
        "Net Benefit"
      )
    names(summary1) <-
      c(
        "Output",
        "Internal Cost",
        "External Cost",
        "Tax Revenue",
        "Total Profit",
        "Net Social Benefit"
      )
    colnames(grades1) <- c("First Name", "Last Name", "Score")
    results2 <-
      results[, which(names(results) %in% c("First.Name", "Last.Name", "Output2"))]
    results2$Revenue <- results$Output2 * price
    results2$FirmCost <- I(results$Output2 >= 2) * results$Cost1
    results2$SocialCost <- externality
    results2$TaxRevenue <- 0
    results2$Profit <- results2$Revenue - results2$FirmCost
    results2$NetBenefit <-
      results2$Profit + results2$TaxRevenue - results2$SocialCost
    summary2 <-
      c(
        sum(results2$Output2),
        sum(results2$FirmCost),
        sum(results2$SocialCost),
        sum(results2$TaxRevenue),
        sum(results2$Profit),
        sum(results2$NetBenefit)
      )
    grades2 <-
      results2[, which(names(results2) %in% c("First.Name", "Last.Name", "Profit"))]
    colnames(results2) <-
      c(
        "First Name",
        "Last Name",
        "Output",
        "Firm Revenue",
        "Firm Cost",
        "Social Cost",
        "Tax Revenue",
        "Profit",
        "Net Benefit"
      )
    names(summary2) <-
      c(
        "Output",
        "Internal Cost",
        "External Cost",
        "Tax Revenue",
        "Total Profit",
        "Net Social Benefit"
      )
    colnames(grades2) <- c("First Name", "Last Name", "Score")
    results3 <-
      results[, which(names(results) %in% c("First.Name", "Last.Name", "Output3"))]
    results3$Revenue <- results$Output3 * price
    results3$FirmCost <-
      4 + I(results$Cleanup3 >= 1) * (results$Cost1 - 2) + I(results$Cleanup3 >= 2) * (results$Cost2 - 2)
    results3$SocialCost <-
      externality * (results$Output3 - results$Cleanup3)
    results3$TaxRevenue <- 2 * (results$Output3 - results$Cleanup3)
    results3$Profit <- results3$Revenue - results3$FirmCost
    results3$NetBenefit <-
      results3$Profit + results3$TaxRevenue - results3$SocialCost
    summary3 <-
      c(
        sum(results3$Output3),
        sum(results3$FirmCost),
        sum(results3$SocialCost),
        sum(results3$TaxRevenue),
        sum(results3$Profit),
        sum(results3$NetBenefit)
      )
    grades3 <-
      results3[, which(names(results3) %in% c("First.Name", "Last.Name", "Profit"))]
    colnames(results3) <-
      c(
        "First Name",
        "Last Name",
        "Output",
        "Firm Revenue",
        "Firm Cost",
        "Social Cost",
        "Tax Revenue",
        "Profit",
        "Net Benefit"
      )
    names(summary3) <-
      c(
        "Output",
        "Internal Cost",
        "External Cost",
        "Tax Revenue",
        "Total Profit",
        "Net Social Benefit"
      )
    colnames(grades3) <- c("First Name", "Last Name", "Score")
    results4 <-
      results[, which(names(results) %in% c("First.Name", "Last.Name", "Bid", "Ask"))]
    results4$permit <- 1
    schedule4 <-
      as.data.frame(matrix(
        nrow = max(results$Cost2),
        ncol = 3,
        dimnames = list(NULL, c('Price', 'Demand', 'Supply'))
      ))
    schedule4$Price <- c(1:nrow(schedule4))
    for (i in 1:nrow(schedule4)) {
      schedule4$Demand[i] <- nrow(subset(results4, Bid >= i))
      schedule4$Supply[i] <- nrow(subset(results4, Ask <= i))
    }
    equilibrium <- as.data.frame(find_optimum(results4$Ask,
                                              results4$permit,
                                              results4$Bid,
                                              results4$permit))
    results4$permitPrice <- equilibrium$price
    results4$trade <- ifelse(
      results4$Bid >= results4$permitPrice &
        results4$Ask <= results4$permitPrice, "Both", ifelse(
          results4$Bid >= results4$permitPrice, "Buy", ifelse(
            results4$Ask <= results4$permitPrice, "Sell", "None")
      )
    )
    results4$Output4 <-
      ifelse(results$Cost1 <= price |
               results4$Bid >= results4$permitPrice,
             2,
             1)
    results4$Revenue <-
      results4$Output4 * price + I(results4$trade == "Sell") * results4$permitPrice
    results4$FirmCost <-
      ifelse(
        results4$trade == "Buy",
        results4$permitPrice,
        ifelse(
          results4$trade == "Sell",
          results$Cost1 + results$Cost2,
          results$Cost1
        )
      )
    results4$SocialCost <-
      externality * (1 + I(results4$trade == "Buy") - I(results4$trade == "Sell"))
    results4$TaxRevenue <- 0
    results4$Profit <- results4$Revenue - results4$FirmCost
    results4$NetBenefit <-
      results4$Profit + results4$TaxRevenue - results4$SocialCost
    summary4 <-
      c(
        sum(results4$Output4),
        sum(results4$FirmCost),
        sum(results4$SocialCost) -
          externality * (schedule4[schedule4$Price == equilibrium$price, "Demand"] -
                           schedule4[schedule4$Price == equilibrium$price, "Supply"]),
        sum(results4$TaxRevenue),
        sum(results4$Profit),
        sum(results4$NetBenefit) +
          externality * (schedule4[schedule4$Price == equilibrium$price, "Demand"] -
                           schedule4[schedule4$Price == equilibrium$price, "Supply"])
      )
    grades4 <-
      results4[, which(names(results4) %in% c("First.Name", "Last.Name", "Profit"))]
    colnames(results4) <-
      c(
        "First Name",
        "Last Name",
        "Bid",
        "Ask",
        "Permit",
        "Permit Price",
        "Trade",
        "Output",
        "Firm Revenue",
        "Firm Cost",
        "Social Cost",
        "Tax Revenue",
        "Profit",
        "Net Benefit"
      )
    names(summary4) <-
      c(
        "Output",
        "Internal Cost",
        "External Cost",
        "Tax Revenue",
        "Total Profit",
        "Net Social Benefit"
      )
    colnames(grades4) <- c("First Name", "Last Name", "Score")
    results <- list(results = results,
                    results1 = results1,
                    results2 = results2,
                    results3 = results3,
                    results4 = results4)
    # results$results <-
    #   results$results[order(results$results$Last.Name, results$results$First.Name), ]
    overallSummary <- list(summary = cbind(summary1, summary2, summary3, summary4),
                           summary1 = summary1,
                           summary2 = summary2,
                           summary3 = summary3,
                           summary4 = summary4)
    colnames(overallSummary$summary) <-
      c("No Regulation",
        "Command & Control",
        "Pollution Tax",
        "Cap & Trade")
    grades <-
      list(grades = cbind(grades1, grades2[, 3], grades3[, 3], grades4[, 3]),
           grades1 = grades1,
           grades2 = grades2,
           grades3 = grades3,
           grades4 = grades4)
    grades$grades$TotalPoints <- rowSums(grades$grades[3:6])
    # grades$grades <-
    #   grades$grades[order(grades$grades$Last.Name, grades$grades$First.Name), ]
    colnames(grades$grades)[3:7] <-
      c("No Regulation",
        "Command & Control",
        "Pollution Tax",
        "Cap & Trade",
        "Total Points")
    rm(results1, results2, results3, results4,
       summary1, summary2, summary3, summary4,
       grades1, grades2, grades3, grades4)
    out <- list(
      type = "pollutionGame",
      results = results,
      summary = overallSummary,
      marketSchedule = schedule4,
      equilibrium = equilibrium,
      grades = grades
    )
    class(out) <- c('econGame', class(out))
    out
  }
