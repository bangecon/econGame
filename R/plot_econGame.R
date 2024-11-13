##' Plot econGame results
##'
##' Plots objects of class \code{econGame} created by tabulating the results of one of the games in the \code{econGame} package
##' @details \code{plot.econGame} currently supports the following games:
##' \code{equilibriumGame} for simple market equilibrium.
##' \code{entryGame} for a simple entry and exit game.
##' \code{staghuntGame} for a bar plot of the outcomes of the stag hunt game.
##' \code{cournotGame} for a bar plot of the outcomes of the Cournot duopoly game.
##' \code{stackelbergGame} for a bar plot of the outcomes of the Stackelberg duopoly game.
##' \code{bertrandGame} for a histogram of the price strategies and equlibrium prices for the Bertrand duopoly game.
##' \code{multipdGame} for a histogram of the strategies of a multi-player prisoner's dilemma game.
##'
##' @param econGame (required) is a list created by an \code{econGame} function.
##' @param round is the round of the game that you want to plot (defaults to NULL for all rounds).
##' @param nrow is the number of rows to use for \code{ggarrange} (defaults to 1).
##' @param ncol is the number of columns to use for \code{ggarrange} (defaults to the number of rounds).
##' @param binwidth is the number of bins to use for histograms.
##' @param ... other optional graphical parameters.
##'
##' @return \code{plot.econGame} returns a \code{ggarrange} or \code{ggplot} object that can be plotted and exported.
##'
##' @export
##'

plot.econGame <- function(econGame,
                          round = NULL,
                          nrow = 1,
                          ncol = NULL,
                          binwidth = NULL,
                          ...) {
  if (is.null(ncol) & is.null(round))
    ncol <- econGame$rounds
  out <- list(NULL)
  if (econGame$type == 'equilibriumGame') {
    if (is.null(round)) {
      for (i in 1:econGame$rounds) {
        out[[i]] <- ggplot() +
          geom_step(
            aes(Demand, Price),
            econGame$schedules[[i]][-which(duplicated(econGame$schedules[[i]]$Demand, fromLast = TRUE)), ],
            direction = 'vh',
            color = 'blue',
            na.rm = TRUE
          ) +
          geom_step(
            aes(Supply, Price),
            econGame$schedules[[i]],
            direction = 'vh',
            color = 'darkorange',
            na.rm = TRUE
          ) +
          geom_point(aes(quantity, price), econGame$equilibria[[i]]) +
          lims(x = c(0, max(econGame$schedules[[i]]$Demand)),
               y = c(0, max(econGame$schedules[[i]]$Price)))
      }
      out <- ggarrange(plotlist = out,
                       nrow = nrow,
                       ncol = ncol,
                       ...)
    } else {
      out <- ggplot() +
        geom_step(
          aes(Demand, Price),
          econGame$schedules[[round]][-which(duplicated(econGame$schedules[[round]]$Demand, fromLast = TRUE)), ],
          direction = 'vh',
          color = 'blue',
          na.rm = TRUE
        ) +
        geom_step(
          aes(Supply, Price),
          econGame$schedules[[round]],
          direction = 'vh',
          color = 'darkorange',
          na.rm = TRUE
        ) +
        geom_point(aes(quantity, price), econGame$equilibria[[round]]) +
        lims(x = c(0, max(econGame$schedules[[i]]$Demand)),
             y = c(0, max(econGame$schedules[[i]]$Price)))
    }
  }
  if (econGame$type == 'ultimatumGame') {
    out <- ggplot() +
      geom_histogram(aes(Offer, fill = Response, color = Response),
                     data = econGame$results,
                     position = 'stack',
                     na.rm = TRUE,
                     binwidth = 0.5,
                     alpha = 0.5) +
      scale_fill_manual(values = c('blue', 'darkorange')) +
      scale_colour_manual(values = c('darkorange', 'blue')) +
      lims(x = c(-1, max(econGame$results$Offer) + 1))
  }
  if (econGame$type == 'publicgoodGame') {
    out <- ggplot() +
      geom_histogram(aes(Contribution),
                     econGame$results,
                     na.rm = TRUE,
                     binwidth = 1,
                     color = 'blue',
                     fill = 'darkorange',
                     alpha = 0.5) +
      lims(x = c(-1, max(econGame$results$Contribution) + 1))
  }
  if (econGame$type == 'anchoringGame') {
    out <- ggplot(aes(x = as.factor(Value), y = Percent),
                  data = econGame$results) +
      geom_boxplot(na.rm = TRUE,
                   notch = TRUE,
                   color = 'blue',
                   fill = 'darkorange') +
      geom_dotplot(
        binaxis = 'y',
        stackdir = 'center',
        dotsize = 0.5,
        binwidth = 1
      ) +
      stat_summary(
        fun.y = mean,
        geom = "point",
        size = 2,
        color = 'blue'
      ) +
      stat_summary(
        fun.data = mean_se,
        geom = "errorbar",
        color = 'blue',
        width = 0.2
      )
  }
  if (econGame$type == 'entryGame') {
    Corn <- list(NULL)
    Soybeans <- list(NULL)
    out <- list(NULL)
    if (is.null(round)) {
      for (i in 1:econGame$rounds) {
        Corn[[i]] <- ggplot() +
          xlim(0, (econGame$equilibria$N[[i]] / 2) + 10) + xlab("Corn") +
          ylim(0, (econGame$equilibria$N[[i]] / 2) + 10) + ylab("Price") +
          geom_function(
            fun = function(x)
              (econGame$equilibria$N[[i]] / 2) +  6 - x,
            color = 'blue'
          ) +
          geom_vline(xintercept = econGame$equilibria$Q_c[[i]],
                     color = 'darkorange') +
          geom_hline(yintercept = 4, color = 'darkorange') +
          geom_hline(yintercept = econGame$equilibria$P_c[[i]]) +
          geom_point(aes(econGame$equilibria$Q_c[[i]], econGame$equilibria$P_c[[i]]))
        Soybeans[[i]] <- ggplot() +
          xlim(0, (econGame$equilibria$N[[i]] / 2) + 10) + xlab("Soybeans") +
          ylim(0, (econGame$equilibria$N[[i]] / 2) + 10) + ylab("Price") +
          geom_function(
            fun = function(x)
              (econGame$equilibria$N[[i]] / 2) + 10 - x,
            color = 'blue'
          ) +
          geom_vline(xintercept = econGame$equilibria$Q_s[[i]],
                     color = 'darkorange') +
          geom_hline(yintercept = 10, color = 'darkorange') +
          geom_hline(yintercept = econGame$equilibria$P_s[[i]]) +
          geom_point(aes(econGame$equilibria$Q_s[[i]], econGame$equilibria$P_s[[i]]))
      }
      plots <- c(Corn, Soybeans)
    } else {
      Corn <- ggplot() +
        xlim(0, (econGame$equilibria$N[[round]] / 2) + 10) + xlab("Corn") +
        ylim(0, (econGame$equilibria$N[[round]] / 2) + 10) + ylab("Price") +
        geom_function(
          fun = function(x)
            (econGame$equilibria$N[[round]] / 2) +  6 - x,
          color = 'blue'
        ) +
        geom_vline(xintercept = econGame$equilibria$Q_c[[round]],
                   color = 'darkorange') +
        geom_hline(yintercept = 4, color = 'darkorange') +
        geom_hline(yintercept = econGame$equilibria$P_c[[round]]) +
        geom_point(aes(econGame$equilibria$Q_c[[round]], econGame$equilibria$P_c[[round]]))
      Soybeans <- ggplot() +
        xlim(0, (econGame$equilibria$N[[round]] / 2) + 10) + xlab("Soybeans") +
        ylim(0, (econGame$equilibria$N[[round]] / 2) + 10) + ylab("Price") +
        geom_function(
          fun = function(x)
            (econGame$equilibria$N[[round]] / 2) + 10 - x,
          color = 'blue'
        ) +
        geom_vline(xintercept = econGame$equilibria$Q_s[[round]],
                   color = 'darkorange') +
        geom_hline(yintercept = 10, color = 'darkorange') +
        geom_hline(yintercept = econGame$equilibria$P_s[[round]]) +
        geom_point(aes(econGame$equilibria$Q_s[[round]], econGame$equilibria$P_s[[round]]))
      plots <- list(Corn, Soybeans)
    }
    out <- ggarrange(plotlist = plots,
                     nrow = 2,
                     ncol = ncol,
                     ...)
  }
  if (econGame$type == 'cournotGame') {
    out <- ggplot() +
      geom_bar(aes(x = Outcome, y = after_stat(prop), group = 1),
               econGame$results, stat = 'count', color = 'blue', fill = 'darkorange')

  }
  if (econGame$type == 'stackelbergGame') {
    out <- ggplot() +
      geom_bar(aes(x = outcomes, y = after_stat(prop), group = 1),
               econGame$outcomes, stat = 'count', color = 'blue', fill = 'darkorange')

  }
  if (econGame$type == 'bertrandGame') {
    out <- ggplot(subset(econGame$results, Round == round)) +
      geom_histogram(aes(x = Price.1),
                     fill = 'darkorange',
                     alpha = 0.3,
                     binwidth = binwidth, ...) +
      geom_histogram(aes(x = Price.M),
                     fill = 'blue',
                     alpha = 0.3,
                     binwidth = binwidth, ...)
  }
  if (econGame$type %in% c('cropchoiceGame', 'pestcontrolGame', 'staghuntGame', 'codeGame', 'cattleGame')) {
    out <- ggplot() +
      geom_bar(aes(x = Outcome, y = after_stat(prop), group = 1),
               econGame$resultsWide, stat = 'count', color = 'blue', fill = 'darkorange')
  }
  if (econGame$type == 'pollutionGame') {
    out <- ggplot() +
      geom_step(
        aes(Demand, Price),
        econGame$marketSchedule[-which(duplicated(econGame$marketSchedule$Demand, fromLast = TRUE)), ],
        direction = 'vh',
        color = 'blue',
        na.rm = TRUE
      ) +
      geom_step(
        aes(Supply, Price),
        econGame$marketSchedule,
        direction = 'vh',
        color = 'darkorange',
        na.rm = TRUE
      ) +
      geom_point(aes(quantity, price), econGame$equilibrium) +
      xlim(0, max(econGame$marketSchedule$Demand)) +
      ylim(0, nrow(econGame$marketSchedule))
  }
  return(out)
}
