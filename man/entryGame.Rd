% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entryGame.R
\name{entryGame}
\alias{entryGame}
\title{Tabulate entry and exit game results.}
\usage{
entryGame(sheet, names = NULL, auth = FALSE, ...)
}
\arguments{
\item{sheet}{(required) is a character string corresponding to the Google Sheets location (url or ID key) containing the individual submissions.}

\item{names}{character list of the column names in \code{sheet}.}

\item{auth}{is a logical indicating whether to use an authentication token to access the Sheet containing the individual submissions.}

\item{email}{is an email address that matches the user account containing the Sheet with the individual submissions.}
}
\value{
\code{type} returns the type of activity (entryGame).

\code{results} returns the original submissions (with market prices and points per round added).

\code{rounds} returns the number of rounds in "results"

\code{equilibria} returns a list containing the equilibria for each round.

\code{grades} returns the aggregated points "won" by each student for the entire activity.
}
\description{
Tabulates and assigns points for the results of a simple in-class entry and exit game using random values.
}
\details{
\code{equilibriumGame} tabulates the results of a simple entry and exit game based on a simplified version of Garratt (2000) with two crops (corn and soybeans). The instructor informs the students that they will choose to plant corn, soybeans, or nothing. Producing corn incurs a cost of four points, while producing soybeans incurs a cost of 10 points. Selling a unit of corn brings revenue equal to \eqn{P_c = (N/2) + 6 - Q_c}, where N equals the number of students participating and Q_c equals the number of students choosing to produce corn. Selling a unit of soybeans brings revenue equal to \eqn{P_s = (N/2) + 10 - Q_s}. These parameters allow for there to be a "normal profit" of one point per student in each market, and lessens the chances that students might win negative points. Students choosing to produce nothing sell their labor in the labor market and break even (less the "normal profit").
}
\references{
Garratt (2000). A Free Entry and Exit Experiment. \emph{Journal of Economic Education,} 31(3), pp.237-243.
}
