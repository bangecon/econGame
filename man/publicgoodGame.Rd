% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/publicgoodGame.R
\name{publicgoodGame}
\alias{publicgoodGame}
\title{Tabulate public good game results.}
\usage{
publicgoodGame(sheet, endowment = 5, return = 2, ...)
}
\arguments{
\item{sheet}{(required) is a character string url corresponding to the Google Sheets location containing the individual submissions.}

\item{endowment}{is the number of gifted points students start the game with (default is 5).}

\item{return}{is the scaling factor for points contributed to the public good (default is 2).}
}
\value{
\code{type} returns the type of activity (publicgoodGame).

\code{results} returns the original submissions.

\code{grades} returns the aggregated, stacked points "won" by each student for the entire activity.
}
\description{
Tabulates and assigns points for the results of a simple in-class public good game.
}
\details{
\code{publicgoodGame} tabulates the results of a simple public good game based on Holt and Laury (1997). Each student starts with an endowment of 5 points that they can either keep or anonymously and voluntarily contribute to a public good. For each point a student keeps, they receive only that point. For each point contributed to the public good, \emph{everyone} receives R/N points, where N is the number of participants and R > 1 is the return the group earns on public good contributions. In other words, students have the opportunity to boost their points if everyone contributes all of them. But, if only a few participants contribute, their net return could diminish.
}
\references{

}
