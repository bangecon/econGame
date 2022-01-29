##' Creates a random list of partner groups
##'
##' @details \code{partnerList} uses a random number to divide a \code{studentList} groups of size N.
##'
##' @param studentList (required) is an object containing the list of student names.
##' @param n is the size of each group.
##'
##' @return \code{studentList} returns list of names and group numbers. .
##' @return \code{results} returns the original submissions (with equilibria and points per round added).
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @export

groupList <- function(studentList, n = 2) {
  studentList$group <- runif(nrow(studentList))
  studentList$group <- ceiling(rank(studentList$group) / n)
  studentList <- studentList[order(studentList$group),]
  out <- studentList
  out
}
