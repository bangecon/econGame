##' Creates a random list of partner groups
##'
##' @details \code{partnerList} uses a random number to divide a \code{studentList} groups of size N.
##'
##' @param sheet (required) is an object containing the list of student names.
##' @param size is the size of each group.
##'
##' @return \code{studentList} returns list of names and group numbers. .
##' @return \code{results} returns the original submissions (with equilibria and points per round added).
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @export

randomGroups <- function(sheet, size = 2, seed = 8675309) {
  studentList <- googlesheets4::read_sheet(sheet)
  studentList <- as.data.frame(studentList)
  set.seed(seed)
  studentList$group <- runif(nrow(studentList))
  studentList$group <- ceiling(rank(studentList$group) / size)
  studentList <- studentList[order(studentList$group),]
  studentList$member <-
    ave(studentList$`Last Name`, studentList$group, FUN = seq_along)
  out <- studentList
  out
}
