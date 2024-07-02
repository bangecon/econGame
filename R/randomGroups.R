##' Creates a random list of partner groups
##'
##' @details \code{partnerList} uses a random number to divide a \code{studentList} groups of size N.
##'
##' @param sheet (required) is an object containing the list of student names with surnames and given names (see options for `last` and `first` below).
##' @param size is the size of each group (default is 2).
##' @param seed is a random seed (default is 8675309).
##' @param last is the name of the column in `sheet` containing the surnames of the participants (default is "Last.Name"). If the column names in `sheet` contain spaces, `randomGroups` automatically replaces spaces with periods using `make.names()` so that "Last Name" becomes "Last.Name".
##' @param first is the name of the column in `sheet` containing the given names of the participants (default is "First.Name").
##'
##' @return \code{studentList} returns list of names and group numbers.
##' @return \code{results} returns the original submissions (with equilibria and points per round added).
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @export

randomGroups <- function(sheet,
                         size = 2,
                         seed = 8675309,
                         names = NULL) {
  googlesheets4::gs4_deauth()
  studentList <- googlesheets4::read_sheet(sheet)
  studentList <- as.data.frame(studentList)
  colnames(studentList) <- make.names(colnames(studentList))
  if (is.null(names)) {
    names = list(first = "First.Name",
                 last = "Last.Name",
                 round = "Round")
  }
  set.seed(seed)
  studentList$group <- runif(nrow(studentList))
  colnames(studentList)[which(colnames(studentList) == names$round)] = "Round"
  studentList <- studentList %>%
    group_by(Round) %>%
    mutate(group = ceiling(rank(group) / size))
  studentList <-
    studentList[order(studentList$Round, studentList$group),]
  studentList$member <- with(studentList,
                             ave(Round, Round, group, FUN = seq_along))
  studentList <-
    studentList[order(studentList$Round, studentList$group, studentList$member),]
  out <- studentList
  out
}
