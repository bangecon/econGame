##' Randomly assigns partner roles
##'
##' @details \code{randomRoles} uses a random number to assign partner roles (e.g. leader-follower, proposer-responder) to a list of student names a Google Sheet containing a student roster into
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

randomLeaders <- function(sheet,
                          size = 2,
                          seed = 8675309,
                          round = 1,
                          auth = FALSE,
                          names = NULL) {
  googlesheets4::gs4_deauth()
  studentList <- googlesheets4::read_sheet(sheet)
  studentList <- as.data.frame(studentList[, -1])
  colnames(studentList) <- make.names(colnames(studentList))
  if (is.null(names)) {
    names = list(first = "First.Name",
                 last = "Last.Name")
  }
  set.seed(seed + round - 1)
  studentList$Rand <- runif(nrow(studentList))
  nStudents <- studentList %>%
    summarise(nStudents = n()) %>%
    as.data.frame
  studentList <- merge(studentList, nStudents)
  studentList <- studentList %>%
    mutate(Role = factor(rank(Rand) / nStudents <= 0.5,
                         labels = c("Leader", "Follower")))
  out.long <- studentList[order(studentList$Last.Name, studentList$First.Name, studentList$Role),
                          c(names$first, names$last, "Role")] %>%
    as.data.frame()
  out.lead <- subset(out.long, Role == "Leader")
  colnames(out.lead) <- c("Leader.First.Name", "Leader.Last.Name", "Role.1")
  out.follow <- subset(out.long, Role == "Follower")
  colnames(out.follow) <- c("Follower.First.Name", "Follower.Last.Name", "Role.2")
  out.wide <- cbind(out.lead, out.follow[, -1]) |>
    as.data.frame()
  out <- list(
    long = out.long,
    wide = out.wide
  )
  out
}
