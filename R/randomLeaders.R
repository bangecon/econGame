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

randomLeaders <- function(sheet,
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
  colnames(studentList)[which(colnames(studentList) == names$round)] = "Round"
  studentList$Rand <- runif(nrow(studentList))
  Round_n <- studentList %>%
    group_by(Round) %>%
    summarise(Round_n = n()) %>%
    as.data.frame
  studentList <- merge(studentList, Round_n, by = "Round")
  studentList <- studentList %>%
    group_by(Round) %>%
    mutate(Role = factor(rank(Rand) / Round_n <= 0.5,
                         labels = c("Leader", "Follower")))
  out.long <- studentList[order(studentList$Round, studentList$Role),
                          c(names$round, names$first, names$last, "Role")] %>%
    as.data.frame()
  out.lead <- subset(out.long, Role == "Leader")
  colnames(out.lead) <- c("Round", "First.Name.1", "Last.Name.1", "Role.1")
  out.follow <- subset(out.long, Role == "Follower")
  colnames(out.follow) <- c("Round", "First.Name.2", "Last.Name.2", "Role.2")
  out.wide <- cbind(out.lead, out.follow[, -1]) |>
    as.data.frame()
  out <- list(
    long = out.long,
    wide = out.wide
  )
  out
}