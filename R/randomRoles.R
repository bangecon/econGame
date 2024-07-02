##' Randomly assigns partner roles
##'
##' @details \code{randomRoles} uses a random number to assign partner roles (e.g. leader-follower, proposer-responder) to a list of student names a Google Sheet containing a student roster into
##'
##' @param sheet (required) is an object containing the list of students.
##' @param size is the size of each group (default is `2`).
##' @param seed is a random seed (default is `8675309`).
##' @param roles contains the labels for the roles (default is `c("Leader", "Follower")`)
##' @param names character list of the column names in `sheet`.
##' @param auth is a logical indicating whether to use an authentication token to access the Sheet containing the individual submissions.
##' @param email is an email address that matches the user account containing the Sheet with the individual submissions (if `auth == TRUE`).
##'
##' @return \code{studentList} returns list of names and group numbers.
##' @return \code{results} returns the original submissions (with equilibria and points per round added).
##' @return \code{grades} returns the aggregated points "won" by each student for the entire activity.
##'
##' @export

randomRoles <- function(sheet,
                          size = 2,
                          seed = 8675309,
                          roleLabs = c("Leader", "Follower"),
                          auth = FALSE,
                          names = NULL,
                          email = NULL,
                          ...) {
  if(auth == TRUE) {
    options(gargle_oauth_cache = ".secrets")
    googlesheets4::gs4_auth()
    googlesheets4::gs4_deauth()
    googlesheets4::gs4_auth(cache = ".secrets", email = email)
  }
  else {
    googlesheets4::gs4_deauth()
  }
  studentList <- googlesheets4::read_sheet(sheet)
  studentList <- as.data.frame(studentList)
  colnames(studentList) <- make.names(colnames(studentList))
  if (is.null(names)) {
    names = list(first = "First.Name",
                 last = "Last.Name",
                 round = "Round")
  }
  studentList[[c(names$first)]] <- tidyr::replace_na(studentList[[c(names$first)]], "John")
  studentList[[c(names$last)]] <- tidyr::replace_na(studentList[[c(names$last)]], "Doe")
  studentList[[c(names$first)]] <- stringr::str_to_title(studentList[[c(names$first)]])
  studentList[[c(names$last)]] <- stringr::str_to_title(studentList[[c(names$last)]])
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
                         labels = c(roleLabs[1], roleLabs[2])))
  out.long <- studentList[order(studentList$Round, studentList$Role),
                          c(names$round, names$first, names$last, "Role")] %>%
    as.data.frame()
  out.lead <- subset(out.long, Role == roleLabs[1])
  colnames(out.lead) <- c("Round", "First.Name.1", "Last.Name.1", "Role.1")
  out.follow <- subset(out.long, Role == roleLabs[2])
  colnames(out.follow) <- c("Round", "First.Name.2", "Last.Name.2", "Role.2")
  out.wide <- cbind(out.lead, out.follow[, -1]) |>
    as.data.frame()
  out <- list(
    long = out.long,
    wide = out.wide
  )
  out
}
