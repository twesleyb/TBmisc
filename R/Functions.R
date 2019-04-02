#-------------------------------------------------------------------------------
#' Creating a file
#'
#' Creates a file in the current working directory. Inspired by the linux command `touch`.
#'
#' @param file character specifying file that will be created.
#' @param dir character specifying the working direcotry.
#'
#' @return None
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#' @references \url{https://gist.github.com/JoshCheek/1224782}
#' @keywords touch create file directory linux
#'
#' @examples
#' touch("test.txt")
#'
#' @export
#'
touch <- function(file = "test.txt", dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = paste("echo >>",file)
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}

#-------------------------------------------------------------------------------
#' git status
#'
#' Check git status of current directory.
#'
#' @param file character specifying the directory of a git repository.
#'
#' @return None
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#' @references \url{https://git-scm.com/docs/git-status}
#' @keywords git status bash cmd
#'
#' @examples
#' gitstatus()
#'
#' @export
#'
gitstatus <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git status"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}
