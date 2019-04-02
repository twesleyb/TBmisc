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
#' Check git status of a repository.
#'
#' @param dir character specifying the directory of a git repository.
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

#-------------------------------------------------------------------------------
#' git add
#'
#' Add file contents to the git index.
#'
#' @param dir character specifying the directory of a git repository.
#'
#' @return None
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#' @references \url{https://git-scm.com/docs/git-add}
#' @keywords git add bash cmd
#'
#' @examples
#' gitadd()
#'
#' @export
#'
gitadd <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git add --all"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}

#-------------------------------------------------------------------------------
#' git commit
#'
#' Record changes to the git repsoitory. Note: the user should have previously
#'   called git2r::config(user.name,user.email) for git to recognize R.
#'
#' @param msg character describing changes made to the repository.
#' @param dir character specifying the directory of a git repository.
#'
#' @return None
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#' @references \url{https://git-scm.com/docs/git-commit}
#' @keywords git commit bash cmd
#'
#' @examples
#' gitcommit()
#'
#' @export
#'
gitcommit <- function(msg = "commit from Rstudio", dir = getwd()){
  cmd = sprintf("git commit -m\"%s\"",msg)
  system(cmd)
}

#-------------------------------------------------------------------------------
#' git push
#'
#' Update remote git repository with local changes.
#'
#' @param dir character specifying the directory of a git repository.
#'
#' @return None
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#' @references \url{https://git-scm.com/docs/git-push}
#' @keywords git push bash cmd
#'
#' @examples
#' gitpush()
#'
#' @export
#'
gitpush <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git push"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}
