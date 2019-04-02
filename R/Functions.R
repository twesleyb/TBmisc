#' Creating a file
#'
#' Creates a file in the current working directory
#'
#' @param file character specifying file that will be created. 
#' @param dir character expansion for the text
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