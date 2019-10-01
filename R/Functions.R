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
#' git config
#'
#' Configure git..
#'
#' @param user_name the user's GitHub username.
#' @param user_email the user's GitHub email.
#'
#' @return None
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#' @references \url{https://git-scm.com/docs/git-config}
#' @keywords git config bash cmd
#'
#' @examples
#' gitconfig("twesleyb","tyler.w.bradshaw@duke.edu")
#'
#' @export
#'

gitconfig <- function(user_name, user_email){
  cmd1 = paste0("git config --global user.name ","'",user_name,"'")
  cmd2 = paste0("git config --global user.email ","'",user_email,"'")
  shell(cmd)}

#-------------------------------------------------------------------------------
#' git pull
#'
#' Incorporate changes from remote git repository to current, local branch.
#'
#' @param dir character specifying the directory of a git repository.
#'
#' @return None
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#' @references \url{https://git-scm.com/docs/git-pull}
#' @keywords git status bash cmd
#'
#' @examples
#' gitpull()
#'
#' @export
#'
gitpull <- function(dir = getwd()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git pull"
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

#-------------------------------------------------------------------------------
#' write.pajek
#'
#' Write network adjacency network to file in Pajek (*.net) format.
#'
#' @param adjm (matrix) symmetric adjacency matrix representing the network graph.
#' @param file (string) name of output file (e.g. 'network.net')
#'
#' @return None
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#' @references \url{https://gephi.org/users/supported-graph-formats/pajek-net-format/}
#' @keywords network graph pajek write
#'
#' @examples
#' write.pajek(adjm, "network.net")
#'
#' @export

write.pajek <- function(adjm, file = "network.net", col.names = FALSE, ...) {
  require(data.table, quietly = TRUE)
  # Write network adjacency matrix to .net file in Pajek format.
  # Uses data.table::fwrite for faster performance.
  # Rename columns as numbers.
  # Node names not currently supported. Causes error loading Pajek file.
  if (col.names == FALSE) {
    colnames(adjm) <- rownames(adjm) <- c(1:ncol(adjm))
  }
  edge_list <- as.data.table(na.omit(melt(adjm)))
  colnames(edge_list) <- c("protA","protB","weight")
  edge_list <- subset(edge_list, weight > 0 )
  node_names <- colnames(adjm)
  v <- as.data.table(paste(seq(1,ncol(adjm)), " \"", node_names, "\"", sep = ""))
  write.table(paste("*Vertices", dim(adjm)[1]), file,
              quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)
  fwrite(v, file, quote = FALSE, sep = " ", row.names = FALSE,
         col.names = FALSE, append = TRUE)
  write.table("*Edges", file, quote = FALSE, sep = " ",
              row.names = FALSE, col.names = FALSE, append = TRUE)
  fwrite(edge_list, file, sep = " ", col.names = FALSE, append = TRUE)
}

#-------------------------------------------------------------------------------
#' silently
#'
#' suppress any unwanted output from a function with sink().
#'
#' @param func (function) symmetric adjacency matrix representing the network graph.
#' @param ... (string) additional arguments passed to func().
#'
#' @return None
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#' @references \url{}
#' @keywords supress output silent quiet
#'
#' @examples
#' silently(wgcna::bicor, exprDat)
#'
#' @export
## Define a function that can suppress unwanted messages from a function.
silently <- function(func, ...) {
  sink(tempfile())
  out <- func(...)
  sink(NULL)
  return(out)
}

#-------------------------------------------------------------------------------
#' ggplotScaleFreeFit
#'
#' Generate plots examining scale free fit of WGCNA network.
#'
#' @param sft (list) output of WGCNA::pickSoftThreshold()
#'
#' @return None
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#' @references \url{}
#' @keywords ggplot scale free fit pickSoftThreshold
#'
#' @examples
#' ggplotScaleFreeFit(sft)
#'
#' @export

ggplotScaleFreeFit <- function(sft) {
  require(ggplot2, quietly = TRUE)
  # Gather the data, calculate scale free fit.
  data <- sft$fitIndices
  data$fit <- -sign(data$slope) * data$SFT.R.sq
  # Generate Scale free topology plot.
  plot1 <- ggplot(data, aes(x = Power, y = fit)) +
    geom_text(aes(label = Power), color = "red") +
    ggtitle("Scale independence") +
    xlab(expression(Soft ~ Threshold ~ Power ~ (beta))) +
    ylab(expression(Scale ~ Free ~ Topology ~ (R^2))) +
    geom_hline(yintercept = 0.9, linetype = "dashed", color = "red", size = 0.6) +
    # geom_hline(yintercept = 0.8, linetype = "dashed", color = "gray", size = 0.6) +
    theme(
      plot.title = element_text(hjust = 0.5, color = "black", size = 11, face = "bold"),
      axis.title.x = element_text(color = "black", size = 11, face = "bold"),
      axis.title.y = element_text(color = "black", size = 11, face = "bold")
    )
  # Generate mean connectivity plot.
  plot2 <- ggplot(data, aes(x = Power, y = mean.k.)) +
    geom_text(aes(label = Power), color = "red") +
    ggtitle("Mean Connectivity") +
    xlab(expression(Soft ~ Threshold ~ Power ~ (beta))) +
    ylab(expression(Mean ~ Connectivity ~ (k))) +
    theme(
      plot.title = element_text(hjust = 0.5, color = "black", size = 11, face = "bold"),
      axis.title.x = element_text(color = "black", size = 11, face = "bold"),
      axis.title.y = element_text(color = "black", size = 11, face = "bold")
    )
  plot3 <- plot_grid(plot1, plot2, labels = "auto")
  data_return <- list(plot1, plot2, plot3)
  names(data_return) <- c("ScaleFreeFit", "MeanConnectivity", "Grid")
  return(data_return)
}

#------------------------------------------------------------------------------

#' grobsize
#'
#' get the actual height and width of a grob
#'
#' @param x (grob) a grob object
#'
#' @return None
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#' @references \url{https://stackoverflow.com/questions/13867325/get-width-of-plot-area-in-ggplot2}
#' @keywords grob size height width
#'
#' @examples
#' ggsize(grob)
#'
#' @export

grobsize <- function(x) {
	# Function to get absolute size of a grob in inches.
	# Modified from: Hack-R's solution on Stackoverflow, see refernces.
	f <- tempfile()
	png(f)
	h <- grid::convertHeight(sum(x$heights), "in", TRUE)
	w <- grid::convertWidth(sum(x$widths), "in", TRUE)
	dev.off()
	unlink(f)
	return(c(w,h))
}

#------------------------------------------------------------------------------

#' ggplotScaleFreePlot
#'
#' evaluate the scale free property of a graph
#'
#' @param connectivity - igraph connectivity of a graph
#'
#' @return None
#'
#' @author Tyler W Bradshaw, \email{tyler.w.bradshaw@duke.edu}
#' @references 
#' @keywords igraph scale-free-fit connectivity
#'
#' @examples
#'
#' @export


ggplotScaleFreePlot <- function(connectivity, nBreaks = 10, truncated = FALSE,
                                removeFirst = FALSE, main = "", ...) {
	require(ggplot2)
	require(normalp)
  k <- connectivity
  discretized.k <- cut(k, nBreaks)
  dk <- tapply(k, discretized.k, mean)
  p.dk <- as.vector(tapply(k, discretized.k, length) / length(k))
  breaks1 <- seq(from = min(k), to = max(k), length = nBreaks + 1)
  hist1 <- suppressWarnings(hist(k,
    breaks = breaks1, equidist = FALSE,
    plot = FALSE, right = TRUE
  )) # ...
  dk2 <- hist1$mids
  dk <- ifelse(is.na(dk), dk2, dk)
  dk <- ifelse(dk == 0, dk2, dk)
  p.dk <- ifelse(is.na(p.dk), 0, p.dk)
  log.dk <- as.vector(log10(dk))
  if (removeFirst) {
    p.dk <- p.dk[-1]
    log.dk <- log.dk[-1]
  }
  log.p.dk <- as.numeric(log10(p.dk + 1e-09))
  lm1 <- lm(log.p.dk ~ log.dk)
  pvalue <- lmp(lm1)
  print(pvalue)

  title <- paste0(
    main, " Scale Free R2 =", as.character(round(summary(lm1)$adj.r.squared, 2)),
    ", slope =", round(lm1$coefficients[[2]], 2)
  )

  OUTPUT <- data.frame(
    scaleFreeRsquared = round(summary(lm1)$adj.r.squared, 2),
    slope = round(lm1$coefficients[[2]], 2)
  )
  # Generate ggplot.
  df <- as.data.frame(cbind(log.dk, log.p.dk))
  plot <- ggplot(df, aes(x = log.dk, y = log.p.dk)) + geom_point(size = 2) +
    ggtitle(title) +
    geom_abline(intercept = coef(lm1)[1], slope = coef(lm1)[2], color = "black", linetype = "dashed") +
    labs(y = expression(Log[10](p(k)))) +
    labs(x = expression(Log[10](k))) +
    theme(
      plot.title = element_text(hjust = 0.5, color = "black", size = 11, face = "bold"),
      axis.title.x = element_text(hjust = 0.5, color = "black", size = 11),
      axis.title.y = element_text(hjust = 0.5, color = "black", size = 11)
    )
  out <- list(ggplot = plot, stats = OUTPUT)
  return(out)
}

