#' get_help
#' @export get_help

# Define a function to parse the R help documentation of an R
# package::function.
get_help <- function(fun,pkg=NULL) {
    # Get help.
    # character vector.
    # From StackOverflow:
    # https://stackoverflow.com/questions/8918753/r-help-page-as-object
    help_file <- utils:::.getHelpFile(help(eval(fun), eval(pkg)))
    help_latex <- capture.output({
      tools:::Rd2latex(help_file)
    })
    # help_text <- tools::parseLatex(help_latex)
    return(help_latex)
  }

# Function to remove empty strings from character vector.
drop_empty_str <- function(string) {
	string[string != ""]
}

# Function to get R documentation header.
get_header <- function(latex) {
	# Get Header.
	loc <- grep("Header[A-Z]\\{", latex)
	header_vec <- drop_empty_str({
		unlist( strsplit(latex[loc], "\\{|\\}"))[-c(1)] 
	})
	header <- paste(unique(header_vec), collapse = ": ")
	return(list(header = header))
}

# Function to get R documentation keywords.
get_keywords <- function(latex) {
	# Get keywords.
	raw <- latex[grep("keyword\\{", latex)]
	keywords <- gsub("\\\\keyword\\{(.+)\\}\\{(.+)\\}", "\\1:\\2", raw)
	return(list(keywords = keywords))
}

# Function to get location of R documentation terms.
get_loc <- function(latex, term) {
	# Get locations of the start and end of a given term.
	empty_item <- c(from = "begin\\{TERM\\}", to = "end\\{TERM\\}")
	re <- gsub("TERM", term, empty_item)
	start_end <- sapply(re, function(ex) grep(ex, latex))
	return(start_end)
}

# Function to get description of R documentation.
get_description <- function(latex) {
	# Get description.
	loc <- get_loc(latex, "Description")
	idx <- do.call(seq, as.list(loc))
	return(list(description = latex[idx][2]))
}

# Function to get usage of R documentation.
get_usage <- function(latex) {
	# Get usage.
	loc <- get_loc(latex, "Usage")
	idx <- do.call(seq, as.list(loc))
	sublatex <- latex[idx]
	subloc <- get_loc(sublatex, "verbatim")
	idx <- do.call(seq, as.list(subloc))
	raw <- paste(tail(head(sublatex[idx], -1), -1), collapse = "")
	usage_no_quote <- gsub("\\\"", "'", raw)
	usage <- gsub("\\s+", " ", usage_no_quote) # remove duplicate ws
	return(list(Usage = usage))
}

# Function to get R documentation arguments.
get_args <- function(latex) {
	# Extract arguments chunk.
	loc <- get_loc(latex, "Arguments")
	idx <- do.call(seq, as.list(loc))
        sublatex <- latex[idx]
        # Get names of arguments.
	re <- "\\\\item\\[\\\\code\\{[a-z]*\\}\\]"
	idx <- grep(re, sublatex)
	f <- function(arg) {
		re <- "\\\\item\\[\\\\code\\{([a-z].*)\\}\\]"
	        arg_name <- unlist(strsplit(gsub(re, "\\1", arg), " "))[1]
	        return(arg_name)
	}
	args <- sapply(sublatex[idx], f, USE.NAMES = FALSE)
	# Get position of start and end of each argument.
	end <- grep("\\\\end\\{Arguments\\}", sublatex)
	idx <- c(idx, end)
	to <- ""
	i <- 1
	loc <- vector(mode = "list", length(args))
	names(loc) <- args
	while (!is.na(to)) {
	from <- idx[i]
	to <- idx[i + 1] - 1
	if (is.na(to)) { next }
	loc[[i]] <- c(from = from, to = to)
	i <- i + 1
	}
	# For each item, extract the text.
	doc_strings <- lapply(loc, function(x) {
	      idx <- do.call(seq, as.list(x))
	      raw <- paste(sublatex[idx], collapse = " ")
	      re <- "\\\\[^ ]*"
	      doc_string <- gsub(re, "\\1", raw)
	      doc_string <- gsub("\\s+", " ", doc_string) # remove duplicate ws
	      doc_string <- trimws(doc_string)
	      return(doc_string)
	})
	# Return named list of arguments and their documentation.
	return(doc_strings)
}

## Main:
# Parse the R documentation generated from help(function,package).
getRdoc <- function(fun, pkg = NULL, arg = NULL, ...) {
	# Written to extract the documentation from download.file().
	# Doesn't work for data.frame.
	latex <- get_help(fun, pkg)
	rdoc <- list(header = get_header(latex),
	     keywords = get_keywords(latex),
	     description = get_description(latex),
	     usage = get_usage(latex),
	     args = get_args(latex)
	     )
  # Return the R documentation.
  if (is.null(arg)) {
    return(rdoc)
  } else {
  # If provided, return just the argument of interest.
    args <- get_args(latex)
    return(args[[arg]])
  }
}
