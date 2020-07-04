
# Random colors.
cmd <- "curl 'http://colormind.io/api/' --silent --data-binary '{\"model\":\"default\"}'"
response <- system(cmd,intern=TRUE)

result <- rjson::fromJSON(response)
colors <- unlist(result,recursive=FALSE)
names(colors) <- gsub("result","color",names(colors))

