#' Get Named Entities using GermaNER
#' 
#' Use GermaNER (\url{https://github.com/tudarmstadt-lt/GermaNER}) for named
#' entity recognition.
#' 
#' The package includes a small data set called 'germaparl_ne_regions'
#' 
#' @param x A \code{partition} object.
#' @param mem Java flags passed into system call.
#' @param filename Name of the GermaNER jar file.
#' @examples
#' \dontrun{
#' library(polmineR)
#' library(magrittr)
#' if (!file.exists(germaner_jar_filename())) germaner_download_jar()
#' 
#' x <- corpus("GERMAPARLMINI") %>% subset(speaker == "Angela Dorothea Merkel")
#' y <- germaner_get_named_entities(x = x)
#' }
#' @importFrom data.table setnames := as.data.table setcolorder
#' @importFrom polmineR get_token_stream
#' @importFrom utils download.file
#' @return A \code{data.table}.
#' @export germaner_get_named_entities
#' @rdname GermaNER
germaner_get_named_entities <- function(x, mem = "-Xmx4g"){
  
  tokens <- get_token_stream(x, p_attribute = "word")
  pos <- get_token_stream(x, p_attribute = "pos")
  
  cutpoints <- cut(
    x = 1L:length(tokens),
    breaks = unique(c(1L, grep("^\\$\\.$", pos), length(tokens))),
    include.lowest = TRUE
  )
  
  tokenstream <- split(tokens, cutpoints)
  
  tokenstream <- lapply(tokenstream, function(x) c(x, "")) # blank lines required for sentences
  vec <- unlist(tokenstream)
  vec_length <- length(vec)
  
  outputfile <- tempfile()
  inputfile <- tempfile()
  writeLines(text = vec, con = inputfile, sep = "\n")
  rm(vec)

  cmd <- sprintf("java %s -jar %s -t %s -o %s", mem, germaner_jar_filename(), inputfile, outputfile)
  system(cmd)
  
  tagged_raw <- readLines(outputfile)
  
  if (length(tagged_raw) != vec_length) warning("differing lengths!")
  unlink(inputfile)
  unlink(outputfile)
  y <- strsplit(x = tagged_raw, split = "\\s+")
  for (i in rev(which(sapply(y, function(x) identical(x, character())) == TRUE))) y[[i]] <- NULL
  df <- data.frame(do.call(rbind, y), stringsAsFactors = FALSE)
  
  dt <- as.data.table(df)
  setnames(dt, old = c("X1", "X2"), new = c("token", "ne"))
  dt[, "cpos" := unlist(apply(x@cpos, 1, function(x) x[1]:x[2]))]
  setcolorder(dt, c("cpos", "token", "ne"))
  
  if (!identical(dt[["token"]], tokens)){
    dt[,"token_original" := tokens]
    warning("Something is wrong!")
  }
  
  return(dt)
}


#' @export germaner_jar_filename
#' @rdname GermaNER
germaner_jar_filename <- function(filename = "GermaNER-09-09-2015.jar"){
  file.path(system.file(package = "GermaNER"), "extdata", "jar", filename)
}

#' @export germaner_download_jar
#' @rdname GermaNER
germaner_download_jar <- function(){
  if (file.exists(germaner_jar_filename())){
    message("The jar file is already present.")
    invisible(return(TRUE))
  }
  dir.create(file.path(system.file(package = "GermaNER"), "extdata", "jar"), recursive = TRUE)
  download.file(
    url = "https://github.com/tudarmstadt-lt/GermaNER/releases/download/germaNER0.9.1/GermaNER-09-09-2015.jar",
    destfile = germaner_jar_filename(),
    quiet = FALSE
  )
}

#' @rdname GermaNER
"germaparl_ne_regions"
