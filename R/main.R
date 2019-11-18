
flatmap <- function(xs, fun) {
  unlist(lapply(xs, fun),
         use.names = FALSE,
         recursive = FALSE)
}

#' Build a custom compound operator.
#' @param sep Character(1) operator name.
#' @param prefix Logical(1) should operator be at beginning?
#' @param group Logical(1) should the parts be wrapped in parens?
#' @param max_length Integer(1) maximum number of named arguments.
#' @return Function -> Character
#' @name Compound
#' @export
compound <- function(sep, prefix = TRUE, group = TRUE, max_length = Inf) {
  stopifnot(length(sep) == 1)
  sep = trimws(sep)
  sep = paste0(" -", sep, " ")
  function(...) {
    find_args = list(...)
    n_args = length(find_args)
    stopifnot(n_args <= max_length)
    arg_name = names(find_args)
    if(is.null(arg_name)) {
      return(trimws(paste0(sep, paste(find_args, collapse = sep))))
    }
    map = flatmap(arg_name, function(nm) {
      paste(paste0("-", nm),
            shQuote(find_args[[nm]]),
            collapse = sep)
    })
    out = paste0(map, collapse = sep)
    grouped = grepl(sep, out) && group
    # cat("Prefixed:", prefix, "\nGrouped:", grouped, "\n")
    if(prefix && !grouped) {
      trimws(paste0(sep, out))
    } else if(prefix && grouped) {
      paste("\\(", trimws(paste0(sep, out)), "\\)")
    } else if(!prefix && grouped) {
      paste("\\(", out, "\\)")
    } else {
      out
    }
  }
}

#' @rdname Compound
#' @export
or <- compound("or", prefix = FALSE)

#' @rdname Compound
#' @export
and <- compound("and", prefix = FALSE)

#' @rdname Compound
#' @export
not <- compound("not", max_length = 1)


#' Wrapper around GNU Find CLI utility.
#'
#' It's designed so that arguments are just passed in
#' order much like the GNU Find utility itself.
#'
#' Hopefully this will be somewhat useful to someone.
#'
#' There are two special kinds of argument names that may be repeated:
#'
#' - switch (sw) :: switches which don't have arguments
#' - compound (cmp) :: compound statements
#'
#' These enable this wrapper to handle the a broader chunch of the
#' Find utilities functionality.
#'
#' Please refer the GNU Find INFO pages for more details.
#' @param search_path Character path names.
#' @param ... Arguments to find.
#' @param find_exec Character(1) path to GNU Find executable.
#' @param debug Logical(1) whether to print generated command.
#' @return Character.
#' @examples
#' # Find all non-hidden directories in $HOME.
#' gnu_find(search_path = "~",
#'          type = "d", maxdepth = 1,
#'          compound = not(path = "*/\\.*"))
#' @export
gnu_find <- function(search_path = getwd(),
                     ...,
                     find_exec = "find",
                     debug = FALSE) {
  find_args = c(...)
  argname = names(find_args)
  switch = argname %in% c("sw", "switch")
  compound = argname %in% c("cmp", "compound")
  pathstr = normalizePath(search_path, mustWork = TRUE)
  pathstr <- shQuote(pathstr)
  instr = ifelse(switch, paste0("-", find_args),
                 ifelse(compound, find_args,
                        paste(paste0("-", argname),
                              shQuote(unname(find_args)))))
  input = trimws(c(pathstr, instr))
  if(debug) {
    cat("Find command:\n",
        paste(find_exec, paste(input, collapse = " ")),
        "\n")
  }
  system2(command = find_exec, args = input, stdout = TRUE)
}
