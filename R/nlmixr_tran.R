#' Extend and translate nlmixr models
#' 
#' @param object A function specifying the nlmixr model
#' @param ... Currently ignored
#' @return \code{object} modified by expanding functions
#' @examples
#' model <- function() {
#'   ini({
#'     lcl <- (0.01)
#'     lv <- log(0.5)
#'     eta_cl ~ 0.1
#'     add_err <- 0.1
#'   })
#'   model({
#'     linear_model_ode_onecmt_iv(cl=exp(lcl+eta_cl), v=exp(lv))
#'     cp <- central/exp(lv)
#'     cp ~ add(add_err)
#'   })
#' }
#' nlmixr_trans(model, linear_model_ode_onecmt_iv)
#' @export
nlmixr_trans <- function(object, ...) {
  args <- match.call(expand.dots = FALSE)$...
  if (!all(sapply(X=args, FUN=is.name))) {
    stop("all ... arguments must be function names")
  }
  ret <- object
  for (idx in seq_along(args)) {
    ret <- nlmixr_trans_single(ret, translate_fun=args[[idx]])
  }
  ret
}

# Operate on the outer function
nlmixr_trans_single <- function(object, translate_fun) {
  methods::functionBody(object) <-
    nlmixr_trans_call(
      methods::functionBody(object),
      translate_fun=translate_fun
    )$object
  object
}

# Normal append doesn't work because it will coerce language objects and calls
# to lists.
#
# The default value of skip=1 is to skip the {} that are around most (or maybe
# all) language vectors.
append_language <- function(x, values, after=length(x), skip=1) {
  ret <- x
  insertions <- setdiff(seq_along(values), skip)
  for (idx in seq_along(insertions)) {
    ret[[after + idx]] <- values[[insertions[idx]]]
  }
  if (after < length(x)) {
    for (idx in seq(from=after+1, to=length(x))) {
      ret[[idx + length(insertions)]] <- x[[idx]]
    }
  }
  ret
}

# Operate on the calls and other language objects within the function
nlmixr_trans_call <- function(object, translate_fun) {
  changed <- FALSE
  if (length(object) > 1) {
    # it is not a simple name
    if (identical(object[[1]], translate_fun)) {
      object <- str2expression(c("{", eval(object), "}"))
      changed <- TRUE
    } else {
      # Go in rev() so that we don't accidentally overwrite something.
      for (idx in rev(seq_along(object))) {
        # Maybe modify the object by recursion
        tmp <- nlmixr_trans_call(object[[idx]], translate_fun=translate_fun)
        if (tmp$changed) {
          # Drop what was just changed
          object[[idx]] <- NULL
          # Insert the new code
          object <- append_language(x=object, values=tmp$object[[1]], after=idx-1)
        } else {
          object[[idx]] <- tmp$object
        }
      }
    }
  } else {
    # it is a name
    if (identical(object, translate_fun)) {
      stop("translate_fun should not show up as a name.  Please report a bug with a reproducible example.") # nocov
    }
  }
  list(object=object, changed=changed)
}
