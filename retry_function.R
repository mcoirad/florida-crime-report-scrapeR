### Useful try/catch function
# https://stackoverflow.com/questions/20770497/how-to-retry-a-statement-on-error

retry <- function(expr, isError=function(x) "try-error" %in% class(x), maxErrors=20, sleep=0.1, retryFunction = function() {return (NULL)}, returnError = TRUE) {
  attempts = 0
  retval = try(eval(expr))
  while (isError(retval)) {
    attempts = attempts + 1
    if (attempts >= maxErrors) {
      if (returnError == TRUE) {
        msg = sprintf("retry: too many retries [[%s]]", capture.output(str(retval)))
        flog.fatal(msg)
        stop(msg)
      } else {
        return ("FAILURE")
      }
    } else {
      msg = sprintf("retry: error in attempt %i/%i [[%s]]", attempts, maxErrors, 
                    capture.output(str(retval)))
      flog.error(msg)
      warning(msg)
    }
    if (sleep > 0) Sys.sleep(sleep)
    retryFunction()
    retval = try(eval(expr))
  }
  return(retval)
}