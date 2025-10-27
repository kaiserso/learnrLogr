#' Learnr Logging Handlers
#'
#' Functions to register handlers that log session and exercise events to a
#' Cloudflare Worker endpoint.
#' @export
initialize_logger <- function() {
  library(httr)
  library(jsonlite)
  library(dotenv)
  library(uuid)

  # use a private .env or .Renviron file to store sensitive configuration
  # parameters and authorization credentials
  if (file.exists(".env")) try(dotenv::load_dot_env(), silent = TRUE)
  if (file.exists(".Renviron")) try(dotenv::load_dot_env(".Renviron"), silent = TRUE)

  message("currently in: ", getwd())
  options(
    logger.endpoint = Sys.getenv("LOGGER_ENDPOINT", unset = ""),
    logger.api.key  = Sys.getenv("LOGGER_API_KEY", unset = ""),
    logger.enabled  = tolower(Sys.getenv("LOGGER_ENABLED", "false")) == "true",
    # allow caller to override the endpoint and api key via options
    logger.endpoint = getOption("logger.endpoint", logger.endpoint),
    logger.api.key = getOption("logger.api.key", logger.api.key),
    logger.enabled = getOption("logger.enabled", logger.enabled),
    # these are user-defined/overridable
    tutorial.id = getOption("tutorial.id", "unknown-tutorial"),
    logger.timeout = getOption("logger.timeout", 3)
  )

  invisible(TRUE)
}

#' Register logging event handlers for learnr
#'
#' @export
register_learnr_handlers <- function() {
  #initialize_logger()

  # Log when a question is submitted
  event_register_handler("session_start", function(session, event, data) {
    message("Session started")
    observe({
      id <- UUIDgenerate()
      payload <- list(
        session = id,
        event = "session_start",
        info = list(
          user_agent = session$clientData$useragent,
          path = session$clientData$url_pathname,
          hostname = session$clientData$url_hostname,
          app = getOption("tutorial.id")
        )
      )
      send_log(payload)
    })
  })
  # log when an exercise is checked after submission
  event_register_handler("exercise_result", function(session, event, data) {

    message("🧩 Exercise submitted: ", data$label)

    observe({
      correct_val <- if (!is.null(data$feedback)) data$feedback$correct else NA
      payload <- list(
        session = session$token,
        event   = event,
        info    = list(
          label = data$label,
          correct = correct_val,
          checked = data$checked,
          hostname = session$clientData$url_hostname,
          app = getOption("tutorial.id")
        )
      )
      send_log(payload)
    })
  })
}

#' Internal helper to send log payload
#' @noRd
send_log <- function(payload) {
  endpoint <- getOption("logger.endpoint", "")
  api_key  <- getOption("logger.api.key", "")
  enabled  <- getOption("logger.enabled", FALSE)
  timeout  <- getOption("logger.timeout", 3)

  if(enabled && nzchar(endpoint) && nzchar(api_key)) {
    res <- try(
      httr::POST(endpoint,
           httr::add_headers(`x-api-key` = api_key),
           body = jsonlite::toJSON(payload, auto_unbox = TRUE),
           encode = "json",
           httr::timeout(timeout)),
      silent = TRUE
    )
    # Log the response or error
    if (inherits(res, "try-error")) {
      message("❌ Failed to send log: ", conditionMessage(attr(res, "condition")))
    } else {
      message("✅ Log sent, status: ", res$status_code)
    }
  } else {
    message("⚠️ Logger is disabled or not properly configured.")
    message(jsonlite::toJSON(payload, auto_unbox = TRUE))
  }
}

.onLoad <- function(libname, pkgname) {
  # Try to load .Renviron manually if it exists in working dir
  envfile <- ".Renviron"
  if (file.exists(envfile)) {
    try(readRenviron(envfile), silent = TRUE)
  }

  # Set default options if not already set
  op <- options()
  op.logger <- list(
    logger.enabled  = tolower(Sys.getenv("LOGGER_ENABLED", "false")) == "true",
    logger.endpoint = Sys.getenv("LOGGER_ENDPOINT", ""),
    logger.api.key  = Sys.getenv("LOGGER_API_KEY", ""),
    logger.timeout  = as.numeric(Sys.getenv("LOGGER_TIMEOUT", 3)),
    logger.app      = Sys.getenv("LOGGER_APP", "unknown")
  )

  toset <- !(names(op.logger) %in% names(op))
  if (any(toset)) options(op.logger[toset])

  invisible()
}
