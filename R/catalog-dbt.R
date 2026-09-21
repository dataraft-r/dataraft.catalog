#' Ingest dbt artifacts with OpenMetadata's existing engine
#'
#' Run `metadata ingest -c` using the separately installed OpenMetadata ingestion
#' CLI. OpenMetadata interprets models, tests, descriptions, tags and SQL lineage;
#' dataraft supplies verified artifacts from one [dataraft.dbt::dr_dbt_build()] or [dataraft.dbt::dr_dbt_test()]
#' invocation. No data rows or database connections are transferred by this
#' adapter. The database service and its table inventory must already exist in
#' OpenMetadata. Refresh database ingestion after creating new physical RAW
#' release relations or dbt models; the dbt engine enriches existing table
#' entities and can omit lineage edges when a table cannot be found.
#' `delivered` means the CLI returned zero, not that every intended graph edge
#' was verified. This is separate from [dr_catalog_openmetadata()], which publishes
#' ordinary product table metadata over HTTP.
#'
#' Install `openmetadata-ingestion[dbt]` in a separate Python environment and
#' supply its `metadata` executable. Version 2.0.0.0 (Python >= 3.10) is the
#' tested minimum; match the ingestion engine to your OpenMetadata server.
#' OpenMetadata's own version compatibility check remains enabled. The adapter
#' does not install Python, provision a server or replace its governance API.
#'
#' Authentication uses an environment variable. The official CLI expands an
#' environment placeholder in a temporary JSON configuration; the resolved token
#' is passed only in the child environment. CLI output is captured and discarded
#' because third-party errors may contain credentials. Configuration, logs and
#' artifact copies are removed after the attempt. The original dbt artifacts
#' remain project data and can contain SQL and database messages.
#'
#' A successful metadata delivery does not mean that dbt tests passed or that a
#' dataraft release was published. Both successful and failed dbt invocations
#' can supply valid metadata. Delivery errors return `pending` with a warning;
#' invalid or changed artifacts return `blocked` without starting the CLI.
#' Retry with `dataraft.core::dr_publish_metadata(adapter, result)`. A small credential-free
#' receipt beside the original artifacts records attempts across R sessions;
#' an already delivered invocation is not sent again. Use one coordinated writer
#' per artifact directory. After refreshing missing table inventory, explicitly
#' redeliver with `dataraft.core::dr_publish_metadata(adapter, result, force = TRUE)`.
#' An interruption after remote ingestion but before
#' the receipt is saved can cause a repeated delivery: this is at-least-once,
#' not an exactly-once transaction.
#'
#' @param endpoint OpenMetadata server base URL, without `/api` or `/api/v1`.
#' @param service Name of an existing OpenMetadata database service.
#' @param token_env Name of the environment variable containing the JWT token.
#'   Its value is resolved only when delivering metadata.
#' @param executable Name or path of the OpenMetadata `metadata` CLI.
#' @param timeout Positive timeout in seconds, or `Inf`.
#' @param options Optional named list of OpenMetadata dbt pipeline settings:
#'   `dbtUpdateDescriptions`, `dbtUpdateOwners`, `includeTags`,
#'   `searchAcrossDatabases`, `overrideLineage` (logical),
#'   `dbtClassificationName` (string), and `parsingTimeoutLimit` (positive integer).
#'   Credentials, artifact paths and arbitrary CLI flags are not accepted here.
#' @returns A catalog adapter for the `catalog` argument of [dataraft.dbt::dr_dbt_build()] and
#'   [dataraft.dbt::dr_dbt_test()]. `dataraft.core::dr_publish_metadata(adapter, result)` returns a delivery list
#'   with `status`, `destination`, `invocation_id`, `attempt`, timestamps,
#'   `exit_status`, `error_class`, `message` and `recorded`. It never changes
#'   the dbt result's `success` or `status` fields.
#' @seealso [dataraft.dbt::dr_dbt_status()], [dataraft.dbt::dr_dbt_publish()]
#' @export
#' @examples
#' catalog <- dr_catalog_openmetadata_dbt(
#'   "https://metadata.example", service = "warehouse"
#' )
#' dataraft.core::dr_inspect(catalog)
#' # After configuring the external service, CLI and token environment:
#' # result <- dataraft.dbt::dr_dbt_build(project, catalog = catalog, stop_on_failure = FALSE)
#' # delivery <- dataraft.core::dr_publish_metadata(catalog, result)
dr_catalog_openmetadata_dbt <- function(
  endpoint,
  service,
  token_env = "OPENMETADATA_JWT_TOKEN",
  executable = "metadata",
  timeout = 300,
  options = list()
) {
  catalog_endpoint(endpoint)
  endpoint <- sub("/+$", "", endpoint)
  if (grepl("/api(/v1)?$", endpoint)) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_catalog",
      "Use the OpenMetadata server base URL without /api or /api/v1."
    )
  }
  dataraft.core::dr_internal_scalar(service, "service")
  dataraft.core::dr_internal_scalar(token_env, "token_env")
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", token_env)) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_catalog",
      "token_env must be an environment variable name, not a token."
    )
  }
  dataraft.core::dr_internal_scalar(executable, "executable")
  if (
    !is.numeric(timeout) ||
      length(timeout) != 1L ||
      is.na(timeout) ||
      timeout <= 0
  ) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_catalog",
      "timeout must be positive seconds or Inf."
    )
  }
  dbt_catalog_options(options)
  structure(
    list(
      id = paste0(
        "openmetadata-dbt-",
        substr(
          fingerprint(list(endpoint, service, options)),
          1L,
          16L
        )
      ),
      endpoint = endpoint,
      service = service,
      token_env = token_env,
      executable = executable,
      timeout = timeout,
      options = options
    ),
    class = c("dr_openmetadata_dbt_catalog", "dr_catalog_adapter")
  )
}


dbt_catalog_options <- function(options) {
  rlang::local_error_call(rlang::caller_env())
  flags <- c(
    "dbtUpdateDescriptions",
    "dbtUpdateOwners",
    "includeTags",
    "searchAcrossDatabases",
    "overrideLineage"
  )
  allowed <- c(flags, "dbtClassificationName", "parsingTimeoutLimit")
  if (
    !is.list(options) ||
      (length(options) &&
        (is.null(names(options)) ||
          anyNA(names(options)) ||
          anyDuplicated(names(options)) ||
          any(!names(options) %in% allowed)))
  ) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_catalog",
      "options must be a uniquely named list of documented OpenMetadata dbt settings."
    )
  }
  for (name in intersect(names(options), flags)) {
    dataraft.core::dr_internal_flag(options[[name]], name)
  }
  if ("dbtClassificationName" %in% names(options)) {
    dataraft.core::dr_internal_scalar(
      options$dbtClassificationName,
      "dbtClassificationName"
    )
  }
  if ("parsingTimeoutLimit" %in% names(options)) {
    value <- options$parsingTimeoutLimit
    if (
      !is.numeric(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        value < 1 ||
        value != floor(value)
    ) {
      dataraft.core::dr_internal_abort(
        subclass = "dataraft_error_catalog",
        "parsingTimeoutLimit must be a positive integer."
      )
    }
  }
  invisible(options)
}


#' @export
#' @importFrom dataraft.core dr_inspect
dr_inspect.dr_openmetadata_dbt_catalog <- function(x, ...) {
  list(
    type = "OpenMetadata dbt ingestion",
    id = x$id,
    endpoint = x$endpoint,
    service = x$service,
    token_env = x$token_env,
    executable = x$executable,
    options = x$options
  )
}


#' @export
#' @importFrom dataraft.core dr_capabilities
dr_capabilities.dr_openmetadata_dbt_catalog <- function(x, ...) {
  result <- catalog_capabilities("OpenMetadata dbt ingestion")
  result$statuses <- c("dbt_success", "dbt_failed")
  result$metadata_inputs <- "dr_dbt_result"
  result
}


#' @export
#' @importFrom dataraft.core dr_check_component
dr_check_component.dr_openmetadata_dbt_catalog <- function(x, ...) {
  dataraft.core::dr_internal_need("processx")
  if (!nzchar(Sys.which(x$executable)) && !file.exists(x$executable)) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_catalog",
      "OpenMetadata CLI not found. Install openmetadata-ingestion[dbt] >= 2.0.0.0 in a separate Python environment and set executable to its metadata command; match the server version.",
      "dr_dbt_catalog_unavailable"
    )
  }
  if (!nzchar(Sys.getenv(x$token_env, unset = ""))) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_catalog",
      "Set the environment variable named by token_env before delivering metadata.",
      "dr_dbt_catalog_credentials"
    )
  }
  invisible(x)
}


#' @export
#' @importFrom dataraft.core dr_publish_metadata
dr_publish_metadata.dr_openmetadata_dbt_catalog <- function(
  catalog,
  metadata,
  ...,
  force = FALSE
) {
  dataraft.core::dr_internal_flag(force, "force")
  if (!inherits(metadata, "dr_dbt_result")) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_catalog",
      "Use this adapter with dr_dbt_build(catalog = ...), dr_dbt_test(catalog = ...), or dr_publish_metadata(adapter, dbt_result).",
      "dr_dbt_catalog_invalid"
    )
  }
  delivery <- list(
    status = "pending",
    destination = catalog$id,
    invocation_id = metadata$invocation_id,
    attempt = 1L,
    started_at = now(),
    finished_at = NULL,
    exit_status = NULL,
    error_class = NULL,
    message = NULL,
    recorded = FALSE
  )
  valid <- tryCatch(
    optional_dbt("dr_internal_dbt_catalog_artifacts")(metadata),
    error = identity
  )
  if (inherits(valid, "error")) {
    delivery$status <- "blocked"
    delivery$error_class <- "dr_dbt_artifact_invalid"
    delivery$message <- "Metadata delivery blocked: dbt artifacts are missing, malformed or changed. Use the original unchanged artifacts from this invocation."
    delivery$finished_at <- now()
    optional_dbt("dr_internal_dbt_catalog_warning")(delivery)
    return(delivery)
  }
  receipt <- file.path(metadata$artifacts_dir, paste0(catalog$id, ".json"))
  previous <- dbt_catalog_read_receipt(receipt, metadata$invocation_id)
  if (!force && identical(previous$status, "delivered")) {
    return(previous)
  }
  delivery$attempt <- (previous$attempt %||% 0L) + 1L
  outcome <- tryCatch(dbt_catalog_ingest(catalog, metadata), error = identity)
  if (inherits(outcome, "error")) {
    known <- c(
      "dr_dbt_catalog_unavailable",
      "dr_dbt_catalog_credentials",
      "dr_dbt_artifact_invalid"
    )
    type <- intersect(class(outcome), known)
    delivery$error_class <- if (length(type)) {
      type[[1L]]
    } else {
      "dr_dbt_catalog_process"
    }
    delivery$message <- switch(
      delivery$error_class,
      dr_dbt_catalog_unavailable = "OpenMetadata CLI is unavailable. Install openmetadata-ingestion[dbt] >= 2.0.0.0 matching the server and set executable; retry dr_publish_metadata(adapter, result).",
      dr_dbt_catalog_credentials = "OpenMetadata authentication is not configured. Set the environment variable named by token_env and retry dr_publish_metadata(adapter, result).",
      dr_dbt_artifact_invalid = "Metadata delivery blocked: artifacts changed while preparing the ingestion. Restore the original artifacts before retrying.",
      "OpenMetadata ingestion could not finish. Check CLI availability, timeout and server connectivity, then retry dr_publish_metadata(adapter, result)."
    )
    if (delivery$error_class == "dr_dbt_artifact_invalid") {
      delivery$status <- "blocked"
    }
  } else {
    delivery$exit_status <- as.integer(outcome)
    if (identical(delivery$exit_status, 0L)) {
      delivery$status <- "delivered"
    } else {
      delivery$error_class <- "dr_dbt_catalog_exit"
      delivery$message <- "OpenMetadata ingestion returned a nonzero exit status. Check engine/server versions, authentication and the existing database service, then retry dr_publish_metadata(adapter, result)."
    }
  }
  delivery$finished_at <- now()
  delivery$recorded <- TRUE
  saved <- tryCatch(
    dbt_catalog_write_receipt(receipt, delivery),
    error = function(e) FALSE
  )
  if (!isTRUE(saved)) {
    delivery$recorded <- FALSE
    rlang::warn(
      "Could not save the metadata delivery receipt. The dbt outcome is unchanged; retrying may deliver metadata again.",
      class = "dr_dbt_catalog_receipt"
    )
  }
  if (delivery$status != "delivered") {
    optional_dbt("dr_internal_dbt_catalog_warning")(delivery)
  }
  delivery
}


dbt_catalog_config <- function(catalog, path, files) {
  rlang::local_error_call(rlang::caller_env())
  artifacts <- list(
    dbtConfigType = "local",
    dbtManifestFilePath = file.path(path, "manifest.json"),
    dbtRunResultsFilePath = file.path(path, "run_results.json")
  )
  if ("catalog.json" %in% files) {
    artifacts$dbtCatalogFilePath <- file.path(path, "catalog.json")
  }
  list(
    source = list(
      type = "dbt",
      serviceName = catalog$service,
      sourceConfig = list(
        config = c(
          list(type = "DBT", dbtConfigSource = artifacts),
          catalog$options
        )
      )
    ),
    sink = list(
      type = "metadata-rest",
      config = structure(list(), names = character())
    ),
    workflowConfig = list(
      loggerLevel = "ERROR",
      raiseOnError = TRUE,
      successThreshold = 100L,
      openMetadataServerConfig = list(
        hostPort = paste0(catalog$endpoint, "/api"),
        authProvider = "openmetadata",
        enableVersionValidation = TRUE,
        storeServiceConnection = FALSE,
        securityConfig = list(jwtToken = "DATARAFT_TOKEN_PLACEHOLDER")
      )
    )
  )
}


dbt_catalog_ingest <- function(catalog, result) {
  rlang::local_error_call(rlang::caller_env())
  dataraft.core::dr_check_component(catalog)
  temporary <- tempfile("dataraft-dbt-metadata-")
  if (!dir.create(temporary, mode = "0700")) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_catalog",
      "Could not create the ingestion directory."
    )
  }
  on.exit(unlink(temporary, recursive = TRUE), add = TRUE)
  files <- names(result$artifact_hashes)
  if (!all(file.copy(file.path(result$artifacts_dir, files), temporary))) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_catalog",
      "Could not prepare the dbt artifacts.",
      "dr_dbt_artifact_invalid"
    )
  }
  optional_dbt("dr_internal_dbt_catalog_artifacts")(result, temporary)
  config <- dbt_catalog_config(catalog, temporary, files)
  encoded <- as.character(jsonlite::toJSON(
    config,
    auto_unbox = TRUE,
    null = "null"
  ))
  # The CLI expands environment variables before JSON parsing. Escape literal
  # dollar signs everywhere else, and JSON-escape the child-only token value.
  encoded <- gsub("$", "\\u0024", encoded, fixed = TRUE)
  encoded <- sub(
    '"jwtToken":"DATARAFT_TOKEN_PLACEHOLDER"',
    '"jwtToken":"${DATARAFT_OPENMETADATA_JWT_JSON}"',
    encoded,
    fixed = TRUE
  )
  config_path <- file.path(temporary, "ingestion.json")
  writeLines(encoded, config_path, useBytes = TRUE)
  Sys.chmod(config_path, "0600")
  token <- as.character(jsonlite::toJSON(
    Sys.getenv(catalog$token_env),
    auto_unbox = TRUE
  ))
  token <- substr(token, 2L, nchar(token) - 1L)
  executable <- unname(Sys.which(catalog$executable))
  if (!nzchar(executable)) {
    executable <- dataraft.core::dr_internal_absolute_path(catalog$executable)
  }
  dbt_catalog_process(
    executable,
    c("ingest", "-c", config_path),
    wd = temporary,
    timeout = catalog$timeout,
    env = c("current", DATARAFT_OPENMETADATA_JWT_JSON = token)
  )
}


dbt_catalog_process <- function(command, args, wd, timeout, env) {
  rlang::local_error_call(rlang::caller_env())
  result <- processx::run(
    command,
    args,
    wd = wd,
    timeout = timeout,
    env = env,
    echo = FALSE,
    error_on_status = FALSE,
    cleanup_tree = TRUE
  )
  result$status
}


dbt_catalog_read_receipt <- function(path, invocation_id) {
  rlang::local_error_call(rlang::caller_env())
  if (!file.exists(path)) {
    return(NULL)
  }
  receipt <- tryCatch(
    jsonlite::read_json(path, simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (
    is.null(receipt) ||
      !identical(receipt$invocation_id, invocation_id) ||
      !is.numeric(receipt$attempt) ||
      length(receipt$attempt) != 1L ||
      is.na(receipt$attempt) ||
      receipt$attempt < 1
  ) {
    return(NULL)
  }
  receipt
}


dbt_catalog_write_receipt <- function(path, delivery) {
  rlang::local_error_call(rlang::caller_env())
  temp <- tempfile(".metadata-receipt-", tmpdir = dirname(path))
  on.exit(unlink(temp), add = TRUE)
  jsonlite::write_json(delivery, temp, auto_unbox = TRUE, null = "null")
  Sys.chmod(temp, "0600")
  isTRUE(file.rename(temp, path))
}
