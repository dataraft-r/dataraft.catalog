#' Send execution lineage to an OpenLineage endpoint
#'
#' Sends a START event using the recorded start time, followed by COMPLETE
#' or FAIL, after execution has finished. This is buffered historical lineage,
#' not live progress monitoring. Both events keep the same deterministic UUID
#' across [dataraft.core::dr_retry_catalogs()] calls. Delivery is at least once; a retry can
#' repeat an already accepted START event. Failed and blocked runs have no
#' output datasets. No data rows or executable transformation definitions are
#' sent. Output schema fields are included when available.
#'
#' External business catalogs consume descriptive metadata. They are distinct
#' from [dataraft.lake::dr_registry()], which records the local lake publication lifecycle.
#' @param endpoint Full HTTP(S) OpenLineage ingestion URL, for example
#'   `https://lineage.example/api/v1/lineage`. Supply authentication through
#'   `request`, never embedded credentials or URL query parameters.
#' @param namespace Job namespace. Dataset namespaces identify physical sources.
#' @param datasets Named list keyed by logical asset or source ID. Each value
#'   contains `namespace` and `name` following OpenLineage naming conventions,
#'   for example `list(namespace = "postgres://warehouse:5432",
#'   name = "insurance.public.contracts")`. Descriptor `dataset` identities
#'   take precedence. Unknown locations use an explicitly logical DataRaft
#'   namespace and cannot merge with physical datasets from other tools.
#'   Supply only public identifiers without credentials or signed URLs.
#' @param request Optional httr2 request or function accepting a request and
#'   returning a configured request. A zero-argument request factory is also
#'   accepted. Factories can obtain fresh credentials at delivery time. The
#'   adapter always restores its configured destination URL.
#' @returns A catalog adapter for [dataraft.core::dr_add_catalog()].
#' @seealso [dr_catalog_openmetadata()], [dataraft.core::dr_retry_catalogs()]
#' @export
#' @examples
#' catalog <- dr_catalog_openlineage("https://lineage.example/api/v1/lineage")
#' dataraft.core::dr_inspect(catalog)
dr_catalog_openlineage <- function(
  endpoint,
  namespace = "dataraft",
  request = NULL,
  datasets = list()
) {
  catalog_endpoint(endpoint)
  dataraft.core::dr_internal_scalar(namespace, "namespace")
  check_catalog_request(request)
  openlineage_check_datasets(datasets)
  structure(
    list(
      id = paste0(
        "openlineage-",
        substr(fingerprint(list(endpoint, namespace, datasets)), 1L, 16L)
      ),
      endpoint = endpoint,
      namespace = namespace,
      request = request,
      datasets = datasets
    ),
    class = c("dr_openlineage_catalog", "dr_catalog_adapter")
  )
}


#' @export
#' @importFrom dataraft.core dr_check_component
dr_check_component.dr_openlineage_catalog <- function(x, ...) {
  dataraft.core::dr_internal_need("httr2")
  catalog_endpoint(x$endpoint)
  dataraft.core::dr_internal_scalar(x$namespace, "namespace")
  check_catalog_request(x$request)
  openlineage_check_datasets(x$datasets %||% list())
  invisible(x)
}


#' @export
#' @importFrom dataraft.core dr_inspect
dr_inspect.dr_openlineage_catalog <- function(x, ...) {
  list(
    type = "OpenLineage",
    id = x$id,
    endpoint = x$endpoint,
    namespace = x$namespace,
    datasets = x$datasets,
    delivery = "buffered START and terminal event"
  )
}


#' @export
#' @importFrom dataraft.core dr_capabilities
dr_capabilities.dr_openlineage_catalog <- function(x, ...) {
  catalog_capabilities("OpenLineage", all_statuses = TRUE)
}


#' @export
#' @importFrom dataraft.core dr_publish_metadata
dr_publish_metadata.dr_openlineage_catalog <- function(catalog, metadata, ...) {
  dataraft.core::dr_check_component(catalog)
  events <- openlineage_events(catalog, metadata)
  for (event in events) {
    request <- catalog_request(catalog$endpoint, catalog$request)
    request <- httr2::req_body_json(request, event, auto_unbox = TRUE)
    request <- httr2::req_method(request, "POST")
    httr2::req_perform(request)
  }
  invisible(NULL)
}


openlineage_events <- function(catalog, metadata) {
  rlang::local_error_call(rlang::caller_env())
  dataraft.core::dr_internal_scalar(metadata$run_id, "metadata$run_id")
  dataraft.core::dr_internal_scalar(metadata$product, "metadata$product")
  producer <- "https://github.com/dataraft-r/dataraft"
  dataset <- function(name, schema = NULL, descriptor = list()) {
    rlang::local_error_call(rlang::caller_env())
    identity <- descriptor$dataset %||% catalog$datasets[[name]]
    physical <- !is.null(identity)
    if (physical) {
      openlineage_check_identity(identity)
    }
    result <- identity %||%
      list(
        namespace = paste0("dataraft://", catalog$namespace),
        name = name
      )
    result$facets <- list(
      dataraft_identity = list(
        `_producer` = producer,
        `_schemaURL` = paste0(
          "https://raw.githubusercontent.com/dataraft-r/dataraft.catalog/",
          "main/inst/schemas/DataraftIdentityDatasetFacet.json"
        ),
        logicalId = name,
        physicalIdentity = physical
      )
    )
    if (length(schema)) {
      result$facets$schema <- list(
        `_producer` = producer,
        `_schemaURL` = paste0(
          "https://openlineage.io/spec/facets/1-1-1/",
          "SchemaDatasetFacet.json#/$defs/SchemaDatasetFacet"
        ),
        fields = unname(lapply(names(schema), function(field) {
          list(name = field, type = as.character(schema[[field]]))
        }))
      )
    }
    result
  }
  inputs <- metadata$inputs %||% list()
  if (is.data.frame(inputs)) {
    inputs <- dataraft.core::dr_internal_safe_descriptors(inputs)
  }
  inputs <- unname(lapply(seq_along(inputs), function(i) {
    input <- inputs[[i]]
    source <- input$source %||% input
    name <- source$id %||%
      source$product %||%
      source$table %||%
      source$path %||%
      input$name %||%
      paste0(metadata$product, "/input-", i)
    dataset(paste(as.character(name), collapse = "."), descriptor = source)
  }))
  successful <- metadata$status %in% c("completed", "published", "cached")
  event <- list(
    eventTime = event_time(metadata$started_at),
    producer = producer,
    schemaURL = "https://openlineage.io/spec/2-0-2/OpenLineage.json#/$defs/RunEvent",
    eventType = "START",
    run = list(runId = lineage_uuid(metadata$run_id)),
    job = list(namespace = catalog$namespace, name = metadata$product),
    inputs = inputs,
    outputs = list()
  )
  end <- event
  end$eventTime <- event_time(metadata$finished_at)
  end$eventType <- if (successful) "COMPLETE" else "FAIL"
  if (successful) {
    output <- dataset(
      metadata$product,
      metadata$schema,
      metadata$outputs %||% list()
    )
    lineage <- metadata$column_lineage
    if (
      isTRUE(lineage$complete) && length(inputs) == 1L && length(lineage$fields)
    ) {
      output$facets$columnLineage <- list(
        `_producer` = producer,
        `_schemaURL` = "https://openlineage.io/spec/facets/1-2-0/ColumnLineageDatasetFacet.json",
        fields = lapply(lineage$fields, function(fields) {
          list(
            inputFields = unname(lapply(fields, function(field) {
              list(
                namespace = inputs[[1]]$namespace,
                name = inputs[[1]]$name,
                field = field
              )
            }))
          )
        })
      )
    }
    end$outputs <- list(output)
  }
  list(event, end)
}


lineage_uuid <- function(run_id) {
  rlang::local_error_call(rlang::caller_env())
  if (
    grepl(
      "^[[:xdigit:]]{8}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{12}$",
      run_id
    )
  ) {
    return(tolower(run_id))
  }
  hash <- digest::digest(
    paste0("dataraft/run/", run_id),
    algo = "sha256",
    serialize = FALSE
  )
  paste(
    substr(hash, 1, 8),
    substr(hash, 9, 12),
    paste0("8", substr(hash, 14, 16)),
    paste0("8", substr(hash, 18, 20)),
    substr(hash, 21, 32),
    sep = "-"
  )
}


event_time <- function(value) {
  rlang::local_error_call(rlang::caller_env())
  dataraft.core::dr_internal_scalar(value, "Event timestamp")
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?Z$", value)) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_catalog",
      "Event timestamps must be ISO 8601 UTC strings ending in Z."
    )
  }
  value
}


catalog_endpoint <- function(endpoint) {
  rlang::local_error_call(rlang::caller_env())
  dataraft.core::dr_internal_scalar(endpoint, "endpoint")
  if (
    !grepl("^https?://[^/]+", endpoint) ||
      grepl("[?#]", endpoint) ||
      grepl("^https?://[^/]*@", endpoint)
  ) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_catalog",
      "Use an HTTP(S) endpoint without credentials, query or fragment; configure authentication with request."
    )
  }
  invisible(endpoint)
}


check_catalog_request <- function(request) {
  rlang::local_error_call(rlang::caller_env())
  if (
    !is.null(request) &&
      !is.function(request) &&
      !inherits(request, "httr2_request")
  ) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_catalog",
      "request must be NULL, an httr2 request or a request factory."
    )
  }
  invisible(request)
}


catalog_request <- function(endpoint, configure) {
  rlang::local_error_call(rlang::caller_env())
  dataraft.core::dr_internal_need("httr2")
  request <- httr2::request(endpoint)
  if (inherits(configure, "httr2_request")) {
    request <- configure
  }
  if (is.function(configure)) {
    request <- if (length(formals(configure))) {
      configure(request)
    } else {
      configure()
    }
  }
  if (!inherits(request, "httr2_request")) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_catalog",
      "The catalog request factory must return an httr2 request."
    )
  }
  httr2::req_timeout(httr2::req_url(request, endpoint), 30)
}


catalog_capabilities <- function(backend, all_statuses = FALSE) {
  rlang::local_error_call(rlang::caller_env())
  list(
    backend = backend,
    read = FALSE,
    write = FALSE,
    lazy = FALSE,
    transactions = FALSE,
    partition = FALSE,
    immutable = FALSE,
    metadata = TRUE,
    delivery = "at-least-once",
    statuses = if (all_statuses) {
      c("completed", "published", "cached", "blocked", "error", "missing")
    } else {
      c("completed", "published", "cached")
    }
  )
}


openlineage_check_identity <- function(identity) {
  if (
    !is.list(identity) || !setequal(names(identity), c("namespace", "name"))
  ) {
    dataraft.core::dr_internal_abort(
      "Dataset identity needs namespace and name.",
      subclass = "dataraft_error_catalog"
    )
  }
  for (field in c("namespace", "name")) {
    value <- identity[[field]]
    dataraft.core::dr_internal_scalar(value, paste("Dataset", field))
    if (grepl("[?#]", value) || grepl("://[^/]*@", value)) {
      dataraft.core::dr_internal_abort(
        "Dataset identities must not contain credentials, queries or fragments.",
        subclass = "dataraft_error_catalog"
      )
    }
  }
  invisible(identity)
}

openlineage_check_datasets <- function(datasets) {
  if (
    !is.list(datasets) ||
      (length(datasets) &&
        (is.null(names(datasets)) ||
          anyNA(names(datasets)) ||
          any(!nzchar(names(datasets))) ||
          anyDuplicated(names(datasets))))
  ) {
    dataraft.core::dr_internal_abort(
      "datasets must be a list uniquely named by logical asset or source ID.",
      subclass = "dataraft_error_catalog"
    )
  }
  invisible(lapply(datasets, openlineage_check_identity))
}
