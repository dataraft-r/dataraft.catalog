#' Publish a table definition to OpenMetadata
#'
#' Upserts a table with `PUT /api/v1/tables`. The database service, database
#' and schema must already exist in OpenMetadata. Supported R types are mapped
#' explicitly to OpenMetadata column types; unsupported types fail before any
#' request. Descriptions come from the product or contract. This adapter does
#' not provision infrastructure, transfer data rows, create users, or implement
#' OpenMetadata's complete governance API.
#'
#' Optional `source_tables` maps source names to existing OpenMetadata table
#' fully qualified names. The adapter resolves their IDs and adds table-level
#' lineage with `PUT /api/v1/lineage`. Unmapped inputs are omitted; column-level
#' lineage is not inferred. Repeating table upserts and lineage edges is safe
#' after an interrupted delivery. Only successful runs publish table metadata.
#' @param endpoint OpenMetadata server base URL, without `/api/v1`.
#' @param database_schema Fully qualified existing database schema, for example
#'   `warehouse.analytics.public`.
#' @param request Optional authenticated httr2 request or request factory,
#'   as described in [dr_catalog_openlineage()].
#' @param source_tables Optional named character vector of source-name to
#'   OpenMetadata table fully qualified name mappings.
#' @returns A catalog adapter for [dataraft.core::dr_add_catalog()].
#' @seealso [dr_catalog_openlineage()], [dataraft.core::dr_retry_catalogs()]
#' @export
#' @examples
#' catalog <- dr_catalog_openmetadata("https://metadata.example",
#'   database_schema = "warehouse.analytics.public")
#' dataraft.core::dr_inspect(catalog)
dr_catalog_openmetadata <- function(
  endpoint,
  database_schema,
  request = NULL,
  source_tables = NULL
) {
  catalog_endpoint(endpoint)
  dataraft.core::dr_internal_scalar(database_schema, "database_schema")
  check_catalog_request(request)
  if (
    !is.null(source_tables) &&
      (!is.character(source_tables) ||
        is.null(names(source_tables)) ||
        anyNA(source_tables) ||
        any(!nzchar(source_tables)) ||
        anyNA(names(source_tables)) ||
        any(!nzchar(names(source_tables))) ||
        anyDuplicated(names(source_tables)))
  ) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_catalog",
      "source_tables must be a named character vector of source names and table fully qualified names."
    )
  }
  endpoint <- sub("/+$", "", endpoint)
  structure(
    list(
      id = paste0(
        "openmetadata-",
        substr(
          fingerprint(list(endpoint, database_schema)),
          1L,
          16L
        )
      ),
      endpoint = endpoint,
      database_schema = database_schema,
      request = request,
      source_tables = source_tables
    ),
    class = c("dr_openmetadata_catalog", "dr_catalog_adapter")
  )
}


#' @export
#' @importFrom dataraft.core dr_check_component
dr_check_component.dr_openmetadata_catalog <- function(x, ...) {
  dataraft.core::dr_internal_need("httr2")
  catalog_endpoint(x$endpoint)
  dataraft.core::dr_internal_scalar(x$database_schema, "database_schema")
  check_catalog_request(x$request)
  invisible(x)
}


#' @export
#' @importFrom dataraft.core dr_inspect
dr_inspect.dr_openmetadata_catalog <- function(x, ...) {
  list(
    type = "OpenMetadata",
    id = x$id,
    endpoint = x$endpoint,
    database_schema = x$database_schema,
    mapped_sources = names(x$source_tables)
  )
}


#' @export
#' @importFrom dataraft.core dr_capabilities
dr_capabilities.dr_openmetadata_catalog <- function(x, ...) {
  catalog_capabilities("OpenMetadata")
}


#' @export
#' @importFrom dataraft.core dr_publish_metadata
dr_publish_metadata.dr_openmetadata_catalog <- function(
  catalog,
  metadata,
  ...
) {
  dataraft.core::dr_check_component(catalog)
  if (!metadata$status %in% c("completed", "published", "cached")) {
    return(invisible(NULL))
  }
  table <- openmetadata_table(catalog, metadata)
  response <- openmetadata_request(catalog, "/api/v1/tables", "PUT", table)
  if (length(catalog$source_tables)) {
    output <- httr2::resp_body_json(response, simplifyVector = FALSE)
    if (is.null(output$id)) {
      dataraft.core::dr_internal_abort(
        subclass = "dataraft_error_catalog",
        "OpenMetadata did not return the table ID needed for lineage."
      )
    }
    inputs <- metadata$inputs
    if (is.data.frame(inputs)) {
      inputs <- dataraft.core::dr_internal_safe_descriptors(inputs)
    }
    input_names <- vapply(
      inputs,
      function(input) {
        input$name %||% input$source$id %||% input$id %||% ""
      },
      character(1)
    )
    for (name in intersect(input_names, names(catalog$source_tables))) {
      fqn <- catalog$source_tables[[name]]
      resolved <- openmetadata_request(
        catalog,
        paste0("/api/v1/tables/name/", utils::URLencode(fqn, reserved = TRUE)),
        "GET"
      )
      source <- httr2::resp_body_json(resolved, simplifyVector = FALSE)
      if (is.null(source$id)) {
        dataraft.core::dr_internal_abort(
          subclass = "dataraft_error_catalog",
          "OpenMetadata did not return the source table ID."
        )
      }
      openmetadata_request(
        catalog,
        "/api/v1/lineage",
        "PUT",
        list(
          edge = list(
            fromEntity = list(id = source$id, type = "table"),
            toEntity = list(id = output$id, type = "table")
          )
        )
      )
    }
  }
  invisible(NULL)
}


openmetadata_request <- function(catalog, path, method, body = NULL) {
  rlang::local_error_call(rlang::caller_env())
  request <- catalog_request(paste0(catalog$endpoint, path), catalog$request)
  if (!is.null(body)) {
    request <- httr2::req_body_json(request, body, auto_unbox = TRUE)
  }
  httr2::req_perform(httr2::req_method(request, method))
}


openmetadata_table <- function(catalog, metadata) {
  rlang::local_error_call(rlang::caller_env())
  schema <- metadata$schema
  if (
    !length(schema) || is.null(names(schema)) || anyDuplicated(names(schema))
  ) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_catalog",
      "OpenMetadata publication needs a non-empty named output schema."
    )
  }
  mapping <- c(
    character = "STRING",
    factor = "STRING",
    ordered = "STRING",
    integer = "INT",
    integer64 = "BIGINT",
    numeric = "DOUBLE",
    double = "DOUBLE",
    logical = "BOOLEAN",
    Date = "DATE",
    POSIXct = "TIMESTAMP",
    raw = "BINARY"
  )
  columns <- unname(lapply(seq_along(schema), function(i) {
    type <- schema[[i]]
    if (
      !is.character(type) || length(type) != 1L || !type %in% names(mapping)
    ) {
      dataraft.core::dr_internal_abort(
        subclass = "dataraft_error_catalog",
        paste0(
          "OpenMetadata cannot represent column `",
          names(schema)[[i]],
          "` with this R type. Transform it to a supported scalar column first."
        )
      )
    }
    list(
      name = names(schema)[[i]],
      dataType = unname(mapping[[type]]),
      ordinalPosition = i
    )
  }))
  result <- list(
    name = metadata$product,
    databaseSchema = catalog$database_schema,
    columns = columns,
    tableType = "Regular"
  )
  description <- metadata$definition$description %||%
    metadata$contract$description
  if (
    is.character(description) &&
      length(description) == 1L &&
      nzchar(description)
  ) {
    result$description <- description
  }
  governance <- metadata$contract$governance %||% list()
  labels <- function(values, source) {
    unname(lapply(as.character(unlist(values)), function(value) {
      list(
        tagFQN = value,
        source = source,
        labelType = "Manual",
        state = "Confirmed"
      )
    }))
  }
  tags <- c(
    labels(governance$tags, "Classification"),
    labels(governance$classification, "Classification"),
    labels(governance$glossary, "Glossary")
  )
  if (length(tags)) {
    result$tags <- tags
  }
  if (!is.null(governance$retention)) {
    result$retentionPeriod <- governance$retention
  }
  # Explicit references avoid guessing a user's ID from a display name.
  if (!is.null(governance$openmetadata_owners)) {
    result$owners <- governance$openmetadata_owners
  }
  for (i in seq_along(result$columns)) {
    name <- result$columns[[i]]$name
    info <- metadata$contract$column_metadata[[name]]
    if (!is.null(info$description)) {
      result$columns[[i]]$description <- info$description
    }
    if (!is.null(info$classification)) {
      result$columns[[i]]$tags <- labels(info$classification, "Classification")
    }
  }
  result
}
