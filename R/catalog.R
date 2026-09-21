#' Assess delivery freshness independently of the last processing attempt
#' @param lake Connected lake.
#' @param at Evaluation time.
#' @return Tibble of published assets with latest attempt and freshness.
#' @export
#' @examplesIf requireNamespace("dataraft.lake", quietly = TRUE) && requireNamespace("duckdb", quietly = TRUE)
#' root <- tempfile("dataraft-example-")
#' config <- dataraft.lake::dr_lake_config(
#'   dataraft.lake::dr_registry_duckdb(file.path(root, "lake.db")),
#'   dataraft.lake::dr_storage_local(file.path(root, "data")),
#'   landing = file.path(root, "landing"), backend = "duckdb"
#' )
#' lake <- dataraft.lake::dr_connect_lake(config)
#' path <- file.path(root, "orders.csv")
#' utils::write.csv(data.frame(order_id = 1:2, amount = c(25, 75)), path,
#'   row.names = FALSE)
#' source <- dataraft.core::dr_source_file("orders.file", path, reader = utils::read.csv)
#' contract <- dataraft.core::dr_contract(
#'   "orders", "1.0.0", "Analytics", "Order amounts", "One order",
#'   c(order_id = "integer", amount = "numeric"), key = "order_id"
#' )
#' release <- dataraft.core::dr_product("orders", contract = contract, code_version = "v1") |>
#'   dataraft.core::dr_add_source(source) |> dataraft.core::dr_publish(to = lake)
#' dr_freshness(lake)
#' dataraft.lake::dr_disconnect_lake(lake)
#' unlink(root, recursive = TRUE)
dr_freshness <- function(lake, at = Sys.time()) {
  catalog_summary(
    lapply(c("assets", "releases", "runs", "events"), function(n) {
      optional_lake("dr_registry")(lake, n)
    }) |>
      stats::setNames(c("assets", "releases", "runs", "events")),
    at
  )
}


catalog_summary <- function(snapshot, at = Sys.time()) {
  rlang::local_error_call(rlang::caller_env())
  releases <- snapshot$releases
  runs <- snapshot$runs
  assets <- snapshot$assets
  events <- snapshot$events
  deliveries <- if (NROW(events)) {
    events[
      events$type %in%
        c("delivery_overdue", "delivery_received"),
    ]
  } else {
    tibble::tibble(
      asset = character(),
      type = character(),
      created_at = character()
    )
  }
  ids <- union(union(releases$asset, runs$asset), deliveries$asset)
  if (!length(ids)) {
    return(tibble::tibble(
      asset = character(),
      release_id = character(),
      published_at = character(),
      published_quality = character(),
      freshness = character(),
      latest_attempt = character(),
      age_hours = double(),
      max_age_hours = double(),
      delivery_status = character()
    ))
  }
  dplyr::bind_rows(lapply(ids, function(id) {
    expected <- if (nrow(deliveries)) {
      deliveries[deliveries$asset == id, ]
    } else {
      deliveries
    }
    expected <- if (nrow(expected)) {
      expected[order(expected$created_at, decreasing = TRUE), ]
    } else {
      expected
    }
    delivery_status <- if (!nrow(expected)) {
      "not_monitored"
    } else if (expected$type[[1]] == "delivery_received") {
      "received"
    } else {
      "missing"
    }
    rr <- releases[releases$asset == id, ]
    rr <- rr[order(rr$published_at, decreasing = TRUE), ]
    attempts <- runs[runs$asset == id, ]
    attempts <- attempts[order(attempts$started_at, decreasing = TRUE), ]
    latest <- if (nrow(attempts)) attempts$status[[1]] else "not_checked"
    if (!nrow(rr)) {
      return(tibble::tibble(
        asset = id,
        release_id = NA_character_,
        published_at = NA_character_,
        published_quality = "not_checked",
        freshness = "missing",
        latest_attempt = latest,
        age_hours = NA_real_,
        max_age_hours = NA_real_,
        delivery_status = delivery_status
      ))
    }
    r <- rr[1, ]
    parts <- strsplit(r$contract[[1]], "@", fixed = TRUE)[[1]]
    contract <- assets[
      assets$kind == "contract" &
        assets$id == parts[1] &
        assets$version == parts[2],
    ]
    max_age <- if (nrow(contract)) {
      jdecode(contract$definition[[1]])$max_age_hours %||%
        NA_real_
    } else {
      NA_real_
    }
    age <- as.numeric(difftime(
      at,
      as.POSIXct(
        r$published_at[[1]],
        format = "%Y-%m-%dT%H:%M:%OSZ",
        tz = "UTC"
      ),
      units = "hours"
    ))
    tibble::tibble(
      asset = id,
      release_id = r$release_id[[1]],
      published_at = r$published_at[[1]],
      published_quality = r$quality[[1]],
      freshness = if (is.na(max_age)) {
        "unknown"
      } else if (age > max_age) {
        "stale"
      } else {
        "current"
      },
      latest_attempt = latest,
      age_hours = age,
      max_age_hours = max_age,
      delivery_status = delivery_status
    )
  }))
}


#' Export a read-only metadata snapshot
#' @param lake Connected lake.
#' @param path JSON output path.
#' @return The normalized path, invisibly. No row data or credentials are
#'   exported.
#' @export
#' @examplesIf requireNamespace("dataraft.lake", quietly = TRUE) && requireNamespace("duckdb", quietly = TRUE)
#' root <- tempfile("dataraft-example-")
#' config <- dataraft.lake::dr_lake_config(
#'   dataraft.lake::dr_registry_duckdb(file.path(root, "lake.db")),
#'   dataraft.lake::dr_storage_local(file.path(root, "data")),
#'   landing = file.path(root, "landing"), backend = "duckdb"
#' )
#' lake <- dataraft.lake::dr_connect_lake(config)
#' dr_catalog_export(lake, file.path(root, "catalog.json"))
#' dataraft.lake::dr_disconnect_lake(lake)
#' unlink(root, recursive = TRUE)
dr_catalog_export <- function(lake, path) {
  # Reports include values and are intentionally excluded from catalog exports.
  names <- c(
    "assets",
    "runs",
    "inputs",
    "quality_results",
    "releases",
    "lineage_edges",
    "events"
  )
  snapshot <- stats::setNames(
    lapply(names, function(n) optional_lake("dr_registry")(lake, n)),
    names
  )
  snapshot$exported_at <- now()
  # Canonical definition JSON remains a string; nested scalar types are preserved.
  tmp <- tempfile(".catalog-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(
    snapshot,
    tmp,
    dataframe = "rows",
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null",
    null = "null"
  )
  if (!file.rename(tmp, path)) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_catalog",
      "Unable to publish catalog snapshot; previous file retained."
    )
  }
  invisible(normalizePath(path))
}


#' Start a read-only data catalog
#' @param lake Connected lake, or NULL when reading an exported snapshot.
#' @param snapshot Path to JSON exported by dr_catalog_export().
#' @param launch Start the app; FALSE returns a shiny.appobj for Connect.
#' @param refresh_seconds Metadata refresh interval.
#' @return A Shiny app object (when launch = FALSE).
#' @export
#' @examplesIf requireNamespace("dataraft.lake", quietly = TRUE) && requireNamespace("duckdb", quietly = TRUE)
#' root <- tempfile("dataraft-example-")
#' config <- dataraft.lake::dr_lake_config(
#'   dataraft.lake::dr_registry_duckdb(file.path(root, "lake.db")),
#'   dataraft.lake::dr_storage_local(file.path(root, "data")),
#'   landing = file.path(root, "landing"), backend = "duckdb"
#' )
#' lake <- dataraft.lake::dr_connect_lake(config)
#' app <- dr_catalog_app(lake, launch = FALSE)
#' class(app)
#' dataraft.lake::dr_disconnect_lake(lake)
#' unlink(root, recursive = TRUE)
dr_catalog_app <- function(
  lake = NULL,
  snapshot = NULL,
  launch = interactive(),
  refresh_seconds = 30
) {
  dataraft.core::dr_internal_need("shiny")
  dataraft.core::dr_internal_need("bslib")
  if (is.null(lake) == is.null(snapshot)) {
    dataraft.core::dr_internal_abort(
      subclass = "dataraft_error_catalog",
      "Supply exactly one of lake or snapshot."
    )
  }
  read_snapshot <- function() {
    rlang::local_error_call(rlang::caller_env())
    if (!is.null(snapshot)) {
      x <- jsonlite::read_json(snapshot, simplifyVector = TRUE)
      for (n in c(
        "assets",
        "runs",
        "inputs",
        "quality_results",
        "releases",
        "lineage_edges",
        "events"
      )) {
        if (is.null(x[[n]]) || !length(x[[n]])) {
          x[[n]] <- tibble::tibble()
        } else {
          x[[n]] <- tibble::as_tibble(x[[n]])
        }
      }
      x
    } else {
      names <- c(
        "assets",
        "runs",
        "inputs",
        "quality_results",
        "releases",
        "lineage_edges",
        "events"
      )
      stats::setNames(
        lapply(names, function(n) optional_lake("dr_registry")(lake, n)),
        names
      )
    }
  }
  ui <- bslib::page_sidebar(
    title = "dataraft | Data Catalog",
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#147d74"
    ),
    sidebar = bslib::sidebar(
      shiny::p("Find trusted datasets, contracts and metrics."),
      shiny::textInput(
        "search",
        "Search",
        placeholder = "Asset, owner, metric..."
      ),
      shiny::selectInput("asset", "Dataset / product", choices = character()),
      shiny::selectInput(
        "definition_id",
        "Contract / product / metric",
        choices = character()
      ),
      shiny::tags$small(
        "Dataset and run lineage. Custom R internals are not inferred."
      ),
      shiny::textOutput("snapshot_time")
    ),
    bslib::navset_card_tab(
      bslib::nav_panel("Overview", shiny::tableOutput("overview")),
      bslib::nav_panel(
        "Definitions",
        shiny::tableOutput("definitions"),
        shiny::verbatimTextOutput("definition")
      ),
      bslib::nav_panel(
        "Quality",
        shiny::p("Latest attempt and published release are shown separately."),
        shiny::tableOutput("quality")
      ),
      bslib::nav_panel(
        "Releases",
        shiny::tableOutput("releases"),
        shiny::h5("Use this product in R"),
        shiny::verbatimTextOutput("usage")
      ),
      bslib::nav_panel(
        "Lineage",
        shiny::uiOutput("lineage_graph"),
        shiny::tableOutput("lineage")
      ),
      bslib::nav_panel("Runs", shiny::tableOutput("runs")),
      bslib::nav_panel("Notifications", shiny::tableOutput("events"))
    )
  )
  server <- function(input, output, session) {
    rlang::local_error_call(rlang::caller_env())
    state <- shiny::reactive({
      shiny::invalidateLater(refresh_seconds * 1000, session)
      read_snapshot()
    })
    summary <- shiny::reactive(catalog_summary(state()))
    shiny::observe({
      choices <- summary()$asset
      picked <- input$asset %||% ""
      selected <- if (picked %in% choices) {
        picked
      } else if (length(choices)) {
        choices[[1]]
      } else {
        character()
      }
      shiny::updateSelectInput(
        session,
        "asset",
        choices = choices,
        selected = selected
      )
      definitions <- state()$assets
      options <- if (nrow(definitions)) {
        paste(definitions$id, definitions$version, sep = "@")
      } else {
        character()
      }
      selected_def <- input$definition_id %||% ""
      shiny::updateSelectInput(
        session,
        "definition_id",
        choices = options,
        selected = if (selected_def %in% options) {
          selected_def
        } else if (length(options)) {
          options[[1]]
        } else {
          character()
        }
      )
    })
    searched <- function(data) {
      rlang::local_error_call(rlang::caller_env())
      search <- input$search %||% ""
      if (!nrow(data) || !nzchar(search)) {
        return(data)
      }
      data[
        apply(as.data.frame(data), 1, function(row) {
          any(grepl(tolower(search), tolower(row), fixed = TRUE), na.rm = TRUE)
        }),
        ,
        drop = FALSE
      ]
    }
    selected <- function(table, column = "asset") {
      rlang::local_error_call(rlang::caller_env())
      x <- state()[[table]]
      if (!nrow(x)) {
        return(x)
      }
      if (nzchar(input$asset %||% "")) {
        x <- x[x[[column]] == input$asset, , drop = FALSE]
      }
      searched(x)
    }
    output$overview <- shiny::renderTable(
      searched(summary()),
      striped = TRUE,
      digits = 1
    )
    output$definitions <- shiny::renderTable(
      {
        x <- state()$assets
        if (!nrow(x)) {
          return(x)
        }
        searched(x[, c("id", "version", "kind", "owner", "description")])
      },
      striped = TRUE
    )
    output$definition <- shiny::renderText({
      x <- state()$assets
      if (!nrow(x)) {
        return("No definitions registered.")
      }
      chosen <- input$definition_id %||% ""
      if (nzchar(chosen)) {
        return(paste(
          vapply(
            x$definition[paste(x$id, x$version, sep = "@") == chosen],
            jsonlite::prettify,
            character(1)
          ),
          collapse = "\n"
        ))
      }
      defs <- x$definition[x$id == (input$asset %||% "")]
      r <- state()$releases
      if (nrow(r)) {
        contracts <- unique(r$contract[r$asset == (input$asset %||% "")])
        defs <- c(
          defs,
          x$definition[paste(x$id, x$version, sep = "@") %in% contracts]
        )
      }
      paste(vapply(defs, jsonlite::prettify, character(1)), collapse = "\n\n")
    })
    output$releases <- shiny::renderTable(selected("releases"), striped = TRUE)
    output$runs <- shiny::renderTable(selected("runs"), striped = TRUE)
    output$events <- shiny::renderTable(selected("events"), striped = TRUE)
    output$quality <- shiny::renderTable(
      {
        q <- state()$quality_results
        runs <- selected("runs")
        if (!nrow(q) || !nrow(runs)) {
          return(tibble::tibble())
        }
        q[q$run_id %in% runs$run_id, , drop = FALSE]
      },
      striped = TRUE
    )
    output$lineage <- shiny::renderTable(
      {
        x <- state()$lineage_edges
        if (!nrow(x)) {
          return(x)
        }
        if (nzchar(input$asset %||% "")) {
          x <- x[
            x$from_id == input$asset | x$to_id == input$asset,
            ,
            drop = FALSE
          ]
        }
        searched(unique(x))
      },
      striped = TRUE
    )
    output$lineage_graph <- shiny::renderUI({
      edges <- state()$lineage_edges
      if (!nrow(edges)) {
        return(shiny::p("No lineage recorded."))
      }
      edges <- unique(edges[, c("from_id", "to_id")])
      nodes <- unique(c(edges$from_id, edges$to_id))
      if (length(nodes) > 24) {
        return(shiny::p(
          "Graph omitted above 24 assets; use the versioned lineage table below."
        ))
      }
      # Declared dataset-level graph; versions remain in the table below.
      ranks <- stats::setNames(rep(0L, length(nodes)), nodes)
      for (iteration in seq_along(nodes)) {
        next_rank <- ranks
        for (i in seq_len(nrow(edges))) {
          next_rank[edges$to_id[i]] <- max(
            next_rank[edges$to_id[i]],
            ranks[edges$from_id[i]] + 1L
          )
        }
        if (identical(next_rank, ranks)) {
          break
        }
        ranks <- next_rank
      }
      if (max(ranks) >= length(nodes)) {
        return(shiny::p(
          "Declared lineage contains a cycle; inspect the table."
        ))
      }
      positions <- lapply(nodes, function(n) {
        peers <- names(ranks)[ranks == ranks[[n]]]
        c(x = 200 + (match(n, peers) - 1) * 360, y = 40 + ranks[[n]] * 110)
      })
      names(positions) <- nodes
      width <- max(vapply(positions, `[[`, numeric(1), "x")) + 200
      height <- max(vapply(positions, `[[`, numeric(1), "y")) + 55
      lines <- lapply(seq_len(nrow(edges)), function(i) {
        a <- positions[[edges$from_id[i]]]
        b <- positions[[edges$to_id[i]]]
        shiny::tags$line(
          x1 = a[["x"]],
          y1 = a[["y"]] + 24,
          x2 = b[["x"]],
          y2 = b[["y"]] - 26,
          stroke = "#8ca5a2",
          `stroke-width` = 2,
          `marker-end` = "url(#dl-arrow)"
        )
      })
      boxes <- lapply(nodes, function(n) {
        p <- positions[[n]]
        shiny::tags$g(
          shiny::tags$rect(
            x = p[["x"]] - 165,
            y = p[["y"]] - 24,
            width = 330,
            height = 48,
            rx = 8,
            fill = if (identical(n, input$asset)) "#d9eee9" else "#f0f5f4",
            stroke = "#147d74"
          ),
          shiny::tags$text(
            x = p[["x"]],
            y = p[["y"]] + 5,
            `text-anchor` = "middle",
            fill = "#163b36",
            `font-size` = 13,
            n
          )
        )
      })
      shiny::tags$div(
        style = "overflow-x:auto;",
        shiny::tags$svg(
          xmlns = "http://www.w3.org/2000/svg",
          viewBox = paste(0, 0, width, height),
          width = "100%",
          style = "min-width:650px;max-height:700px;",
          role = "img",
          `aria-label` = "Declared dataset lineage",
          shiny::tags$defs(shiny::tags$marker(
            id = "dl-arrow",
            viewBox = "0 0 10 10",
            refX = 9,
            refY = 5,
            markerWidth = 6,
            markerHeight = 6,
            orient = "auto-start-reverse",
            shiny::tags$path(d = "M 0 0 L 10 5 L 0 10 z", fill = "#8ca5a2")
          )),
          lines,
          boxes
        )
      )
    })
    output$usage <- shiny::renderText({
      x <- selected("releases")
      if (!nrow(x)) {
        return("No published data.")
      }
      x <- x[order(x$published_at, decreasing = TRUE), ]
      paste0(
        'dr_tbl(lake, "',
        x$asset[[1]],
        '", release = "',
        x$release_id[[1]],
        '") |>\n  dplyr::collect()'
      )
    })
    output$snapshot_time <- shiny::renderText(paste(
      "Metadata:",
      state()$exported_at %||% "live connection"
    ))
  }
  app <- shiny::shinyApp(ui, server)
  if (isTRUE(launch)) shiny::runApp(app) else app
}
