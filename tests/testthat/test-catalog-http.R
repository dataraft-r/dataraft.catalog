catalog_fixture <- function(env = parent.frame()) {
  skip_if_not_installed("httr2")
  skip_if_not_installed("webfakes")
  path <- withr::local_tempdir(.local_envir = env)
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  handler <- local({
    directory <- path
    function(req, res) {
      calls <- list.files(directory, pattern = "^request-")
      id <- length(calls) + 1L
      jsonlite::write_json(
        list(
          method = toupper(req$method),
          path = req$path,
          body = req$json,
          authorization = req$get_header("Authorization")
        ),
        file.path(directory, paste0("request-", id, ".json")),
        auto_unbox = TRUE
      )
      if (file.exists(file.path(directory, "fail"))) {
        res$set_status(503L)$send_json(
          list(error = "fixture unavailable"),
          auto_unbox = TRUE
        )
      } else {
        res$send_json(
          list(
            id = if (grepl("/name/", req$path)) {
              "00000000-0000-4000-8000-000000000001"
            } else {
              "00000000-0000-4000-8000-000000000002"
            }
          ),
          auto_unbox = TRUE
        )
      }
    }
  })
  app$post("/api/v1/lineage", handler)
  app$put("/api/v1/lineage", handler)
  app$put("/api/v1/tables", handler)
  app$get("/api/v1/tables/name/warehouse.raw.orders", handler)
  process <- webfakes::new_app_process(app)
  withr::defer(process$stop(), envir = env)
  list(path = path, url = sub("/$", "", process$url()), calls = function() {
    lapply(
      list.files(path, pattern = "^request-", full.names = TRUE),
      jsonlite::read_json,
      simplifyVector = FALSE
    )
  })
}

test_that("OpenLineage sends authenticated START and COMPLETE with stable identity", {
  server <- catalog_fixture()
  adapter <- dr_catalog_openlineage(
    paste0(server$url, "/api/v1/lineage"),
    request = function(request) {
      httr2::req_auth_bearer_token(request, "test-token")
    }
  )
  result <- dr_product("orders") |>
    dr_add_source(data.frame(id = 1:2)) |>
    dr_add_catalog(adapter) |>
    dr_run()
  calls <- server$calls()
  expect_length(calls, 2L)
  expect_equal(vapply(calls, `[[`, character(1), "method"), rep("POST", 2))
  expect_equal(
    vapply(calls, function(x) x$body$eventType, character(1)),
    c("START", "COMPLETE")
  )
  expect_equal(calls[[1]]$authorization, "Bearer test-token")
  expect_equal(calls[[1]]$body$run$runId, calls[[2]]$body$run$runId)
  expect_match(
    calls[[2]]$body$run$runId,
    "^[[:xdigit:]]{8}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{12}$"
  )
  expect_equal(
    calls[[2]]$body$outputs[[1]]$facets$schema$fields[[1]]$name,
    "id"
  )
  expect_equal(calls[[1]]$body$eventTime, result$started_at)
  expect_false(grepl("test-token", jsonlite::toJSON(dr_inspect(adapter))))
})

test_that("catalog outages preserve data and retry the same lineage identity", {
  server <- catalog_fixture()
  evidence <- withr::local_tempdir()
  writeLines("fail", file.path(server$path, "fail"))
  adapter <- dr_catalog_openlineage(paste0(server$url, "/api/v1/lineage"))
  expect_warning(
    result <- dr_product("orders") |>
      dr_add_source(data.frame(id = 1L)) |>
      dr_add_catalog(adapter) |>
      dr_run(evidence = evidence),
    "delivery failed"
  )
  expect_equal(result$status, "completed")
  first_id <- server$calls()[[1]]$body$run$runId
  expect_equal(dr_run_history(evidence)$pending_catalogs, 1)
  unlink(file.path(server$path, "fail"))
  dr_retry_catalogs(evidence, adapter)
  calls <- server$calls()
  expect_length(calls, 3L)
  expect_equal(calls[[3]]$body$run$runId, first_id)
  expect_equal(dr_run_history(evidence)$pending_catalogs, 0)
  dr_retry_catalogs(evidence, adapter)
  expect_length(server$calls(), 3L)
})

test_that("a committed output stays published when metadata delivery fails", {
  server <- catalog_fixture()
  writeLines("fail", file.path(server$path, "fail"))
  output <- file.path(server$path, "published.rds")
  methods <- list(
    dr_write_target = function(target, data, context, ...) {
      saveRDS(data, target$path)
      list(type = "fixture", path = target$path)
    },
    dr_check_component = function(x, ...) invisible(x)
  )
  namespace <- asNamespace("dataraft.core")
  table <- get(".__S3MethodsTable__.", envir = namespace)
  for (generic in names(methods)) {
    registerS3method(
      generic,
      "catalog_fixture_target",
      methods[[generic]],
      envir = namespace
    )
  }
  withr::defer(rm(
    list = paste0(names(methods), ".catalog_fixture_target"),
    envir = table
  ))
  target <- structure(list(path = output), class = "catalog_fixture_target")
  expect_warning(
    result <- dr_product("orders") |>
      dr_add_source(data.frame(id = 1L)) |>
      dr_set_target(target) |>
      dr_add_catalog(dr_catalog_openlineage(paste0(
        server$url,
        "/api/v1/lineage"
      ))) |>
      dr_run(),
    "delivery failed"
  )
  expect_equal(result$status, "published")
  expect_equal(readRDS(output)$id, 1L)
  expect_equal(dr_collect(result)$id, 1L)
})

test_that("blocked runs emit FAIL and never claim output datasets", {
  server <- catalog_fixture()
  result <- dr_product("orders") |>
    dr_add_source(data.frame(id = 1L)) |>
    dr_add_quality(~ id < 0) |>
    dr_add_catalog(dr_catalog_openlineage(paste0(
      server$url,
      "/api/v1/lineage"
    ))) |>
    dr_run(stop_on_failure = FALSE)
  calls <- server$calls()
  expect_equal(result$status, "blocked")
  expect_equal(calls[[2]]$body$eventType, "FAIL")
  expect_length(calls[[2]]$body$outputs, 0L)
})

test_that("OpenMetadata upserts typed schemas and resolves optional lineage", {
  server <- catalog_fixture()
  adapter <- dr_catalog_openmetadata(
    server$url,
    "warehouse.analytics.public",
    source_tables = c(raw = "warehouse.raw.orders")
  )
  result <- dr_product("orders") |>
    dr_add_source(
      data.frame(
        id = 1L,
        amount = 1.5,
        valid = TRUE,
        day = as.Date("2026-01-01"),
        label = "a"
      ),
      name = "raw"
    ) |>
    dr_add_catalog(adapter) |>
    dr_run()
  calls <- server$calls()
  expect_length(calls, 3L)
  expect_equal(calls[[1]]$method, "PUT")
  expect_equal(calls[[1]]$path, "/api/v1/tables")
  expect_equal(calls[[1]]$body$databaseSchema, "warehouse.analytics.public")
  expect_equal(
    vapply(calls[[1]]$body$columns, `[[`, character(1), "dataType"),
    c("INT", "DOUBLE", "BOOLEAN", "DATE", "STRING")
  )
  expect_equal(
    calls[[3]]$body$edge$fromEntity$id,
    "00000000-0000-4000-8000-000000000001"
  )
  expect_equal(
    calls[[3]]$body$edge$toEntity$id,
    "00000000-0000-4000-8000-000000000002"
  )
  expect_equal(result$status, "completed")
})
