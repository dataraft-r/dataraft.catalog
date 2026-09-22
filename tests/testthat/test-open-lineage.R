test_that("OpenLineage includes known direct field dependencies only", {
  catalog <- dr_catalog_openlineage(
    "https://example.com/events",
    namespace = "test"
  )
  meta <- list(
    run_id = "run",
    product = "orders",
    status = "completed",
    started_at = "2026-01-01T00:00:00Z",
    finished_at = "2026-01-01T00:00:01Z",
    schema = c(net = "numeric"),
    inputs = list(list(name = "source", source = list(id = "gross"))),
    column_lineage = list(complete = TRUE, fields = list(net = "amount"))
  )
  events <- openlineage_events(catalog, meta)
  expect_equal(
    events[[2]]$outputs[[1]]$facets$columnLineage$fields$net$inputFields[[
      1
    ]]$field,
    "amount"
  )
  meta$column_lineage$complete <- FALSE
  expect_null(
    openlineage_events(catalog, meta)[[2]]$outputs[[1]]$facets$columnLineage
  )
})

test_that("OpenMetadata publishes descriptive governance with explicit references", {
  catalog <- dr_catalog_openmetadata("https://example.com", "warehouse.public")
  metadata <- list(
    product = "orders",
    schema = c(id = "integer"),
    contract = list(
      governance = list(
        retention = "P7Y",
        classification = "PII.Sensitive",
        glossary = "Finance.Order",
        openmetadata_owners = list(list(id = "owner-id", type = "team"))
      ),
      column_metadata = list(
        id = list(
          classification = "PII.Sensitive",
          description = "Customer identifier"
        )
      )
    )
  )
  table <- openmetadata_table(catalog, metadata)
  expect_equal(table$retentionPeriod, "P7Y")
  expect_equal(table$owners[[1]]$id, "owner-id")
  expect_equal(table$tags[[1]]$tagFQN, "PII.Sensitive")
  expect_equal(table$tags[[2]]$source, "Glossary")
  expect_equal(table$columns[[1]]$tags[[1]]$tagFQN, "PII.Sensitive")
})


test_that("physical dataset identities are independent from job namespaces", {
  identity <- list(
    namespace = "postgres://warehouse:5432",
    name = "insurance.public.orders"
  )
  catalog <- dr_catalog_openlineage(
    "https://example.com/events",
    namespace = "orchestrator",
    datasets = list(orders = identity)
  )
  meta <- list(
    run_id = "run",
    product = "orders",
    status = "completed",
    started_at = "2026-01-01T00:00:00Z",
    finished_at = "2026-01-01T00:00:01Z",
    inputs = list(list(source = list(id = "raw", dataset = identity)))
  )
  event <- openlineage_events(catalog, meta)[[2]]
  expect_identical(event$job$namespace, "orchestrator")
  expect_identical(event$outputs[[1]]$namespace, identity$namespace)
  expect_identical(event$outputs[[1]]$name, identity$name)
  expect_identical(event$inputs[[1]]$name, identity$name)
  expect_identical(
    event$outputs[[1]]$facets$dataraft_identity$logicalId,
    "orders"
  )
  expect_identical(
    event$outputs[[1]]$facets$dataraft_identity$physicalIdentity,
    TRUE
  )
  meta$product <- "unlocated"
  output <- openlineage_events(catalog, meta)[[2]]$outputs[[1]]
  expect_identical(output$namespace, "dataraft://orchestrator")
  expect_identical(output$facets$dataraft_identity$physicalIdentity, FALSE)
})

test_that("physical dataset identities reject secret-bearing locations", {
  error <- tryCatch(
    dr_catalog_openlineage(
      "https://example.com/events",
      datasets = list(
        raw = list(
          namespace = "postgres://user:password@host",
          name = "db.public.raw"
        )
      )
    ),
    error = identity
  )
  expect_s3_class(error, "dataraft_error_catalog")
  expect_equal(grepl("password", conditionMessage(error)), FALSE)
})
