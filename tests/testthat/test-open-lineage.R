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
