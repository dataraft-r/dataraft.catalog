# Test bindings for this package; unavailable optional packages are not loaded.
family_owners <- c(
  "dr_capabilities" = "dataraft.core",
  "dr_component_capabilities" = "dataraft.core",
  "dr_catalog_openmetadata_dbt" = "dataraft.catalog",
  "dbt_artifact_hashes" = "dataraft.dbt",
  "dbt_catalog_config" = "dataraft.catalog",
  "dbt_catalog_process" = "dataraft.catalog",
  "dbt_catalog_write_receipt" = "dataraft.catalog",
  "dr_catalog_openlineage" = "dataraft.catalog",
  "dr_catalog_openmetadata" = "dataraft.catalog",
  "openmetadata_table" = "dataraft.catalog",
  "dr_check_component" = "dataraft.core",
  "dr_add_source" = "dataraft.core",
  "dr_add_quality" = "dataraft.core",
  "dr_set_target" = "dataraft.core",
  "dr_add_catalog" = "dataraft.core",
  "dr_inspect" = "dataraft.core",
  "dr_dbt_project" = "dataraft.dbt",
  "dr_dbt_build" = "dataraft.dbt",
  "dr_dbt_test" = "dataraft.dbt",
  "dbt_process" = "dataraft.dbt",
  "dbt_read_artifacts" = "dataraft.dbt",
  "dr_write_target" = "dataraft.core",
  "dr_publish_metadata" = "dataraft.core",
  "dr_collect" = "dataraft.core",
  "dr_run" = "dataraft.core",
  "dr_product" = "dataraft.core",
  "dr_run_history" = "dataraft.core",
  "dr_retry_catalogs" = "dataraft.core",
  "dr_disconnect_lake" = "dataraft.lake",
  "need" = "dataraft.core",
  "scalar" = "dataraft.core"
)
for (name in names(family_owners)) {
  owner <- family_owners[[name]]
  if (requireNamespace(owner, quietly = TRUE)) {
    assign(name, get(name, asNamespace(owner), inherits = FALSE))
  }
}
local_family_bindings <- function(..., .package = NULL, .env = parent.frame()) {
  bindings <- list(...)
  if (
    !is.null(.package) && !.package %in% c("dataraft", unique(family_owners))
  ) {
    return(do.call(
      testthat::local_mocked_bindings,
      c(bindings, list(.package = .package, .env = .env))
    ))
  }
  owners <- unname(family_owners[names(bindings)])
  if (anyNA(owners)) {
    stop("Unknown mocked family binding")
  }
  for (owner in unique(owners)) {
    do.call(
      testthat::local_mocked_bindings,
      c(bindings[owners == owner], list(.package = owner, .env = .env))
    )
  }
}
