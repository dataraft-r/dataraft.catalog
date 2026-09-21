optional_dbt <- function(name) {
  dataraft.core::dr_internal_need("dataraft.dbt", "This catalog operation")
  getExportedValue("dataraft.dbt", name)
}

optional_lake <- function(name) {
  dataraft.core::dr_internal_need("dataraft.lake", "This catalog operation")
  getExportedValue("dataraft.lake", name)
}
