# dataraft.catalog 0.1.0.9005

* Deprecated compatibility facade: retirement is scheduled for 2027-01-01. Use dataraft.adapters.

# dataraft.catalog 0.1.0.9004

* Become a compatibility facade re-exporting catalog functions from dataraft.adapters. New projects should use dataraft.adapters.

* Use the umbrella CI manifest as the single immutable family dependency lock.

# dataraft.catalog 0.1.0.9000

* `dr_freshness()` reports unknown freshness for model releases without a versioned freshness contract, while preserving freshness checks for other assets in the same catalog.

* `dr_catalog_pane()` opens the catalog through Shiny's actual URL callback in an IDE viewer, with browser fallback and explicit foreground execution.

* `dr_freshness()` and catalog usage examples select the latest publication by registry sequence, preserving timestamp fallback only for legacy exported snapshots.

* `dr_catalog_openlineage()` separates job and dataset namespaces, accepts physical dataset identity mappings, and retains logical IDs in a custom facet. Unknown physical identities are explicitly marked logical.

* Publish direct-column maps as OpenLineage facets and governance fields to OpenMetadata. Improve contrast, labels, tab semantics and keyboard navigation.

* Keep stateless helpers private and prefix shared implementation interfaces with `dr_internal_`. Move component tests into their owning repository; add minimal and downstream CI.

* Move endpoint and schema validation tests into the independently checked component.

* Initial independent DataRaft package.

* Require lake/dbt integrations only when those optional operations are used.
