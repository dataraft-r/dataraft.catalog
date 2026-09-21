# dataraft.catalog 0.1.0.9000

* Publish direct-column maps as OpenLineage facets and governance fields to OpenMetadata. Improve contrast, labels, tab semantics and keyboard navigation.

* Keep stateless helpers private and prefix shared implementation interfaces with `dr_internal_`. Move component tests into their owning repository; add minimal and downstream CI.

* Move endpoint and schema validation tests into the independently checked component.

* Initial independent DataRaft package.

* Require lake/dbt integrations only when those optional operations are used.
