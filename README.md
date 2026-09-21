# dataraft.catalog

This integration is **experimental**. OpenLineage and OpenMetadata evolve independently of DataRaft; pin and validate the versions you deploy. Metadata delivery is at least once and does not make a data write transactional.

Publish metadata with OpenLineage or OpenMetadata adapters and browse catalog snapshots. Metadata delivery is separate from successful data publication and can be retried.

This is an independently installable DataRaft component. The `dataraft`
metapackage provides the shared introduction and re-exports the family API.
See `help(package = "dataraft.catalog")` for the component reference.

Install the development version:

```r
install.packages("pak")
pak::pak("dataraft-r/dataraft.catalog")
```

[Get started with DataRaft](https://github.com/dataraft-r/dataraft).
