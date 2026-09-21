local_dbt_catalog_project <- function(failed = FALSE, .env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = .env)
  writeLines("name: fixture", file.path(root, "dbt_project.yml"))
  calls <- new.env(parent = emptyenv())
  calls$dbt <- 0L
  local_family_bindings(
    dbt_process = function(command, args, ...) {
      calls$dbt <- calls$dbt + 1L
      target <- args[match("--target-path", args) + 1L]
      fixture <- system.file(
        "extdata",
        "dbt-artifacts",
        package = "dataraft.dbt"
      )
      file.copy(list.files(fixture, full.names = TRUE), target)
      if (failed) {
        path <- file.path(target, "run_results.json")
        runs <- jsonlite::read_json(path)
        runs$results[[1L]]$status <- "fail"
        jsonlite::write_json(runs, path, auto_unbox = TRUE, null = "null")
      }
      list(status = if (failed) 1L else 0L, stdout = "", stderr = "")
    },
    .env = .env
  )
  list(
    project = dr_dbt_project(root, executable = file.path(R.home("bin"), "R")),
    calls = calls
  )
}

local_dbt_catalog <- function(.env = parent.frame(), ...) {
  withr::local_envvar(
    c(DATARAFT_TEST_OM_TOKEN = "fixture.jwt.token"),
    .local_envir = .env
  )
  dr_catalog_openmetadata_dbt(
    "https://metadata.example",
    "warehouse",
    token_env = "DATARAFT_TEST_OM_TOKEN",
    executable = file.path(R.home("bin"), "R"),
    ...
  )
}

test_that("the dbt catalog is optional and has an inspectable safe specification", {
  skip_if_not_installed("dataraft.dbt")
  x <- dr_catalog_openmetadata_dbt("https://metadata.example/", "warehouse")
  expect_identical(x$endpoint, "https://metadata.example")
  expect_identical(dr_inspect(x)$token_env, "OPENMETADATA_JWT_TOKEN")
  expect_true(dr_capabilities(x)$metadata)
  expect_false(dr_capabilities(x)$write)
  expect_error(
    dr_catalog_openmetadata_dbt("https://metadata.example/api", "x"),
    "without /api"
  )
  expect_error(
    dr_catalog_openmetadata_dbt("https://token@example.com", "x"),
    "without credentials"
  )
  expect_error(
    dr_catalog_openmetadata_dbt(
      "https://example.com",
      "x",
      token_env = "some.token"
    ),
    "variable name"
  )
  expect_error(
    dr_catalog_openmetadata_dbt(
      "https://example.com",
      "x",
      options = list(jwtToken = "not allowed")
    ),
    "documented"
  )
  expect_error(dr_catalog_openmetadata_dbt(
    "https://example.com",
    "x",
    options = list(includeTags = "yes")
  ))
  expect_error(
    dr_catalog_openmetadata_dbt(
      "https://example.com",
      "x",
      options = list(parsingTimeoutLimit = 0.5)
    ),
    "positive integer"
  )
  fixture <- local_dbt_catalog_project()
  result <- dr_dbt_build(fixture$project, echo = FALSE)
  expect_null(result$catalog_delivery)
  expect_length(result$artifact_hashes, 2L)
  expect_identical(result$invocation_id, result$manifest$metadata$invocation_id)
  expect_error(dr_publish_metadata(x, list(status = "published")), "dbt_result")
  expect_error(
    dr_dbt_build(
      fixture$project,
      catalog = structure(list(), class = "unsupported_catalog")
    ),
    "metadata_inputs"
  )
})

test_that("callbacks and external S3 catalogs use the same dbt workflow", {
  skip_if_not_installed("dataraft.dbt")
  fixture <- local_dbt_catalog_project()
  seen <- NULL
  callback <- function(result) {
    seen <<- result$invocation_id
    list(secret = "must not be retained")
  }
  first <- dr_dbt_build(fixture$project, echo = FALSE, catalog = callback)
  expect_identical(seen, first$invocation_id)
  expect_identical(first$catalog_delivery$status, "delivered")
  expect_null(first$catalog_delivery$secret)
  local_adapter_method(
    "dr_publish_metadata",
    "fixture_dbt_catalog",
    function(catalog, metadata, ...) {
      seen <<- metadata$invocation_id
      invisible(NULL)
    }
  )
  local_adapter_method(
    "dr_capabilities",
    "fixture_dbt_catalog",
    function(x, ...) {
      c(dr_component_capabilities(), list(metadata_inputs = "dr_dbt_result"))
    }
  )
  adapter <- structure(list(), class = "fixture_dbt_catalog")
  second <- dr_dbt_build(fixture$project, echo = FALSE, catalog = adapter)
  expect_identical(seen, second$invocation_id)
  expect_identical(second$catalog_delivery$status, "delivered")
  expect_true(second$success)
  expect_warning(
    failed <- dr_dbt_build(
      fixture$project,
      echo = FALSE,
      catalog = function(result) stop("secret-bearing external error")
    ),
    "Metadata delivery failed"
  )
  expect_true(failed$success)
  expect_identical(failed$catalog_delivery$status, "pending")
  expect_false(grepl("secret-bearing", failed$catalog_delivery$message))
})

test_that("delivery retries use the same verified invocation without rebuilding dbt", {
  skip_if_not_installed("dataraft.dbt")
  fixture <- local_dbt_catalog_project()
  adapter <- local_dbt_catalog(options = list(includeTags = FALSE))
  attempts <- 0L
  captured <- NULL
  local_family_bindings(dbt_catalog_process = function(
    command,
    args,
    wd,
    timeout,
    env
  ) {
    attempts <<- attempts + 1L
    config <- jsonlite::read_json(args[[3L]])
    captured <<- config
    files <- config$source$sourceConfig$config$dbtConfigSource
    expect_equal(
      dbt_read_artifacts(dirname(
        files$dbtManifestFilePath
      ))$manifest$metadata$invocation_id,
      "synthetic-documentation-example"
    )
    expect_true(file.exists(files$dbtRunResultsFilePath))
    expect_null(files$dbtCatalogFilePath)
    expect_identical(args[1:2], c("ingest", "-c"))
    expect_identical(config$workflowConfig$successThreshold, 100L)
    if (attempts == 1L) 2L else 0L
  })
  expect_warning(
    result <- dr_dbt_build(fixture$project, echo = FALSE, catalog = adapter),
    "nonzero exit status",
    class = "dr_dbt_catalog_delivery"
  )
  expect_true(result$success)
  expect_equal(result$catalog_delivery$status, "pending")
  expect_equal(result$catalog_delivery$attempt, 1L)
  expect_false(captured$source$sourceConfig$config$includeTags)
  expect_identical(
    captured$workflowConfig$openMetadataServerConfig$hostPort,
    "https://metadata.example/api"
  )
  expect_false(dir.exists(dirname(
    captured$source$sourceConfig$config$dbtConfigSource$dbtManifestFilePath
  )))
  # A fresh R result object and adapter need no callback or credentials in JSON.
  saved <- tempfile(fileext = ".rds")
  saveRDS(result, saved)
  result <- readRDS(saved)
  delivery <- dr_publish_metadata(adapter, result)
  expect_identical(delivery$status, "delivered")
  expect_equal(delivery$attempt, 2L)
  expect_true(delivery$recorded)
  expect_equal(fixture$calls$dbt, 1L)
  again <- dr_publish_metadata(adapter, result)
  expect_identical(again$status, "delivered")
  expect_equal(attempts, 2L)
  redelivered <- dr_publish_metadata(adapter, result, force = TRUE)
  expect_equal(redelivered$attempt, 3L)
  expect_identical(redelivered$status, "delivered")
  expect_equal(fixture$calls$dbt, 1L)
  receipt <- jsonlite::read_json(file.path(
    result$artifacts_dir,
    paste0(adapter$id, ".json")
  ))
  expect_equal(receipt$attempt, 3L)
  expect_false(any(
    c("token", "stdout", "stderr", "manifest", "options") %in% names(receipt)
  ))
})

test_that("failed dbt tests can deliver metadata while retaining their failure", {
  skip_if_not_installed("dataraft.dbt")
  fixture <- local_dbt_catalog_project(failed = TRUE)
  adapter <- local_dbt_catalog()
  local_family_bindings(dbt_catalog_process = function(...) 0L)
  result <- dr_dbt_test(
    fixture$project,
    echo = FALSE,
    stop_on_failure = FALSE,
    catalog = adapter
  )
  expect_false(result$success)
  expect_identical(result$status, 1L)
  expect_identical(result$catalog_delivery$status, "delivered")
  error <- tryCatch(
    dr_dbt_build(fixture$project, echo = FALSE, catalog = adapter),
    error = identity
  )
  expect_s3_class(error, "dr_dbt_failed")
  expect_false(error$result$success)
  expect_identical(error$result$catalog_delivery$status, "delivered")
})

test_that("missing and changed artifacts cannot invoke the metadata engine", {
  skip_if_not_installed("dataraft.dbt")
  fixture <- local_dbt_catalog_project()
  adapter <- local_dbt_catalog()
  local_family_bindings(dbt_catalog_process = function(...) {
    stop("must not run")
  })
  result <- dr_dbt_build(fixture$project, echo = FALSE)
  path <- file.path(result$artifacts_dir, "manifest.json")
  manifest <- jsonlite::read_json(path)
  manifest$metadata$invocation_id <- "different-invocation"
  jsonlite::write_json(manifest, path, auto_unbox = TRUE)
  expect_warning(delivery <- dr_publish_metadata(adapter, result), "blocked")
  expect_identical(delivery$status, "blocked")
  result <- dr_dbt_build(fixture$project, echo = FALSE)
  cat(
    "\n",
    file = file.path(result$artifacts_dir, "manifest.json"),
    append = TRUE
  )
  expect_warning(delivery <- dr_publish_metadata(adapter, result), "changed")
  expect_identical(delivery$status, "blocked")
  result <- dr_dbt_build(fixture$project, echo = FALSE)
  unlink(file.path(result$artifacts_dir, "run_results.json"))
  expect_warning(delivery <- dr_publish_metadata(adapter, result), "missing")
  expect_identical(delivery$status, "blocked")
  local_family_bindings(dbt_process = function(...) {
    list(status = 2L, stdout = "", stderr = "")
  })
  expect_warning(
    result <- dr_dbt_build(
      fixture$project,
      echo = FALSE,
      stop_on_failure = FALSE,
      catalog = adapter
    ),
    "blocked"
  )
  expect_false(result$success)
  expect_identical(result$catalog_delivery$status, "blocked")
})

test_that("optional catalog files must have matching invocation identity", {
  skip_if_not_installed("dataraft.dbt")
  fixture <- local_dbt_catalog_project()
  adapter <- local_dbt_catalog()
  result <- dr_dbt_build(fixture$project, echo = FALSE)
  catalog <- list(
    metadata = list(invocation_id = "another-run"),
    nodes = list()
  )
  jsonlite::write_json(
    catalog,
    file.path(result$artifacts_dir, "catalog.json"),
    auto_unbox = TRUE
  )
  result$artifact_hashes <- dbt_artifact_hashes(result$artifacts_dir)
  expect_warning(delivery <- dr_publish_metadata(adapter, result), "blocked")
  expect_identical(delivery$status, "blocked")
  catalog$metadata$invocation_id <- result$invocation_id
  jsonlite::write_json(
    catalog,
    file.path(result$artifacts_dir, "catalog.json"),
    auto_unbox = TRUE
  )
  result$artifact_hashes <- dbt_artifact_hashes(result$artifacts_dir)
  local_family_bindings(dbt_catalog_process = function(
    command,
    args,
    ...
  ) {
    config <- jsonlite::read_json(args[[3L]])
    expect_true(file.exists(
      config$source$sourceConfig$config$dbtConfigSource$dbtCatalogFilePath
    ))
    0L
  })
  expect_identical(dr_publish_metadata(adapter, result)$status, "delivered")
})

test_that("metadata availability and process errors preserve dbt outcomes and hide secrets", {
  skip_if_not_installed("dataraft.dbt")
  fixture <- local_dbt_catalog_project()
  adapter <- local_dbt_catalog()
  adapter$executable <- tempfile("missing-metadata-")
  expect_warning(
    result <- dr_dbt_build(fixture$project, echo = FALSE, catalog = adapter),
    "CLI is unavailable"
  )
  expect_true(result$success)
  expect_identical(
    result$catalog_delivery$error_class,
    "dr_dbt_catalog_unavailable"
  )
  adapter$executable <- file.path(R.home("bin"), "R")
  withr::local_envvar(c(DATARAFT_TEST_OM_TOKEN = ""))
  expect_warning(
    delivery <- dr_publish_metadata(adapter, result),
    "authentication"
  )
  expect_identical(delivery$error_class, "dr_dbt_catalog_credentials")
  withr::local_envvar(c(DATARAFT_TEST_OM_TOKEN = "sensitive-fixture-token"))
  local_family_bindings(dbt_catalog_process = function(...) {
    stop("raw subprocess failure contains sensitive-fixture-token")
  })
  expect_warning(
    delivery <- dr_publish_metadata(adapter, result),
    "could not finish"
  )
  expect_identical(delivery$error_class, "dr_dbt_catalog_process")
  expect_false(grepl(
    "sensitive-fixture-token",
    jsonlite::toJSON(delivery),
    fixed = TRUE
  ))
  receipt <- readLines(file.path(
    result$artifacts_dir,
    paste0(adapter$id, ".json")
  ))
  expect_false(any(grepl("sensitive-fixture-token", receipt, fixed = TRUE)))
  expect_true(result$success)
})

test_that("a failed receipt write cannot hide successful remote ingestion", {
  skip_if_not_installed("dataraft.dbt")
  fixture <- local_dbt_catalog_project()
  adapter <- local_dbt_catalog()
  local_family_bindings(
    dbt_catalog_process = function(...) 0L,
    dbt_catalog_write_receipt = function(...) FALSE
  )
  expect_warning(
    result <- dr_dbt_build(fixture$project, echo = FALSE, catalog = adapter),
    "Could not save",
    class = "dr_dbt_catalog_receipt"
  )
  expect_true(result$success)
  expect_identical(result$catalog_delivery$status, "delivered")
  expect_false(result$catalog_delivery$recorded)
})

test_that("a real subprocess receives literal config and environment-only credentials", {
  skip_if_not_installed("dataraft.dbt")
  skip_on_os("windows")
  python <- Sys.which("python3")
  skip_if(!nzchar(python), "Python fixture executable unavailable")
  fixture <- local_dbt_catalog_project()
  root <- withr::local_tempdir()
  executable <- file.path(root, "metadata fixture")
  capture <- file.path(root, "capture.json")
  writeLines(
    c(
      paste0("#!", python),
      "import sys, os, json, pathlib",
      "assert sys.argv[1:3] == ['ingest', '-c']",
      "raw = pathlib.Path(sys.argv[3]).read_text()",
      "config = json.loads(os.path.expandvars(raw))",
      "secret = os.environ['DATARAFT_TEST_OM_TOKEN']",
      "assert secret not in raw",
      "server = config['workflowConfig']['openMetadataServerConfig']",
      "assert server['securityConfig']['jwtToken'] == secret",
      "server['securityConfig']['jwtToken'] = '<redacted>'",
      "pathlib.Path(os.environ['DATARAFT_TEST_OM_CAPTURE']).write_text(json.dumps(config))",
      "print(secret)",
      "print(secret, file=sys.stderr)",
      "sys.exit(int(os.environ.get('DATARAFT_TEST_OM_EXIT', '0')))"
    ),
    executable
  )
  Sys.chmod(executable, "0700")
  secret <- 'fixture.token.with"quotes\\and${literal}'
  withr::local_envvar(c(
    DATARAFT_TEST_OM_TOKEN = secret,
    DATARAFT_TEST_OM_CAPTURE = capture,
    DATARAFT_TEST_OM_EXIT = "7",
    DATARAFT_TEST_UNEXPANDED = "must-not-appear"
  ))
  adapter <- dr_catalog_openmetadata_dbt(
    "https://metadata.example",
    service = "warehouse_${DATARAFT_TEST_UNEXPANDED}",
    token_env = "DATARAFT_TEST_OM_TOKEN",
    executable = executable
  )
  expect_warning(
    result <- dr_dbt_build(fixture$project, echo = FALSE, catalog = adapter),
    "nonzero"
  )
  expect_identical(result$catalog_delivery$exit_status, 7L)
  expect_false(grepl(
    secret,
    jsonlite::toJSON(result$catalog_delivery),
    fixed = TRUE
  ))
  config <- jsonlite::read_json(capture)
  expect_identical(
    config$source$serviceName,
    "warehouse_${DATARAFT_TEST_UNEXPANDED}"
  )
  expect_false(dir.exists(dirname(
    config$source$sourceConfig$config$dbtConfigSource$dbtManifestFilePath
  )))
  withr::local_envvar(c(DATARAFT_TEST_OM_EXIT = "0"))
  expect_identical(dr_publish_metadata(adapter, result)$status, "delivered")
  expect_equal(fixture$calls$dbt, 1L)
})

test_that("the generated config validates against the installed official SDK", {
  skip_if_not_installed("dataraft.dbt")
  python <- Sys.getenv("DATARAFT_OPENMETADATA_PYTHON")
  skip_if(
    !nzchar(python),
    "Set DATARAFT_OPENMETADATA_PYTHON for optional official SDK validation"
  )
  fixture <- local_dbt_catalog_project()
  adapter <- local_dbt_catalog(
    options = list(
      dbtUpdateDescriptions = TRUE,
      includeTags = TRUE,
      parsingTimeoutLimit = 30L
    )
  )
  result <- dr_dbt_build(fixture$project, echo = FALSE)
  config <- dbt_catalog_config(
    adapter,
    result$artifacts_dir,
    names(result$artifact_hashes)
  )
  config_path <- tempfile(fileext = ".json")
  jsonlite::write_json(config, config_path, auto_unbox = TRUE, null = "null")
  script <- paste(
    "import sys",
    "from pathlib import Path",
    "from metadata.config.common import load_config_file",
    "from metadata.generated.schema.metadataIngestion.workflow import OpenMetadataWorkflowConfig",
    "from metadata.generated.schema.metadataIngestion.dbtPipeline import DbtPipeline",
    "from metadata.utils.ssl_registry import get_verify_ssl_fn",
    "from metadata.ingestion.ometa.client import REST, ClientConfig",
    "config = load_config_file(Path(sys.argv[1]))",
    "workflow = OpenMetadataWorkflowConfig.model_validate(config)",
    "DbtPipeline.model_validate(config['source']['sourceConfig']['config'])",
    "server = workflow.workflowConfig.openMetadataServerConfig",
    "verify = get_verify_ssl_fn(server.verifySSL)(server.sslConfig)",
    "client = REST(ClientConfig(base_url=server.hostPort, verify=verify))",
    "effective = client._session.merge_environment_settings(server.hostPort, {}, False, client._verify, None)['verify']",
    "assert effective is True or (isinstance(effective, str) and effective), 'TLS verification must remain enabled'",
    "print('Official OpenMetadata workflow and dbt schemas validated')",
    sep = "\n"
  )
  checked <- processx::run(
    python,
    c("-c", script, config_path),
    error_on_status = FALSE
  )
  expect_identical(checked$status, 0L, info = checked$stderr)
  expect_match(checked$stdout, "schemas validated")
})

test_that("the installed OpenMetadata CLI reaches a local server and reports rejection safely", {
  skip_if_not_installed("dataraft.dbt")
  executable <- Sys.getenv("DATARAFT_OPENMETADATA_CLI")
  skip_if(
    !nzchar(executable),
    "Set DATARAFT_OPENMETADATA_CLI for optional real CLI smoke testing"
  )
  skip_if_not_installed("webfakes")
  fixture <- local_dbt_catalog_project()
  path <- withr::local_tempdir()
  app <- webfakes::new_app()
  handler <- local({
    directory <- path
    function(req, res) {
      # Do not record authentication headers or SDK request bodies.
      writeLines(req$path, file.path(directory, "request-path.txt"))
      res$set_status(403L)$send_json(
        list(message = "fixture rejection"),
        auto_unbox = TRUE
      )
    }
  })
  app$use(handler)
  server <- webfakes::new_app_process(app)
  withr::defer(server$stop())
  withr::local_envvar(c(DATARAFT_TEST_OM_TOKEN = "fixture.jwt.token"))
  adapter <- dr_catalog_openmetadata_dbt(
    sub("/$", "", server$url()),
    "warehouse",
    token_env = "DATARAFT_TEST_OM_TOKEN",
    executable = executable,
    timeout = 30
  )
  expect_warning(
    result <- dr_dbt_build(fixture$project, echo = FALSE, catalog = adapter),
    class = "dr_dbt_catalog_delivery"
  )
  expect_true(result$success)
  expect_identical(result$catalog_delivery$status, "pending")
  expect_true(file.exists(file.path(path, "request-path.txt")))
  expect_match(readLines(file.path(path, "request-path.txt")), "^/api/")
  expect_false(grepl(
    "fixture.jwt.token",
    jsonlite::toJSON(result$catalog_delivery),
    fixed = TRUE
  ))
})
