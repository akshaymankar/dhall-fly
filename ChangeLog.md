# Changelog for dhall-fly

## 0.3.0

* Add support for `set_pipeline` step.

## 0.2.4

* Tighten bounds for dhall-json.

## 0.2.3

* Upgrade dhall to 1.29 and dhall-json to 1.6.1.
* Upgrade dhall-concourse to 0.6.0.

## 0.2.2

* Upgrade dhall to 1.28 and dhall-json to 1.6.

## 0.2.1

* Groups retain the order they are in list of grouped jobs.

## 0.2.0

* Tested with dhall-concourse 0.5.0.
* Add `--file` command line option to render pipelines from files.
* Add support for grouping jobs.
* Fix: Add inputs to `PutSteps`.
* Fix: Add tags, timeouts and attempts to `ToJSON` instance of `PutStep`.
* Fix: Add vars, tags, timeouts and attempts to `TaskStep`.

## 0.1.0

* Support for dhall-concourse 0.2.2.

## Unreleased changes
