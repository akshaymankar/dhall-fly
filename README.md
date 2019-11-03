# dhall-fly [![Build Status](https://travis-ci.org/akshaymankar/dhall-fly.svg?branch=master)](https://travis-ci.org/akshaymankar/dhall-fly)

Tool to read concourse config written using [dhall-concourse](https://github.com/akshaymankar/dhall-concourse).

## Installation

1. Install stack: https://docs.haskellstack.org/en/stable/README/
1. Clone this repository **recursively**
1. Run `stack install` in the repository. This will install `dhall-fly` binary in `~/.local/bin`.

## Usage

```bash
fly -t <TARGET> set-pipeline -c <(dhall-fly <<< '/path/to/pipeline.dhall')
```
