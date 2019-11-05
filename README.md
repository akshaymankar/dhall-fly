# dhall-fly [![Build Status](https://travis-ci.org/akshaymankar/dhall-fly.svg?branch=master)](https://travis-ci.org/akshaymankar/dhall-fly)

Tool to read concourse config written using [dhall-concourse](https://github.com/akshaymankar/dhall-concourse).

## Installation

### Using Homebrew

`brew install akshaymankar/tap/dhall-fly`

### Copy the binary (only MacOS)

Go to [Releases Page](https://github.com/akshaymankar/dhall-fly/releases), download the `dhall-fly-<version>-darwin.tgz`, use the binary inside. 
**Note:** This is only tested on Mojave, should work on Catalina.

### From Hackage

`cabal install dhall-fly`

### From source

1. Install stack: https://docs.haskellstack.org/en/stable/README/
1. Clone this repository **recursively**
1. Run `stack install` in the repository. This will install `dhall-fly` binary in `~/.local/bin`.

## Usage

```bash
fly -t <TARGET> set-pipeline -c <(dhall-fly <<< '/path/to/pipeline.dhall')
```
