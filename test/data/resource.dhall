let Concourse = ../../dhall-concourse/package.dhall

let Prelude = ../../dhall-concourse/lib/prelude.dhall

let JSON = Prelude.JSON

in  Concourse.schemas.Resource::{
    , name = "git-kubo-ci"
    , type = ./in-built-resource-type.dhall
    , source =
        Some
          ( toMap
              { privateKey = JSON.string "((git-ssh-key.private_key))"
              , uri =
                  JSON.string "git@github.com:cloudfoundry-incubator/kubo-ci"
              , branch = JSON.string "master"
              }
          )
    }
