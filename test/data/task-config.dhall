let Concourse = ../../dhall-concourse/package.dhall

in  Concourse.schemas.TaskConfig::{
    , run = Concourse.schemas.TaskRunConfig::{ path = "true" }
    }
