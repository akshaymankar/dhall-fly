let Concourse = ../../dhall-concourse/package.dhall

in  Concourse.schemas.SetPipelineStep::{
    , set_pipeline = "test-pipeline"
    , file = "test-file"
    }
