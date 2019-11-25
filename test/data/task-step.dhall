let Concourse = ../../dhall-concourse/package.dhall

in  Concourse.schemas.TaskStep::{
    , task = "test-task"
    , config = Concourse.Types.TaskSpec.File "some-file"
    }
