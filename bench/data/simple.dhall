let Concourse = ../../dhall-concourse/package.dhall

let Prelude =
      https://prelude.dhall-lang.org/v11.1.0/package.dhall sha256:99462c205117931c0919f155a6046aec140c70fb8876d208c7c77027ab19c2fa

let busyboxImage =
      Concourse.schemas.ImageResource::{
      , type = "docker-image"
      , source = Some (toMap { repository = Prelude.JSON.string "busybox" })
      }

let job =
      Concourse.schemas.Job::{
      , name = "hello"
      , plan =
          [ Concourse.helpers.taskStep
              Concourse.schemas.TaskStep::{
              , task = "hello"
              , config =
                  Concourse.Types.TaskSpec.Config
                    Concourse.schemas.TaskConfig::{
                    , image_resource = Some busyboxImage
                    , run =
                        Concourse.schemas.TaskRunConfig::{
                        , path = "bash"
                        , args = Some [ "-c", "echo Hello Dhall" ]
                        }
                    }
              }
          ]
      }

in  [ job ]
