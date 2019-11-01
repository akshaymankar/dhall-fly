let Concourse = ../../dhall-concourse/package.dhall

let Prelude = ../../dhall-concourse/lib/prelude.dhall

let JSON = Prelude.JSON

in  Concourse.Types.ResourceType.Custom
      Concourse.schemas.CustomResourceType::{
      , name = "bosh-deployment"
      , type = "docker-image"
      , source =
          Some
            ( toMap
                { repository =
                    JSON.string "cloudfoundry/bosh-deployment-resource"
                , tag = JSON.string "latest"
                }
            )
      }
