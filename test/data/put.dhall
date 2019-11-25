let Concourse = ../../dhall-concourse/package.dhall

in Concourse.schemas.PutStep::{ resource = ./resource.dhall }
