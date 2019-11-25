let Concourse = ../../dhall-concourse/package.dhall

in Concourse.schemas.GetStep::{ resource = ./resource.dhall }
