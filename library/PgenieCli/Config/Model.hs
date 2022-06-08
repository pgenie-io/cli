module PgenieCli.Config.Model where

import qualified Domain
import qualified DomainAeson
import qualified DomainOptics
import PgenieCli.Prelude

Domain.declare
  Nothing
  ( mconcat
      [ Domain.enumDeriver,
        Domain.boundedDeriver,
        Domain.showDeriver,
        Domain.eqDeriver,
        Domain.ordDeriver,
        Domain.genericDeriver,
        Domain.constructorIsLabelDeriver,
        Domain.accessorIsLabelDeriver,
        DomainOptics.labelOpticDeriver,
        DomainAeson.toJsonDeriver
      ]
  )
  =<< Domain.loadSchema "domain/config.domain.yaml"
