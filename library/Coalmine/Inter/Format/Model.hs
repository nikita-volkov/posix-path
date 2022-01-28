module Coalmine.Inter.Format.Model where

import Coalmine.Prelude
import qualified Domain
import qualified DomainOptics

Domain.declare
  Nothing
  ( mconcat
      [ Domain.enumDeriver,
        Domain.boundedDeriver,
        Domain.showDeriver,
        Domain.eqDeriver,
        Domain.ordDeriver,
        Domain.genericDeriver,
        Domain.accessorIsLabelDeriver,
        Domain.constructorIsLabelDeriver
      ]
  )
  =<< Domain.loadSchema "domain/inter-format.domain.yaml"
