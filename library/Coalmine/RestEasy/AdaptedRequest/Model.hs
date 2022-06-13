module Coalmine.RestEasy.AdaptedRequest.Model where

import Coalmine.InternalPrelude
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
        Domain.constructorIsLabelDeriver,
        DomainOptics.labelOpticDeriver
      ]
  )
  =<< Domain.loadSchema "domain/rest-easy-request.domain.yaml"
