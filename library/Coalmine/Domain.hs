module Coalmine.Domain where

import Coalmine.InternalPrelude
import Domain qualified
import DomainOptics qualified

declareStd =
  Domain.declare
    Nothing
    ( mconcat
        [ Domain.enumDeriver,
          Domain.boundedDeriver,
          Domain.showDeriver,
          Domain.eqDeriver,
          Domain.ordDeriver,
          Domain.genericDeriver,
          Domain.hashableDeriver,
          Domain.hasFieldDeriver,
          Domain.constructorIsLabelDeriver,
          Domain.accessorIsLabelDeriver,
          Domain.mapperIsLabelDeriver,
          DomainOptics.labelOpticDeriver
        ]
    )

loadAndDeclare path =
  declareStd
    =<< Domain.loadSchema path
