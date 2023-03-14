module Coalmine.Domain where

import Coalmine.InternalPrelude
import Domain qualified
import DomainOptics qualified
import Language.Haskell.TH.Syntax qualified as Th

declareStd :: Domain.Schema -> Th.Q [Th.Dec]
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

loadAndDeclare :: FilePath -> Th.Q [Th.Dec]
loadAndDeclare path =
  declareStd
    =<< Domain.loadSchema path
