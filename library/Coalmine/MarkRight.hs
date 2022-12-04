module Coalmine.MarkRight where

import Coalmine.Inter
import Coalmine.InternalPrelude hiding (List)
import Coalmine.MarkRight.Ast qualified as Ast
import Coalmine.TH.QuasiQuoter qualified as Qq
import Data.Text qualified as Text

instance LenientParser Ast.Tree where
  lenientParser =
    error "TODO"

q :: QuasiQuoter
q =
  Qq.lenientLiteral (Proxy @Ast.Tree)
