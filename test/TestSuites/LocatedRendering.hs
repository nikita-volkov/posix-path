module TestSuites.LocatedRendering where

import Coalmine.Inter
import qualified Coalmine.Located.Rendering as Rendering
import Coalmine.Prelude
import Coalmine.Tasty
import qualified Data.Text as Text

tests =
  [ testCase "single-line" $
      let input =
            [i|
              create table "group" (
                "id" int8 not null null generated always as identity primary key,
                "name" text not null unique
              );
            |]
          selectionStart = 22 + 21
          selectionEnd = selectionStart + 4
          actualResult = Rendering.render selectionStart selectionEnd input
          expectedResult =
            [i|
            
            |]
       in assertEqual (toString actualResult) expectedResult actualResult
  ]
