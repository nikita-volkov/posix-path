module TestSuites.LocatedRendering where

import Coalmine.Inter
import qualified Coalmine.Located.Rendering as Rendering
import Coalmine.Prelude
import Coalmine.Tasty
import qualified Data.Text as Text

tests =
  [ testGroup "select" $
      [ testCase "multiline" $
          let input =
                [i|
                  "id" int8 not null null generated always as identity primary key,
                  "name" text not null unique
                |]
              actualResult = Text.intercalate "\n" $ Rendering.select 2 19 6 $ Text.lines input
              expectedResult =
                [i|
                    |
                  2 | "id" int8 not null null generated always as identity primary key,
                    |                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                  3 | "name" text not null unique
                    | ^^^^^^
                |]
           in assertEqual (toString actualResult) expectedResult actualResult,
        testCase "single-line" $
          let input =
                [i|
                  "id" int8 not null null generated always as identity primary key,
                |]
              actualResult = Text.intercalate "\n" $ Rendering.select 2 19 23 $ Text.lines input
              expectedResult =
                [i|
                    |
                  2 | "id" int8 not null null generated always as identity primary key,
                    |                    ^^^^
                |]
           in assertEqual (toString actualResult) expectedResult actualResult
      ],
    testGroup "render" $
      [ testCase "1" $
          let input =
                [i|
                  create table "group" (
                    "id" int8 not null null generated always as identity primary key,
                    "name" text not null unique
                  );
                |]
              selectionStart = 23 + 21
              selectionEnd = selectionStart + 4
              actualResult = Rendering.render input selectionStart selectionEnd "Details"
              expectedResult =
                [i|
                  2:22:
                    |
                  2 |   "id" int8 not null null generated always as identity primary key,
                    |                      ^^^^
                  Details
                |]
           in assertEqual (toString actualResult) expectedResult actualResult,
        testCase "2" $
          let input =
                [i|
                  create table "group" (
                    "id" int8 not null null generated always as identity primary key,
                    "name" text not null unique
                  );
                |]
              selectionStart = 13
              selectionEnd = selectionStart + 7
              actualResult = Rendering.render input selectionStart selectionEnd "Details"
              expectedResult =
                [i|
                  1:14:
                    |
                  1 | create table "group" (
                    |              ^^^^^^^
                  Details
                |]
           in assertEqual (toString actualResult) expectedResult actualResult,
        testCase "3" $
          let input =
                [i|
                  create table "group" (
                    "id" int8 not null null generated always as identity primary key,
                    "name" text not null unique
                  );
                |]
              selectionStart = 13
              selectionEnd = selectionStart + 7 + 3 + 2 + 4
              actualResult = Rendering.render input selectionStart selectionEnd "Details"
              expectedResult =
                [i|
                  1:14:
                    |
                  1 | create table "group" (
                    |              ^^^^^^^^^
                  2 |   "id" int8 not null null generated always as identity primary key,
                    | ^^^^^^
                  Details
                |]
           in assertEqual (toString actualResult) expectedResult actualResult,
        testCase "4" $
          let input =
                [i|
                  create table "group" (
                    "id" int8 not null null generated always as identity primary key,
                    "name" text not null unique
                  );
                |]
              selectionStart = 22
              selectionEnd = selectionStart + 67 + 29 + 2
              actualResult = Rendering.render input selectionStart selectionEnd "Details"
              expectedResult =
                [i|
                  1:23:
                    |
                  1 | create table "group" (
                    |                       
                  2 |   "id" int8 not null null generated always as identity primary key,
                    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                  3 |   "name" text not null unique
                    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                  Details
                |]
           in assertEqual (toString actualResult) expectedResult actualResult,
        testCase "5" $
          let input =
                [i|
                  create table "group" (
                    "id" int8 not null null generated always as identity primary key,
                    "name" text not null unique
                  );
                |]
              selectionStart = 23
              selectionEnd = selectionStart + 67 + 29 + 2
              actualResult = Rendering.render input selectionStart selectionEnd "Details"
              expectedResult =
                [i|
                  2:1:
                    |
                  2 |   "id" int8 not null null generated always as identity primary key,
                    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                  3 |   "name" text not null unique
                    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                  4 | );
                    | 
                  Details
                |]
           in assertEqual (toString actualResult) expectedResult actualResult,
        testCase "6" $
          let input =
                [i|
                  create table "group" (
                    "id" int8 not null null generated always as identity primary key,
                    "name" text not null unique
                  );
                |]
              selectionStart = 23
              selectionEnd = selectionStart + 67 + 29 + 1
              actualResult = Rendering.render input selectionStart selectionEnd "Details"
              expectedResult =
                [i|
                  2:1:
                    |
                  2 |   "id" int8 not null null generated always as identity primary key,
                    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                  3 |   "name" text not null unique
                    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                  Details
                |]
           in assertEqual (toString actualResult) expectedResult actualResult
      ]
  ]
