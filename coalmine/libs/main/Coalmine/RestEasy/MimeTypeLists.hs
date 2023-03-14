module Coalmine.RestEasy.MimeTypeLists where

import Coalmine.InternalPrelude

binary :: (IsString a) => [a]
binary = ["application/octet-stream"]

html :: (IsString a) => [a]
html = ["text/html", "application/xhtml+xml"]

json :: (IsString a) => [a]
json = ["application/json", "text/json"]

text :: (IsString a) => [a]
text = ["text/plain"]

xhtml :: (IsString a) => [a]
xhtml = ["application/xhtml+xml", "text/html"]

xml :: (IsString a) => [a]
xml = ["application/xml", "text/xml"]

yaml :: (IsString a) => [a]
yaml = ["application/yaml", "text/vnd.yaml", "text/yaml", "text/x-yaml", "application/x-yaml"]
