<h1 align="left">
<img src="logo.png" height=25 width=35 />
json-mask
</h1>

A simple library for filtering JSON using the `fields` syntax inspired by Google's [Partial Response](https://developers.google.com/slides/api/guides/performance#partial). Field masks remove unnecessary values while retaining the structure of the JSON resource.

## Mask Syntax

* `a,b,c` comma-separated list will select multiple fields
* `a/b/c` path will select a field from its parent
* `a(b,c)` sub-selection will select many fields from a parent
* `a/*/c` the star `*` wildcard will select all items in a field

Terminal characters `,*()/` can be escaped using a single backslash `\`.

## Usage

```haskell
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.ByteString.Lazy as BSL
import JSONMask (masked)

val :: Aeson.Value
val = [Aeson.aesonQQ|{"p":{"a":1,"b":2},"z":1}|]

main :: IO ()
main = BSL.putStr . Aeson.encode $ masked val "p/a,z"

--  {p: {a: 1}, z: 1}
```

More examples can be found in the tests.
