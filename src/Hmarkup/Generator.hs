module Hmarkup.Generator where

import qualified System.IO as IO
import qualified Hmarkup.Tags as Tags
import qualified Text.Printf as Printf

data StyleSheet
  = ExternalCSS
      { styleSheetPath :: String
      }
  | InlineCSS
      { style :: String
      }

data Javascript
  = ExternalJS
      { jsPath :: String
      }
  | InlineJS
      { script :: String
      }

boilerPlate :: String
boilerPlate =
  "<html lang=\"en\">\n<head>\n<meta charset=\"UTF-8\">\n<title>%s</title>\n%s\n%s\n</head>\n<body>\n%s\n</body>\n%s\n</html>\n"

instance Show StyleSheet where
  show (ExternalCSS p) = Printf.printf "<link rel=\"stylesheet\" href=\"%s\">" p
  show (InlineCSS s) = Printf.printf "<style type=\"text/css\">%s</style>" s

instance Show Javascript where
  show (ExternalJS p) = Printf.printf "<script src=\"%s\"></script>" p
  show (InlineJS s) = Printf.printf "<script>%s</script>" s

showStyleSheets :: [StyleSheet] -> String
showStyleSheets st = concat [show x | x <- st]

showScripts :: [Javascript] -> String
showScripts js = concat [show x | x <- js]

generateHtml ::
     String
  -> [StyleSheet]
  -> [Javascript]
  -> [Javascript]
  -> [Tags.Tag]
  -> String
generateHtml title css preload postload tags =
  Printf.printf
    boilerPlate
    title
    (showStyleSheets css)
    (showScripts preload)
    (Tags.showTags tags)
    (showScripts postload)

writeHtml ::
     String
  -> [StyleSheet]
  -> [Javascript]
  -> [Javascript]
  -> [Tags.Tag]
  -> String
  -> IO ()
writeHtml title css preload postload tags output =
  IO.writeFile output (generateHtml title css preload postload tags)
