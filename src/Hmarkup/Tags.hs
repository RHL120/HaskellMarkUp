module Hmarkup.Tags where
import qualified Data.Text
import qualified Text.Printf as Printf

data Prop
  = Prop [(String, String)]
  | NoProp

instance Show Prop where
  show (Prop p) =
    " " ++ unwords [uncurry (Printf.printf "%s=\"%s\"") x | x <- p]
  show NoProp = ""

data Tag
  = Text
      { contents :: String
      }
  | Paragraph
      { props :: Prop
      , subtags :: [Tag]
      }
  | Heading
      { level :: Int
      , props :: Prop
      , subtags :: [Tag]
      }
  | Link
      { props :: Prop
      , subtags :: [Tag]
      }
  | Div
      { props :: Prop
      , subtags :: [Tag]
      }
  | Bold
      { subtags :: [Tag]
      }
  | Italic
      { subtags :: [Tag]
      }
  | Image
      { props :: Prop
      }
  | Header
      { props :: Prop
      , subtags :: [Tag]
      }
  | Nav
      { props :: Prop
      , subtags :: [Tag]
      }
  | Main
      { props :: Prop
      , subtags :: [Tag]
      }
  | Footer
      { props :: Prop
      , subtags :: [Tag]
      }
  | OrderedList
      { props :: Prop
      , subtags :: [Tag]
      }
  | UnorderedList
      { props :: Prop
      , subtags :: [Tag]
      }
  | ListItem
      { props :: Prop
      , subtags :: [Tag]
      }
  | LnBreak

showTags :: [Tag] -> String
showTags st = concat [show x | x <- st]

instance Show Tag where
  show (Text str) =
    concat
      [ if x == '\n'
        then "<br>"
        else [x]
      | x <- str
      ]
  show (Paragraph p st) =
    Printf.printf "<p%s>\n%s\n</p>\n" (show p) (showTags st)
  show (Heading l p s) =
    if l >= 6
      then error "Can't convert Heading with more than level 6"
      else Printf.printf "<h%d%s>\n%s\n</h%d>\n" l (show p) (showTags s) l
  show (Link p st) = Printf.printf "<a %s>\n%s\n</a>" (show p) (showTags st)
  show (Div p st) =
    Printf.printf "<div %s>\n%s\n</div>" (show p) (showTags st)
  show (Bold st) = Printf.printf "<b>\n%s\n</b>" (showTags st)
  show (Italic st) = Printf.printf "<i>\n%s\n</i>" (showTags st)
  show (Image p) = Printf.printf "<img %s>\n" (show p)
  show (OrderedList p st) =
    Printf.printf "<ol %s>\n%s\n</ol>" (show p) (showTags st)
  show (UnorderedList p st) =
    Printf.printf "<ul %s>\n%s\n</ul>" (show p) (showTags st)
  show (ListItem p st) =
    Printf.printf "<li %s>\n%s\n</li>" (show p) (showTags st)
  show LnBreak = "<br>"
  show (Header p st) =
    Printf.printf "<header%s>\n%s\n</header>\n" (show p) (showTags st)
  show (Nav p st) =
    Printf.printf "<nav%s>\n%s\n</nav>\n" (show p) (showTags st)
  show (Main p st) =
    Printf.printf "<main%s>\n%s\n</main>\n" (show p) (showTags st)
  show (Footer p st) =
    Printf.printf "<footer%s>\n%s\n</footer>\n" (show p) (showTags st)
