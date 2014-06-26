{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-typed-holes #-}


module ID.Html5
    ( ElementBuilder
    , EmptyBuilder

    , emptyTags
    , contentTags
    , attrNames

    , generateElement
    ) where


import           Control.Applicative
import           Control.Monad
import qualified Data.Map.Lazy       as M
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           System.Random.MWC
import           Text.XML


type ElementBuilder  = [Node] -> [(Name, T.Text)] -> Element
type EmptyBuilder    = [(Name, T.Text)] -> Element

emptyEl :: Name -> EmptyBuilder
emptyEl name attrs = Element name (M.fromList attrs) []

el :: Name -> ElementBuilder
el name children attrs = Element name (M.fromList attrs) children

text :: T.Text -> Node
text = NodeContent


emptyTags :: V.Vector EmptyBuilder
emptyTags = V.fromList [ emptyEl "area"
                       , emptyEl "base"
                       , emptyEl "br"
                       , emptyEl "col"
                       , emptyEl "embed"
                       , emptyEl "hr"
                       , emptyEl "img"
                       , emptyEl "input"
                       , emptyEl "keygen"
                       , emptyEl "link"
                       , emptyEl "menuitem"
                       , emptyEl "meta"
                       , emptyEl "param"
                       , emptyEl "source"
                       , emptyEl "track"
                       , emptyEl "wbr"
                       ]

contentTags :: V.Vector ElementBuilder
contentTags = V.fromList [ el "a"
                         , el "abbr"
                         , el "address"
                         , el "article"
                         , el "aside"
                         , el "audio"
                         , el "b"
                         , el "bdo"
                         , el "blockquote"
                         , el "body"
                         , el "button"
                         , el "canvas"
                         , el "caption"
                         , el "cite"
                         , el "code"
                         , el "colgroup"
                         , el "command"
                         , el "datalist"
                         , el "dd"
                         , el "del"
                         , el "details"
                         , el "dfn"
                         , el "div"
                         , el "dl"
                         , el "dt"
                         , el "em"
                         , el "fieldset"
                         , el "figcaption"
                         , el "figure"
                         , el "footer"
                         , el "form"
                         , el "h1"
                         , el "h2"
                         , el "h3"
                         , el "h4"
                         , el "h5"
                         , el "h6"
                         , el "head"
                         , el "header"
                         , el "hgroup"
                         , el "html"
                         , el "i"
                         , el "iframe"
                         , el "ins"
                         , el "kbd"
                         , el "label"
                         , el "legend"
                         , el "li"
                         , el "map"
                         , el "mark"
                         , el "menu"
                         , el "meter"
                         , el "nav"
                         , el "noscript"
                         , el "object"
                         , el "ol"
                         , el "optgroup"
                         , el "option"
                         , el "output"
                         , el "p"
                         , el "pre"
                         , el "progress"
                         , el "q"
                         , el "rp"
                         , el "rt"
                         , el "ruby"
                         , el "samp"
                         , el "script"
                         , el "section"
                         , el "select"
                         , el "small"
                         , el "span"
                         , el "strong"
                         , el "style"
                         , el "sub"
                         , el "summary"
                         , el "sup"
                         , el "table"
                         , el "tbody"
                         , el "td"
                         , el "textarea"
                         , el "tfoot"
                         , el "th"
                         , el "thead"
                         , el "time"
                         , el "title"
                         , el "tr"
                         , el "ul"
                         , el "var"
                         , el "video"
                         ]

attrNames :: V.Vector Name
attrNames = V.fromList [ "accept"
                       , "acceptCharset"
                       , "accesskey"
                       , "action"
                       , "alt"
                       , "async"
                       , "autocomplete"
                       , "autofocus"
                       , "autoplay"
                       , "challenge"
                       , "charset"
                       , "checked"
                       , "cite"
                       , "class"
                       , "cols"
                       , "colspan"
                       , "content"
                       , "contenteditable"
                       , "contextmenu"
                       , "controls"
                       , "coords"
                       , "data"
                       , "datetime"
                       , "defer"
                       , "dir"
                       , "disabled"
                       , "draggable"
                       , "enctype"
                       , "for"
                       , "form"
                       , "formaction"
                       , "formenctype"
                       , "formmethod"
                       , "formnovalidate"
                       , "formtarget"
                       , "headers"
                       , "height"
                       , "hidden"
                       , "high"
                       , "href"
                       , "hreflang"
                       , "httpEquiv"
                       , "icon"
                       , "id"
                       , "ismap"
                       , "item"
                       , "itemprop"
                       , "keytype"
                       , "label"
                       , "lang"
                       , "list"
                       , "loop"
                       , "low"
                       , "manifest"
                       , "max"
                       , "maxlength"
                       , "media"
                       , "method"
                       , "min"
                       , "multiple"
                       , "name"
                       , "novalidate"
                       , "onbeforeonload"
                       , "onbeforeprint"
                       , "onblur"
                       , "oncanplay"
                       , "oncanplaythrough"
                       , "onchange"
                       , "onclick"
                       , "oncontextmenu"
                       , "ondblclick"
                       , "ondrag"
                       , "ondragend"
                       , "ondragenter"
                       , "ondragleave"
                       , "ondragover"
                       , "ondragstart"
                       , "ondrop"
                       , "ondurationchange"
                       , "onemptied"
                       , "onended"
                       , "onerror"
                       , "onfocus"
                       , "onformchange"
                       , "onforminput"
                       , "onhaschange"
                       , "oninput"
                       , "oninvalid"
                       , "onkeydown"
                       , "onkeyup"
                       , "onload"
                       , "onloadeddata"
                       , "onloadedmetadata"
                       , "onloadstart"
                       , "onmessage"
                       , "onmousedown"
                       , "onmousemove"
                       , "onmouseout"
                       , "onmouseover"
                       , "onmouseup"
                       , "onmousewheel"
                       , "ononline"
                       , "onpagehide"
                       , "onpageshow"
                       , "onpause"
                       , "onplay"
                       , "onplaying"
                       , "onprogress"
                       , "onpropstate"
                       , "onratechange"
                       , "onreadystatechange"
                       , "onredo"
                       , "onresize"
                       , "onscroll"
                       , "onseeked"
                       , "onseeking"
                       , "onselect"
                       , "onstalled"
                       , "onstorage"
                       , "onsubmit"
                       , "onsuspend"
                       , "ontimeupdate"
                       , "onundo"
                       , "onunload"
                       , "onvolumechange"
                       , "onwaiting"
                       , "open"
                       , "optimum"
                       , "pattern"
                       , "ping"
                       , "placeholder"
                       , "preload"
                       , "pubdate"
                       , "radiogroup"
                       , "readonly"
                       , "rel"
                       , "required"
                       , "reversed"
                       , "rows"
                       , "rowspan"
                       , "sandbox"
                       , "scope"
                       , "scoped"
                       , "seamless"
                       , "selected"
                       , "shape"
                       , "size"
                       , "sizes"
                       , "span"
                       , "spellcheck"
                       , "src"
                       , "srcdoc"
                       , "start"
                       , "step"
                       , "style"
                       , "subject"
                       , "summary"
                       , "tabindex"
                       , "target"
                       , "title"
                       , "type"
                       , "usemap"
                       , "value"
                       , "width"
                       , "wrap"
                       , "xmlns"
                       ]

randItem :: V.Vector a -> GenIO -> IO a
randItem v g = (v V.!) <$> uniformR (0, V.length v - 1) g

randList :: Double -> GenIO -> IO a -> IO [a]
randList r g m =
    mapM (const m) . takeWhile (<=r) =<<  (sequence $ repeat (uniform g))

generateElement :: Double -> GenIO -> IO Element
generateElement r g =
    (`fmap` randList r g gattr) =<< generate' =<< uniform g
    where generate' :: Double -> IO EmptyBuilder
          generate' v | v < 0.5   = randItem emptyTags g
                      | otherwise = randItem contentTags g <*> randList r g gnode
          gnode = generateNode r g
          gattr = generateAttribute r g

generateNode :: Double -> GenIO -> IO Node
generateNode r g = generate' =<< uniform g
    where generate' :: Double -> IO Node
          generate' x | x < 0.5   = generateText r g
                      | otherwise = NodeElement <$> generateElement r g

generateAttribute :: Double -> GenIO -> IO (Name, T.Text)
generateAttribute r g = (,) <$> randItem attrNames g <*> generateValue r g

generateValue :: Double -> GenIO -> IO T.Text
generateValue r g = T.pack <$> randList r g (randItem alphaNum g)
    where alphaNum = V.fromList $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

generateText :: Double -> GenIO -> IO Node
generateText _ g =
    text . T.unwords . (`take` xs) <$> uniformR (0, length xs - 1) g
    where xs = T.words loremIpsum

loremIpsum :: T.Text
loremIpsum = "\
\Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cupiditates non\n\
\Epicuri divisione finiebat, sed sua satietate. Scaevola tribunus plebis\n\
\ferret ad plebem vellentne de ea re quaeri. Istic sum, inquit. Duo Reges:\n\
\constructio interrete.\n\
\\n\
\At enim hic etiam dolore. Ex ea difficultate illae fallaciloquae, ut ait\n\
\Accius, malitiae natae sunt. Si est nihil nisi corpus, summa erunt illa:\n\
\valitudo, vacuitas doloris, pulchritudo, cetera. Quamquam ab iis\n\
\philosophiam et omnes ingenuas disciplinas habemus; Tum Torquatus: Prorsus,\n\
\inquit, assentior; Non igitur de improbo, sed de callido improbo quaerimus,\n\
\qualis Q. Cur id non ita fit? Hunc vos beatum;\n\
\\n\
\Quis enim potest ea, quae probabilia videantur ei, non probare? Haec quo\n\
\modo conveniant, non sane intellego. Illa videamus, quae a te de amicitia\n\
\dicta sunt. Sic enim censent, oportunitatis esse beate vivere. Duo enim\n\
\genera quae erant, fecit tria. Graccho, eius fere, aequalí? Utinam quidem\n\
\dicerent alium alio beatiorem! Iam ruinas videres. Dulce amarum, leve\n\
\asperum, prope longe, stare movere, quadratum rotundum.\n\
\\n\
\Uterque enim summo bono fruitur, id est voluptate. Nos paucis ad haec\n\
\additis finem faciamus aliquando; Cum autem in quo sapienter dicimus, id\n\
\a primo rectissime dicitur. Quando enim Socrates, qui parens philosophiae\n\
\iure dici potest, quicquam tale fecit? Nonne igitur tibi videntur, inquit,\n\
\mala? Si enim ad populum me vocas, eum."

