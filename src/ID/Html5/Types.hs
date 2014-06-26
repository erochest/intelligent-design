module ID.Html5.Types
    ( AttributeBuilder
    , ElementBuilder
    ) where


import qualified Data.Text as T
import           Text.XML


type AttributeBuilder = [(Name, T.Text)] -> Element
type ElementBuilder   = [Node] -> AttributeBuilder

