{-# LANGUAGE OverloadedStrings #-}


module ID.Html5
    ( emptyTags
    , contentTags
    , attrNames
    ) where


import           Text.Blaze.Html5
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A


emptyTags :: [Html]
emptyTags = [ area
            , base
            , br
            , col
            , embed
            , hr
            , img
            , input
            , keygen
            , link
            , menuitem
            , meta
            , param
            , source
            , track
            , wbr
            ]

contentTags :: [Html -> Html]
contentTags = [ a
              , abbr
              , address
              , article
              , aside
              , audio
              , b
              , bdo
              , blockquote
              , body
              , button
              , canvas
              , caption
              , H.cite
              , code
              , colgroup
              , command
              , datalist
              , dd
              , del
              , details
              , dfn
              , H.div
              , dl
              , dt
              , em
              , fieldset
              , figcaption
              , figure
              , footer
              , H.form
              , h1, h2, h3, h4, h5, h6
              , H.head
              , header
              , hgroup
              , html
              , i
              , iframe
              , ins
              , kbd
              , H.label
              , legend
              , li
              , H.map
              , mark
              , menu
              , meter
              , nav
              , noscript
              , object
              , ol
              , optgroup
              , option
              , output
              , p
              , pre
              , progress
              , q
              , rp
              , rt
              , ruby
              , samp
              , script
              , section
              , select
              , small
              , H.span
              , strong
              , H.style
              , sub
              , H.summary
              , sup
              , table
              , tbody
              , td
              , textarea
              , tfoot
              , th
              , thead
              , time
              , H.title
              , tr
              , ul
              , var
              , video
              ]

attrNames :: [AttributeValue -> Attribute]
attrNames = [ accept
            , acceptCharset
            , accesskey
            , action
            , alt
            , async
            , autocomplete
            , autofocus
            , autoplay
            , challenge
            , charset
            , checked
            , A.cite
            , class_
            , cols
            , colspan
            , content
            , contenteditable
            , contextmenu
            , controls
            , coords
            , data_
            , datetime
            , defer
            , dir
            , disabled
            , draggable
            , enctype
            , for
            , A.form
            , formaction
            , formenctype
            , formmethod
            , formnovalidate
            , formtarget
            , headers
            , height
            , hidden
            , high
            , href
            , hreflang
            , httpEquiv
            , icon
            , A.id
            , ismap
            , item
            , itemprop
            , keytype
            , A.label
            , lang
            , list
            , loop
            , low
            , manifest
            , A.max
            , maxlength
            , media
            , method
            , A.min
            , multiple
            , name
            , novalidate
            , onbeforeonload
            , onbeforeprint
            , onblur
            , oncanplay
            , oncanplaythrough
            , onchange
            , onclick
            , oncontextmenu
            , ondblclick
            , ondrag
            , ondragend
            , ondragenter
            , ondragleave
            , ondragover
            , ondragstart
            , ondrop
            , ondurationchange
            , onemptied
            , onended
            , onerror
            , onfocus
            , onformchange
            , onforminput
            , onhaschange
            , oninput
            , oninvalid
            , onkeydown
            , onkeyup
            , onload
            , onloadeddata
            , onloadedmetadata
            , onloadstart
            , onmessage
            , onmousedown
            , onmousemove
            , onmouseout
            , onmouseover
            , onmouseup
            , onmousewheel
            , ononline
            , onpagehide
            , onpageshow
            , onpause
            , onplay
            , onplaying
            , onprogress
            , onpropstate
            , onratechange
            , onreadystatechange
            , onredo
            , onresize
            , onscroll
            , onseeked
            , onseeking
            , onselect
            , onstalled
            , onstorage
            , onsubmit
            , onsuspend
            , ontimeupdate
            , onundo
            , onunload
            , onvolumechange
            , onwaiting
            , open
            , optimum
            , pattern
            , ping
            , placeholder
            , preload
            , pubdate
            , radiogroup
            , readonly
            , rel
            , required
            , reversed
            , rows
            , rowspan
            , sandbox
            , scope
            , scoped
            , seamless
            , selected
            , shape
            , size
            , sizes
            , A.span
            , spellcheck
            , src
            , srcdoc
            , start
            , step
            , A.style
            , subject
            , A.summary
            , tabindex
            , target
            , A.title
            , type_
            , usemap
            , value
            , width
            , wrap
            , xmlns
            ]
