module ColorTheme exposing (ColorTheme, darkTheme, lightTheme)

import Color.OneDark as OneDark
import Element exposing (Color)


type alias ColorTheme =
    { name : String
    , background : Color
    , text : Color
    , weakText : Color
    , accent1 : Color
    , accent2 : Color
    }


lightTheme : ColorTheme
lightTheme =
    { name = "Light Theme"
    , background = OneDark.white
    , text = OneDark.black
    , weakText = OneDark.gutterGrey
    , accent1 = OneDark.darkRed
    , accent2 = OneDark.darkYellow
    }


darkTheme : ColorTheme
darkTheme =
    { name = "Dark Theme"
    , background = OneDark.black
    , text = OneDark.white
    , weakText = OneDark.commentGrey
    , accent1 = OneDark.lightRed
    , accent2 = OneDark.lightYellow
    }
