module Coalmine.Plotting.Types where

import Coalmine.Prelude
import qualified Data.Attoparsec.Text as Atto
import Domain
import DomainAeson
import DomainCereal
import DomainOptics

declare
  Nothing
  ( mconcat
      [ enumDeriver,
        boundedDeriver,
        showDeriver,
        eqDeriver,
        ordDeriver,
        genericDeriver,
        accessorIsLabelDeriver,
        constructorIsLabelDeriver,
        labelOpticDeriver,
        toJsonDeriver,
        serializeDeriver
      ]
  )
  =<< loadSchema "domain/plotting.domain.yaml"

instance NFData Diagram

instance NFData Scale

instance NFData ScaleTime

instance NFData ChartsLayout

instance NFData ChartsLayoutBinary

instance NFData Charts

instance NFData Chart

instance NFData Colour

instance LenientParser Colour where
  lenientParser =
    asum
      [ AliceblueColour <$ Atto.asciiCI "aliceblue",
        AntiquewhiteColour <$ Atto.asciiCI "antiquewhite",
        AquaColour <$ Atto.asciiCI "aqua",
        AquamarineColour <$ Atto.asciiCI "aquamarine",
        AzureColour <$ Atto.asciiCI "azure",
        BeigeColour <$ Atto.asciiCI "beige",
        BisqueColour <$ Atto.asciiCI "bisque",
        BlackColour <$ Atto.asciiCI "black",
        BlanchedalmondColour <$ Atto.asciiCI "blanchedalmond",
        BlueColour <$ Atto.asciiCI "blue",
        BluevioletColour <$ Atto.asciiCI "blueviolet",
        BrownColour <$ Atto.asciiCI "brown",
        BurlywoodColour <$ Atto.asciiCI "burlywood",
        CadetblueColour <$ Atto.asciiCI "cadetblue",
        ChartreuseColour <$ Atto.asciiCI "chartreuse",
        ChocolateColour <$ Atto.asciiCI "chocolate",
        CoralColour <$ Atto.asciiCI "coral",
        CornflowerblueColour <$ Atto.asciiCI "cornflowerblue",
        CornsilkColour <$ Atto.asciiCI "cornsilk",
        CrimsonColour <$ Atto.asciiCI "crimson",
        CyanColour <$ Atto.asciiCI "cyan",
        DarkblueColour <$ Atto.asciiCI "darkblue",
        DarkcyanColour <$ Atto.asciiCI "darkcyan",
        DarkgoldenrodColour <$ Atto.asciiCI "darkgoldenrod",
        DarkgrayColour <$ Atto.asciiCI "darkgray",
        DarkgreenColour <$ Atto.asciiCI "darkgreen",
        DarkgreyColour <$ Atto.asciiCI "darkgrey",
        DarkkhakiColour <$ Atto.asciiCI "darkkhaki",
        DarkmagentaColour <$ Atto.asciiCI "darkmagenta",
        DarkolivegreenColour <$ Atto.asciiCI "darkolivegreen",
        DarkorangeColour <$ Atto.asciiCI "darkorange",
        DarkorchidColour <$ Atto.asciiCI "darkorchid",
        DarkredColour <$ Atto.asciiCI "darkred",
        DarksalmonColour <$ Atto.asciiCI "darksalmon",
        DarkseagreenColour <$ Atto.asciiCI "darkseagreen",
        DarkslateblueColour <$ Atto.asciiCI "darkslateblue",
        DarkslategrayColour <$ Atto.asciiCI "darkslategray",
        DarkslategreyColour <$ Atto.asciiCI "darkslategrey",
        DarkturquoiseColour <$ Atto.asciiCI "darkturquoise",
        DarkvioletColour <$ Atto.asciiCI "darkviolet",
        DeeppinkColour <$ Atto.asciiCI "deeppink",
        DeepskyblueColour <$ Atto.asciiCI "deepskyblue",
        DimgrayColour <$ Atto.asciiCI "dimgray",
        DimgreyColour <$ Atto.asciiCI "dimgrey",
        DodgerblueColour <$ Atto.asciiCI "dodgerblue",
        FirebrickColour <$ Atto.asciiCI "firebrick",
        FloralwhiteColour <$ Atto.asciiCI "floralwhite",
        ForestgreenColour <$ Atto.asciiCI "forestgreen",
        FuchsiaColour <$ Atto.asciiCI "fuchsia",
        GainsboroColour <$ Atto.asciiCI "gainsboro",
        GhostwhiteColour <$ Atto.asciiCI "ghostwhite",
        GoldColour <$ Atto.asciiCI "gold",
        GoldenrodColour <$ Atto.asciiCI "goldenrod",
        GrayColour <$ Atto.asciiCI "gray",
        GreenColour <$ Atto.asciiCI "green",
        GreenyellowColour <$ Atto.asciiCI "greenyellow",
        GreyColour <$ Atto.asciiCI "grey",
        HoneydewColour <$ Atto.asciiCI "honeydew",
        HotpinkColour <$ Atto.asciiCI "hotpink",
        IndianredColour <$ Atto.asciiCI "indianred",
        IndigoColour <$ Atto.asciiCI "indigo",
        IvoryColour <$ Atto.asciiCI "ivory",
        KhakiColour <$ Atto.asciiCI "khaki",
        LavenderColour <$ Atto.asciiCI "lavender",
        LavenderblushColour <$ Atto.asciiCI "lavenderblush",
        LawngreenColour <$ Atto.asciiCI "lawngreen",
        LemonchiffonColour <$ Atto.asciiCI "lemonchiffon",
        LightblueColour <$ Atto.asciiCI "lightblue",
        LightcoralColour <$ Atto.asciiCI "lightcoral",
        LightcyanColour <$ Atto.asciiCI "lightcyan",
        LightgoldenrodyellowColour <$ Atto.asciiCI "lightgoldenrodyellow",
        LightgrayColour <$ Atto.asciiCI "lightgray",
        LightgreenColour <$ Atto.asciiCI "lightgreen",
        LightgreyColour <$ Atto.asciiCI "lightgrey",
        LightpinkColour <$ Atto.asciiCI "lightpink",
        LightsalmonColour <$ Atto.asciiCI "lightsalmon",
        LightseagreenColour <$ Atto.asciiCI "lightseagreen",
        LightskyblueColour <$ Atto.asciiCI "lightskyblue",
        LightslategrayColour <$ Atto.asciiCI "lightslategray",
        LightslategreyColour <$ Atto.asciiCI "lightslategrey",
        LightsteelblueColour <$ Atto.asciiCI "lightsteelblue",
        LightyellowColour <$ Atto.asciiCI "lightyellow",
        LimeColour <$ Atto.asciiCI "lime",
        LimegreenColour <$ Atto.asciiCI "limegreen",
        LinenColour <$ Atto.asciiCI "linen",
        MagentaColour <$ Atto.asciiCI "magenta",
        MaroonColour <$ Atto.asciiCI "maroon",
        MediumaquamarineColour <$ Atto.asciiCI "mediumaquamarine",
        MediumblueColour <$ Atto.asciiCI "mediumblue",
        MediumorchidColour <$ Atto.asciiCI "mediumorchid",
        MediumpurpleColour <$ Atto.asciiCI "mediumpurple",
        MediumseagreenColour <$ Atto.asciiCI "mediumseagreen",
        MediumslateblueColour <$ Atto.asciiCI "mediumslateblue",
        MediumspringgreenColour <$ Atto.asciiCI "mediumspringgreen",
        MediumturquoiseColour <$ Atto.asciiCI "mediumturquoise",
        MediumvioletredColour <$ Atto.asciiCI "mediumvioletred",
        MidnightblueColour <$ Atto.asciiCI "midnightblue",
        MintcreamColour <$ Atto.asciiCI "mintcream",
        MistyroseColour <$ Atto.asciiCI "mistyrose",
        MoccasinColour <$ Atto.asciiCI "moccasin",
        NavajowhiteColour <$ Atto.asciiCI "navajowhite",
        NavyColour <$ Atto.asciiCI "navy",
        OldlaceColour <$ Atto.asciiCI "oldlace",
        OliveColour <$ Atto.asciiCI "olive",
        OlivedrabColour <$ Atto.asciiCI "olivedrab",
        OrangeColour <$ Atto.asciiCI "orange",
        OrangeredColour <$ Atto.asciiCI "orangered",
        OrchidColour <$ Atto.asciiCI "orchid",
        PalegoldenrodColour <$ Atto.asciiCI "palegoldenrod",
        PalegreenColour <$ Atto.asciiCI "palegreen",
        PaleturquoiseColour <$ Atto.asciiCI "paleturquoise",
        PalevioletredColour <$ Atto.asciiCI "palevioletred",
        PapayawhipColour <$ Atto.asciiCI "papayawhip",
        PeachpuffColour <$ Atto.asciiCI "peachpuff",
        PeruColour <$ Atto.asciiCI "peru",
        PinkColour <$ Atto.asciiCI "pink",
        PlumColour <$ Atto.asciiCI "plum",
        PowderblueColour <$ Atto.asciiCI "powderblue",
        PurpleColour <$ Atto.asciiCI "purple",
        RedColour <$ Atto.asciiCI "red",
        RosybrownColour <$ Atto.asciiCI "rosybrown",
        RoyalblueColour <$ Atto.asciiCI "royalblue",
        SaddlebrownColour <$ Atto.asciiCI "saddlebrown",
        SalmonColour <$ Atto.asciiCI "salmon",
        SandybrownColour <$ Atto.asciiCI "sandybrown",
        SeagreenColour <$ Atto.asciiCI "seagreen",
        SeashellColour <$ Atto.asciiCI "seashell",
        SiennaColour <$ Atto.asciiCI "sienna",
        SilverColour <$ Atto.asciiCI "silver",
        SkyblueColour <$ Atto.asciiCI "skyblue",
        SlateblueColour <$ Atto.asciiCI "slateblue",
        SlategrayColour <$ Atto.asciiCI "slategray",
        SlategreyColour <$ Atto.asciiCI "slategrey",
        SnowColour <$ Atto.asciiCI "snow",
        SpringgreenColour <$ Atto.asciiCI "springgreen",
        SteelblueColour <$ Atto.asciiCI "steelblue",
        TanColour <$ Atto.asciiCI "tan",
        TealColour <$ Atto.asciiCI "teal",
        ThistleColour <$ Atto.asciiCI "thistle",
        TomatoColour <$ Atto.asciiCI "tomato",
        TurquoiseColour <$ Atto.asciiCI "turquoise",
        VioletColour <$ Atto.asciiCI "violet",
        WheatColour <$ Atto.asciiCI "wheat",
        WhiteColour <$ Atto.asciiCI "white",
        WhitesmokeColour <$ Atto.asciiCI "whitesmoke",
        YellowColour <$ Atto.asciiCI "yellow",
        YellowgreenColour <$ Atto.asciiCI "yellowgreen"
      ]
