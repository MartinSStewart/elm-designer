module Codecs exposing
    ( documentCodec
    , encodeFontWeight
    , fontFamilyCodec
    , fontWeightCodec
    , fontWeightDecoder
    , localCodec
    , viewportCodec
    )

{-| Serialize and deserialize a document to JSON.
-}

import Document exposing (..)
import Element exposing (Color, Orientation(..))
import Fonts
import Html.Events as E
import Json.Decode as D
import Json.Encode
import Serialize exposing (Codec, Error)
import Style.Background
import Style.Border exposing (..)
import Style.Font exposing (..)
import Style.Layout exposing (..)
import Time exposing (Posix)
import Tree exposing (Tree)
import UUID exposing (UUID)


encodeFontWeight : FontWeight -> Json.Encode.Value
encodeFontWeight value =
    Serialize.encodeToJson fontWeightCodec value


fontWeightDecoder : (FontWeight -> msg) -> D.Decoder msg
fontWeightDecoder tagger =
    E.targetValue
        |> D.andThen
            (D.decodeString D.value
                >> Result.andThen (Serialize.decodeFromJson fontWeightCodec)
                >> fromResult
            )
        |> D.map tagger


fromResult : Result e a -> D.Decoder a
fromResult result =
    case result of
        Ok value ->
            D.succeed value

        Err _ ->
            D.fail "Cannot decode value object"


documentCodec : Codec UUID.Error Document
documentCodec =
    Serialize.record Document
        |> Serialize.field .schemaVersion Serialize.int
        |> Serialize.field .lastUpdatedOn posixCodec
        |> Serialize.field .pages (Serialize.list (treeCodec nodeCodec))
        |> Serialize.field .viewport viewportCodec
        |> Serialize.field .collapsedTreeItems (Serialize.set Serialize.string)
        |> Serialize.finishRecord


treeCodec : Codec e a -> Codec e (Tree a)
treeCodec codecA =
    Serialize.record Tree.tree
        |> Serialize.field Tree.label codecA
        |> Serialize.field Tree.children (Serialize.list (treeCodec codecA))
        |> Serialize.finishRecord


posixCodec : Codec e Time.Posix
posixCodec =
    Serialize.int |> Serialize.map Time.millisToPosix Time.posixToMillis


{-| Unlike other styles we just need to serialize the font family name here.
-}
fontFamilyCodec : Codec e FontFamily
fontFamilyCodec =
    Serialize.string
        |> Serialize.map
            (\name ->
                -- From JSON
                Fonts.findFamily name
            )
            (\family ->
                -- To JSON
                family.name
            )


fontWeightCodec : Codec e FontWeight
fontWeightCodec =
    Serialize.customType
        (\a b c d e f g h i j k l m n o p q r value ->
            case value of
                Heavy ->
                    a

                HeavyItalic ->
                    b

                ExtraBold ->
                    c

                ExtraBoldItalic ->
                    d

                Bold ->
                    e

                BoldItalic ->
                    f

                SemiBold ->
                    g

                SemiBoldItalic ->
                    h

                Medium ->
                    i

                MediumItalic ->
                    j

                Regular ->
                    k

                Italic ->
                    l

                Light ->
                    m

                LightItalic ->
                    n

                ExtraLight ->
                    o

                ExtraLightItalic ->
                    p

                Hairline ->
                    q

                HairlineItalic ->
                    r
        )
        |> Serialize.variant0 Heavy
        |> Serialize.variant0 HeavyItalic
        |> Serialize.variant0 ExtraBold
        |> Serialize.variant0 ExtraBoldItalic
        |> Serialize.variant0 Bold
        |> Serialize.variant0 BoldItalic
        |> Serialize.variant0 SemiBold
        |> Serialize.variant0 SemiBoldItalic
        |> Serialize.variant0 Medium
        |> Serialize.variant0 MediumItalic
        |> Serialize.variant0 Regular
        |> Serialize.variant0 Italic
        |> Serialize.variant0 Light
        |> Serialize.variant0 LightItalic
        |> Serialize.variant0 ExtraLight
        |> Serialize.variant0 ExtraLightItalic
        |> Serialize.variant0 Hairline
        |> Serialize.variant0 HairlineItalic
        |> Serialize.finishCustomType


type alias Rgba =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


viewportCodec : Codec e Viewport
viewportCodec =
    Serialize.customType
        (\a b c value ->
            case value of
                DeviceModel data0 ->
                    a data0

                Custom data0 data1 data2 ->
                    b data0 data1 data2

                Fluid ->
                    c
        )
        |> Serialize.variant1 DeviceModel Serialize.string
        |> Serialize.variant3 Custom Serialize.int Serialize.int orientationCodec
        |> Serialize.variant0 Fluid
        |> Serialize.finishCustomType


orientationCodec : Codec e Orientation
orientationCodec =
    Serialize.customType
        (\a b value ->
            case value of
                Portrait ->
                    a

                Landscape ->
                    b
        )
        |> Serialize.variant0 Portrait
        |> Serialize.variant0 Landscape
        |> Serialize.finishCustomType


labelPositionCodec : Codec e LabelPosition
labelPositionCodec =
    Serialize.customType
        (\a b c d e value ->
            case value of
                LabelAbove ->
                    a

                LabelBelow ->
                    b

                LabelLeft ->
                    c

                LabelRight ->
                    d

                LabelHidden ->
                    e
        )
        |> Serialize.variant0 LabelAbove
        |> Serialize.variant0 LabelBelow
        |> Serialize.variant0 LabelLeft
        |> Serialize.variant0 LabelRight
        |> Serialize.variant0 LabelHidden
        |> Serialize.finishCustomType


labelDataCodec : Codec e LabelData
labelDataCodec =
    Serialize.record LabelData
        |> Serialize.field .text Serialize.string
        |> Serialize.field .position labelPositionCodec
        |> Serialize.finishRecord


imageDataCodec : Codec e ImageData
imageDataCodec =
    Serialize.record ImageData
        |> Serialize.field .src Serialize.string
        |> Serialize.field .description Serialize.string
        |> Serialize.finishRecord


rowDataCodec : Codec e RowData
rowDataCodec =
    Serialize.record RowData
        |> Serialize.field .wrapped Serialize.bool
        |> Serialize.finishRecord


textDataCodec : Codec e TextData
textDataCodec =
    Serialize.record TextData
        |> Serialize.field .text Serialize.string
        |> Serialize.finishRecord


headingDataCodec : Codec e HeadingData
headingDataCodec =
    Serialize.record HeadingData
        |> Serialize.field .text Serialize.string
        |> Serialize.field .level Serialize.int
        |> Serialize.finishRecord


nodeTypeCodec : Codec e NodeType
nodeTypeCodec =
    Serialize.customType
        (\a b c d e f g h i j k l m n value ->
            case value of
                HeadingNode data0 ->
                    a data0

                ParagraphNode data0 ->
                    b data0

                TextNode data0 ->
                    c data0

                RowNode data0 ->
                    d data0

                ColumnNode ->
                    e

                TextColumnNode ->
                    f

                ImageNode data0 ->
                    g data0

                ButtonNode data0 ->
                    h data0

                CheckboxNode data0 ->
                    i data0

                TextFieldNode data0 ->
                    j data0

                TextFieldMultilineNode data0 ->
                    k data0

                RadioNode data0 ->
                    l data0

                OptionNode data0 ->
                    m data0

                PageNode ->
                    n
        )
        |> Serialize.variant1 HeadingNode headingDataCodec
        |> Serialize.variant1 ParagraphNode textDataCodec
        |> Serialize.variant1 TextNode textDataCodec
        |> Serialize.variant1 RowNode rowDataCodec
        |> Serialize.variant0 ColumnNode
        |> Serialize.variant0 TextColumnNode
        |> Serialize.variant1 ImageNode imageDataCodec
        |> Serialize.variant1 ButtonNode textDataCodec
        |> Serialize.variant1 CheckboxNode labelDataCodec
        |> Serialize.variant1 TextFieldNode labelDataCodec
        |> Serialize.variant1 TextFieldMultilineNode labelDataCodec
        |> Serialize.variant1 RadioNode labelDataCodec
        |> Serialize.variant1 OptionNode textDataCodec
        |> Serialize.variant0 PageNode
        |> Serialize.finishCustomType


alignmentCodec : Codec e Alignment
alignmentCodec =
    Serialize.customType
        (\a b c d value ->
            case value of
                Center ->
                    a

                Start ->
                    b

                End ->
                    c

                None ->
                    d
        )
        |> Serialize.variant0 Center
        |> Serialize.variant0 Start
        |> Serialize.variant0 End
        |> Serialize.variant0 None
        |> Serialize.finishCustomType


backgroundCodec : Codec e Style.Background.Background
backgroundCodec =
    Serialize.customType
        (\a b c d value ->
            case value of
                Style.Background.Cropped data0 ->
                    a data0

                Style.Background.Uncropped data0 ->
                    b data0

                Style.Background.Tiled data0 ->
                    c data0

                Style.Background.None ->
                    d
        )
        |> Serialize.variant1 Style.Background.Cropped Serialize.string
        |> Serialize.variant1 Style.Background.Uncropped Serialize.string
        |> Serialize.variant1 Style.Background.Tiled Serialize.string
        |> Serialize.variant0 Style.Background.None
        |> Serialize.finishCustomType


borderCornerCodec : Codec e BorderCorner
borderCornerCodec =
    Serialize.record BorderCorner
        |> Serialize.field .locked Serialize.bool
        |> Serialize.field .topLeft Serialize.int
        |> Serialize.field .topRight Serialize.int
        |> Serialize.field .bottomRight Serialize.int
        |> Serialize.field .bottomLeft Serialize.int
        |> Serialize.finishRecord


borderWidthCodec : Codec e BorderWidth
borderWidthCodec =
    Serialize.record BorderWidth
        |> Serialize.field .locked Serialize.bool
        |> Serialize.field .top Serialize.int
        |> Serialize.field .right Serialize.int
        |> Serialize.field .bottom Serialize.int
        |> Serialize.field .left Serialize.int
        |> Serialize.finishRecord


borderStyleCodec : Codec e BorderStyle
borderStyleCodec =
    Serialize.customType
        (\a b c value ->
            case value of
                Solid ->
                    a

                Dashed ->
                    b

                Dotted ->
                    c
        )
        |> Serialize.variant0 Solid
        |> Serialize.variant0 Dashed
        |> Serialize.variant0 Dotted
        |> Serialize.finishCustomType


textAlignmentCodec : Codec e TextAlignment
textAlignmentCodec =
    Serialize.customType
        (\a b c d value ->
            case value of
                TextLeft ->
                    a

                TextCenter ->
                    b

                TextRight ->
                    c

                TextJustify ->
                    d
        )
        |> Serialize.variant0 TextLeft
        |> Serialize.variant0 TextCenter
        |> Serialize.variant0 TextRight
        |> Serialize.variant0 TextJustify
        |> Serialize.finishCustomType


fontTypeCodec : Codec e FontType
fontTypeCodec =
    Serialize.customType
        (\a b value ->
            case value of
                Native data0 ->
                    a data0

                External data0 ->
                    b data0
        )
        |> Serialize.variant1 Native (Serialize.list Serialize.string)
        |> Serialize.variant1 External Serialize.string
        |> Serialize.finishCustomType


localCodec : Codec e a -> Codec e (Local a)
localCodec codecA =
    Serialize.customType
        (\a b value ->
            case value of
                Local data0 ->
                    a data0

                Inherit ->
                    b
        )
        |> Serialize.variant1 Local codecA
        |> Serialize.variant0 Inherit
        |> Serialize.finishCustomType


spacingCodec : Codec e Spacing
spacingCodec =
    Serialize.customType
        (\a b value ->
            case value of
                SpaceEvenly ->
                    a

                Spacing data0 ->
                    b data0
        )
        |> Serialize.variant0 SpaceEvenly
        |> Serialize.variant1 Spacing (Serialize.tuple Serialize.int Serialize.int)
        |> Serialize.finishCustomType


paddingCodec : Codec e Padding
paddingCodec =
    Serialize.record Padding
        |> Serialize.field .locked Serialize.bool
        |> Serialize.field .top Serialize.int
        |> Serialize.field .right Serialize.int
        |> Serialize.field .bottom Serialize.int
        |> Serialize.field .left Serialize.int
        |> Serialize.finishRecord


transformationCodec : Codec e Transformation
transformationCodec =
    Serialize.record Transformation
        |> Serialize.field .offsetX Serialize.float
        |> Serialize.field .offsetY Serialize.float
        |> Serialize.field .rotation Serialize.float
        |> Serialize.field .scale Serialize.float
        |> Serialize.finishRecord


strategyCodec : Codec e Strategy
strategyCodec =
    Serialize.customType
        (\a b c d value ->
            case value of
                Px data0 ->
                    a data0

                Content ->
                    b

                Fill data0 ->
                    c data0

                Unspecified ->
                    d
        )
        |> Serialize.variant1 Px Serialize.int
        |> Serialize.variant0 Content
        |> Serialize.variant1 Fill Serialize.int
        |> Serialize.variant0 Unspecified
        |> Serialize.finishCustomType


lengthCodec : Codec e Length
lengthCodec =
    Serialize.record Length
        |> Serialize.field .strategy strategyCodec
        |> Serialize.field .min (Serialize.maybe Serialize.int)
        |> Serialize.field .max (Serialize.maybe Serialize.int)
        |> Serialize.finishRecord


nodeCodec : Codec UUID.Error Node
nodeCodec =
    Serialize.record Node
        |> Serialize.field .id uuidCodec
        |> Serialize.field .name Serialize.string
        |> Serialize.field .width lengthCodec
        |> Serialize.field .height lengthCodec
        |> Serialize.field .transformation transformationCodec
        |> Serialize.field .padding paddingCodec
        |> Serialize.field .spacing spacingCodec
        |> Serialize.field .fontFamily (localCodec fontFamilyCodec)
        |> Serialize.field .fontColor (localCodec colorCodec)
        |> Serialize.field .fontSize (localCodec Serialize.int)
        |> Serialize.field .fontWeight fontWeightCodec
        |> Serialize.field .letterSpacing Serialize.float
        |> Serialize.field .wordSpacing Serialize.float
        |> Serialize.field .textAlignment textAlignmentCodec
        |> Serialize.field .borderColor colorCodec
        |> Serialize.field .borderStyle borderStyleCodec
        |> Serialize.field .borderWidth borderWidthCodec
        |> Serialize.field .borderCorner borderCornerCodec
        |> Serialize.field .backgroundColor (Serialize.maybe colorCodec)
        |> Serialize.field .background backgroundCodec
        |> Serialize.field .alignmentX alignmentCodec
        |> Serialize.field .alignmentY alignmentCodec
        |> Serialize.field .type_ nodeTypeCodec
        |> Serialize.finishRecord


uuidCodec : Codec UUID.Error UUID
uuidCodec =
    Serialize.string |> Serialize.mapValid UUID.fromString UUID.toString


colorCodec : Codec e Color
colorCodec =
    Serialize.record Rgba
        |> Serialize.field .red Serialize.float
        |> Serialize.field .green Serialize.float
        |> Serialize.field .blue Serialize.float
        |> Serialize.field .alpha Serialize.float
        |> Serialize.finishRecord
        |> Serialize.map Element.fromRgb Element.toRgb
