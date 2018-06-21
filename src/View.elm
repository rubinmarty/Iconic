module View exposing (..)

import Types exposing (..)

import Html exposing (Html)
import Html.Attributes as Html exposing (style)
import Html.Events as Html
import Array exposing (Array)


boxSize = 150
boxBorderThickness = 2
selectedBorderExtraThickness = 3

boxesOuterWidth w =
    (boxSize + boxBorderThickness * 2) * w

sidebarCollapsedWidth = 40
sidebarExpandedWidth = 450
sidebarExpanderMargin = 2

pixels : Int -> String
pixels x =
    toString x ++ "px"


scoreToString score =
       "Your score: "
    ++ (toString score.right)
    ++ "/"
    ++ (toString (score.right + score.wrong))

scoreBanner : Model -> Html a
scoreBanner model =
    Html.p [] [Html.text <| scoreToString <| score model]

pressKeyFooter =
    Html.p [] [Html.i [] [Html.text "(Press any key to continue.)"]]

introMessage : Array String
introMessage =
    Array.fromList
        [ "Iconic memory is the brain's ability to perfectly recall large amounts of visual memory for a very brief period of time."
        , "This program utilizes the \"partial report\" paradigm for testing iconic memory, based off of a similar procedure used by George Sperling in 1960."
        , "After a few seconds, letters will flash on your screen. Press the key corresponding to the letter in the red box."
        ]
    

introModeView : Int -> Html a
introModeView index =
    let
        myStyle =
            style
                [ ("margin", "0px 80px")
                , ("font-size", "40px")
                , ("text-align", "center")
                , ("display", "inline-block")
                ]
        body =
            Html.p [myStyle] [Html.text <| Maybe.withDefault "" (Array.get index introMessage)]
    in
        Html.div [] [body, Html.br [] [], pressKeyFooter]

boxesModeView : Model -> Stage -> Html a
boxesModeView model stage =
    let
        ( w , h ) =
            model.dimensions

        isCorrect i =
            i == model.index

        text ch =
            case stage of
                Letters ->
                    String.fromChar ch
                _ ->
                    "?"

        css =
            [ ("width", pixels boxSize)
            , ("height", pixels boxSize)
            , ("line-height", pixels boxSize)
            , ("font-size", pixels boxSize)
            , ("display", "inline-block")
            , ("border-color", "black")
            , ("border-style", "solid")
            , ("border-width", pixels boxBorderThickness)
            , ("margin", pixels (-boxBorderThickness // 2))
            , ("text-align", "center")
            , ("position", "relative")
            ]

        invisible =
            [ ("color", "transparent") ]

        selectedCss i =
            if isCorrect i then
                [ ("border-color", "red")
                , ("border-width", pixels (boxBorderThickness + selectedBorderExtraThickness))
                , ("margin", pixels (-boxBorderThickness // 2 - selectedBorderExtraThickness))
                , ("z-index", "1")
                ]
            else []

        myStyle i =
            style (
                case stage of
                    Blank ->
                        css ++ invisible
                    Letters ->
                        css
                    Waiting ->
                        css ++ invisible
                    Guess ->
                        selectedCss i ++ css ++ invisible ++ selectedCss i
            )

        toDiv i ch =
            Html.div [myStyle i] [Html.text <| text ch]

        outerStyle w =
            style
                [ ("width", pixels <| boxesOuterWidth w)
                , ("margin", "0 auto")
                ]

        divs =
            List.indexedMap toDiv (Array.toList (model.data))
    in
        Html.div [outerStyle w] (divs ++ [scoreBanner model])
        

resultModeView : Model -> Html a
resultModeView model =
    let
        
        textcolor =
            case model.recentCorrect of
                Just b ->
                    if b then "Green" else "Red"
                Nothing ->
                    ""
        message =
            case model.recentCorrect of
                Just b ->
                    if b then "Correct" else "Incorrect"
                Nothing ->
                    ""
        message2 =
            scoreToString (score model)
        body =
            Html.h1 [style [("color", textcolor), ("font-size", "40px")]] [Html.text message]

    in
        Html.div [] [body, scoreBanner model, pressKeyFooter]

sidebar : Model -> Html Msg
sidebar model =
    let
        width = 
            if model.sideBarExpanded then sidebarExpandedWidth else sidebarCollapsedWidth
        rotationAmount =
            if model.sideBarExpanded then "315deg" else "135deg"
        event =
            if model.sideBarExpanded then NoOp else SetSidebarExpansion True
        borderColor =
            if model.sideBarExpanded then "rgb(85, 85, 85)" else "transparent"
        backgroundColor =
            if model.sideBarExpanded then "#333" else "rgb(102, 102, 102)"
        arrowStyle =
            style
                [ ("border", "solid white")
                , ("border-width", "0 3px 3px 0")
                , ("padding", "3px")
                , ("-webkit-transform", "rotate(" ++ rotationAmount ++ ")")
                , ("transform", "rotate(" ++ rotationAmount ++ ")")
                , ("width", pixels 0)
                , ("margin", "12px auto")
                , ("transition", "all .5s ease")
                ]
        arrow =
            Html.div [arrowStyle] []
        expanderStyle =
            style
                [ ("width", pixels <| width - sidebarExpanderMargin * 2)
                , ("height", pixels <| sidebarCollapsedWidth - sidebarExpanderMargin * 2)
                , ("margin", pixels sidebarExpanderMargin)
                , ("border", "1px solid " ++ borderColor)
                , ("background-color", "rgb(102, 102, 102)")
                , ("border-radius", pixels <| sidebarCollapsedWidth // 2)
                , ("transition", "all .5s ease")
                ]
        expander =
            Html.div [expanderStyle, Html.onClick <| SetSidebarExpansion <| not model.sideBarExpanded] [arrow]
        sidebarStyle =
            style
                [ ("right", "0px")
                , ("top", "0px")
                , ("height", "100%")
                , ("width", pixels width)
                , ("position", "fixed")
                , ("background-color", backgroundColor)
                , ("border-left", "1px solid black")
                , ("border-right", "1px solid black")
                , ("transition", "all .5s ease")
                ]

        delaySlider =
            Html.input
                [ Html.type_ "range"
                , Html.min "0"
                , Html.max "100"
                , Html.step "50"
                , Html.defaultValue "0"
                , Html.title "Set the amount of time between the letters disappearing and the red square appearing."
                , Html.onInput <| SetWaitingTime << Result.withDefault 0 << String.toFloat
                , style [("width", "100%")]
                ]
                [ Html.text "Delay Amount" ]

        sizeSlider =
            Html.input
                [ Html.type_ "range"
                , Html.min "3"
                , Html.max "5"
                , Html.step "1"
                , Html.defaultValue "4"
                , Html.title "Set the width of the grid of boxes."
                , Html.onInput <| SetGridWidth << Result.withDefault 0 << String.toInt
                , style [("width", "100%")]
                ]
                [ Html.text "Delay Amount" ]

        delaySliderText =
            "Set delay:"

        sizeSliderText =
            "Set grid size:"

        bodyStyle =
            style
                [ ("margin-left", pixels sidebarCollapsedWidth)
                , ("margin-right", pixels sidebarCollapsedWidth)
                ]
        body =
            Html.div [bodyStyle] [Html.text delaySliderText, delaySlider, Html.text sizeSliderText, sizeSlider]
    in
        Html.div [sidebarStyle, Html.onClick event] [expander, body]

view : Model -> Html Msg
view model =

    let
        mainStyle =
            style
                [ ("text-align", "center")
                ]
        header =
            Html.h1 [] [Html.text "Iconic Memory"]
        body =
            case model.mode of
                IntroMode i ->
                    introModeView i
                BoxesMode stage ->
                    boxesModeView model stage
                ResultMode ->
                    resultModeView model
                SettingsMode ->
                    Html.text "Settings Mode"
                StatsMode ->
                    Html.text "Stats Mode"
        main =
            Html.div [mainStyle, Html.onClick <| SetSidebarExpansion False] [header, body]
    in
        Html.div [] [main, sidebar model]