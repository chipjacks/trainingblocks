module MonoIcons exposing (add, archive, arrowDown, arrowLeft, arrowLeftDown, arrowLeftUp, arrowRight, arrowRightDown, arrowRightUp, arrowUp, attachment, backspace, ban, barChart, barChartAlt, board, book, bookmark, calendar, call, camera, caretDown, caretLeft, caretRight, caretUp, check, chevronDoubleDown, chevronDoubleLeft, chevronDoubleRight, chevronDoubleUp, chevronDown, chevronLeft, chevronRight, chevronUp, circle, circleAdd, circleArrowDown, circleArrowLeft, circleArrowRight, circleArrowUp, circleCheck, circleError, circleHelp, circleInformation, circleRemove, circleWarning, clipboard, clipboardCheck, clipboardList, clock, close, cloud, cloudDownload, cloudUpload, computer, copy, creditCard, delete, deleteAlt, document, documentAdd, documentCheck, documentDownload, documentEmpty, documentRemove, download, drag, edit, editAlt, email, enter, expand, export, externalLink, eye, eyeOff, favorite, filter, filter1, filterAlt, folder, folderAdd, folderCheck, folderDownload, folderRemove, grid, heart, home, icon, image, inbox, laptop, link, linkAlt, list, location, lock, logOut, map, megaphone, menu, message, messageAlt, mobile, moon, next, notification, notificationOff, optionsHorizontal, optionsVertical, pause, percentage, pin, play, previous, refresh, remove, repeat, search, select, send, settings, share, shoppingCart, shoppingCartAdd, shuffle, sort, speakers, stop, sun, sunrise, switch, table, tablet, tag, undo, unlock, user, userAdd, userCheck, userRemove, users, volumeOff, volumeUp, warning, webcam, zoomIn, zoomOut)

{-| Generated from <https://github.com/mono-company/mono-icons>
-}

import Html.Attributes
import Html exposing (Html)
import Svg
import Svg.Attributes

icon : Svg.Svg msg -> Html msg
icon svg =
    Svg.svg [ Svg.Attributes.width "1.2em", Svg.Attributes.height "1.2em", Svg.Attributes.viewBox "0 0 24 24" ] [ svg ]


add : String -> Svg.Svg msg
add fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4a1 1 0 0 1 1 1v6h6a1 1 0 1 1 0 2h-6v6a1 1 0 1 1-2 0v-6H5a1 1 0 1 1 0-2h6V5a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


archive : String -> Svg.Svg msg
archive fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M2 5a2 2 0 0 1 2-2h16a2 2 0 0 1 2 2v2a2 2 0 0 1-1.017 1.742c.011.084.017.17.017.258v10a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V9c0-.087.006-.174.017-.258A2 2 0 0 1 2 7V5zm18 2V5H4v2h16zM5 9v10h14V9H5zm3 3a1 1 0 0 1 1-1h6a1 1 0 1 1 0 2H9a1 1 0 0 1-1-1z", Html.Attributes.attribute "fill" fill ] [] ]


arrowDown : String -> Svg.Svg msg
arrowDown fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4a1 1 0 0 1 1 1v11.586l4.293-4.293a1 1 0 0 1 1.414 1.414l-6 6a1 1 0 0 1-1.414 0l-6-6a1 1 0 1 1 1.414-1.414L11 16.586V5a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


arrowLeftDown : String -> Svg.Svg msg
arrowLeftDown fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M17.707 6.293a1 1 0 0 1 0 1.414L9.414 16H15a1 1 0 1 1 0 2H7a1 1 0 0 1-1-1V9a1 1 0 1 1 2 0v5.586l8.293-8.293a1 1 0 0 1 1.414 0z", Html.Attributes.attribute "fill" fill ] [] ]


arrowLeftUp : String -> Svg.Svg msg
arrowLeftUp fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M8 9.414V15a1 1 0 1 1-2 0V7a1 1 0 0 1 1-1h8a1 1 0 1 1 0 2H9.414l8.293 8.293a1 1 0 0 1-1.414 1.414L8 9.414z", Html.Attributes.attribute "fill" fill ] [] ]


arrowLeft : String -> Svg.Svg msg
arrowLeft fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M11.707 5.293a1 1 0 0 1 0 1.414L7.414 11H19a1 1 0 1 1 0 2H7.414l4.293 4.293a1 1 0 0 1-1.414 1.414l-6-6a1 1 0 0 1 0-1.414l6-6a1 1 0 0 1 1.414 0z", Html.Attributes.attribute "fill" fill ] [] ]


arrowRightDown : String -> Svg.Svg msg
arrowRightDown fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M6.293 6.293a1 1 0 0 1 1.414 0L16 14.586V9a1 1 0 1 1 2 0v8a1 1 0 0 1-1 1H9a1 1 0 1 1 0-2h5.586L6.293 7.707a1 1 0 0 1 0-1.414z", Html.Attributes.attribute "fill" fill ] [] ]


arrowRightUp : String -> Svg.Svg msg
arrowRightUp fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M8 7a1 1 0 0 1 1-1h8a1 1 0 0 1 1 1v8a1 1 0 1 1-2 0V9.414l-8.293 8.293a1 1 0 0 1-1.414-1.414L14.586 8H9a1 1 0 0 1-1-1z", Html.Attributes.attribute "fill" fill ] [] ]


arrowRight : String -> Svg.Svg msg
arrowRight fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12.293 5.293a1 1 0 0 1 1.414 0l6 6a1 1 0 0 1 0 1.414l-6 6a1 1 0 0 1-1.414-1.414L16.586 13H5a1 1 0 1 1 0-2h11.586l-4.293-4.293a1 1 0 0 1 0-1.414z", Html.Attributes.attribute "fill" fill ] [] ]


arrowUp : String -> Svg.Svg msg
arrowUp fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4a1 1 0 0 1 .707.293l6 6a1 1 0 0 1-1.414 1.414L13 7.414V19a1 1 0 1 1-2 0V7.414l-4.293 4.293a1 1 0 0 1-1.414-1.414l6-6A1 1 0 0 1 12 4z", Html.Attributes.attribute "fill" fill ] [] ]


attachment : String -> Svg.Svg msg
attachment fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M19.463 5.576c-.688-.75-1.929-.796-2.756.031l-8.1 8.1c-.21.21-.21.476 0 .686.21.21.476.21.686 0l6.7-6.7a1 1 0 0 1 1.414 1.414l-6.7 6.7a2.45 2.45 0 0 1-3.514 0 2.45 2.45 0 0 1 0-3.514l8.1-8.1c1.567-1.568 4.115-1.619 5.63.015 1.552 1.569 1.597 4.104-.03 5.613l-9.486 9.486c-2.19 2.19-5.624 2.19-7.814 0-2.19-2.19-2.19-5.624 0-7.814l8.1-8.1a1 1 0 0 1 1.414 1.414l-8.1 8.1c-1.41 1.41-1.41 3.576 0 4.986 1.41 1.41 3.576 1.41 4.986 0l9.5-9.5.031-.03c.75-.687.796-1.929-.031-2.756l-.03-.031z", Html.Attributes.attribute "fill" fill ] [] ]


backspace : String -> Svg.Svg msg
backspace fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M17.707 8.293a1 1 0 0 1 0 1.414L15.414 12l2.293 2.293a1 1 0 0 1-1.414 1.414L14 13.414l-2.293 2.293a1 1 0 0 1-1.414-1.414L12.586 12l-2.293-2.293a1 1 0 1 1 1.414-1.414L14 10.586l2.293-2.293a1 1 0 0 1 1.414 0z", Html.Attributes.attribute "fill" fill ] [], Svg.node "path" [ Html.Attributes.attribute "fill-rule" "evenodd", Html.Attributes.attribute "clip-rule" "evenodd", Html.Attributes.attribute "d" "M22 5a1 1 0 0 0-1-1H9.46a2 2 0 0 0-1.519.698l-5.142 6a2 2 0 0 0 0 2.604l5.142 6A2 2 0 0 0 9.46 20H21a1 1 0 0 0 1-1V5zm-2 13H9.46l-5.143-6L9.46 6H20v12z", Html.Attributes.attribute "fill" fill ] [] ]


ban : String -> Svg.Svg msg
ban fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M5.68 7.094A8 8 0 0 0 16.905 18.32L5.68 7.094zM7.094 5.68L18.32 16.906A8 8 0 0 0 7.094 5.68zM2 12C2 6.477 6.477 2 12 2s10 4.477 10 10-4.477 10-10 10S2 17.523 2 12z", Html.Attributes.attribute "fill" fill ] [] ]


barChartAlt : String -> Svg.Svg msg
barChartAlt fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4a1 1 0 0 1 1 1v14a1 1 0 1 1-2 0V5a1 1 0 0 1 1-1zm5 4a1 1 0 0 1 1 1v10a1 1 0 1 1-2 0V9a1 1 0 0 1 1-1zM7 12a1 1 0 0 1 1 1v6a1 1 0 1 1-2 0v-6a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


barChart : String -> Svg.Svg msg
barChart fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M3 5a2 2 0 0 1 2-2h14a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V5zm16 0H5v14h14V5zm-7 2a1 1 0 0 1 1 1v8a1 1 0 1 1-2 0V8a1 1 0 0 1 1-1zm4 2a1 1 0 0 1 1 1v6a1 1 0 1 1-2 0v-6a1 1 0 0 1 1-1zm-8 2a1 1 0 0 1 1 1v4a1 1 0 1 1-2 0v-4a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


board : String -> Svg.Svg msg
board fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M2 5a2 2 0 0 1 2-2h16a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V5zm6 0H4v14h4V5zm2 0v14h4V5h-4zm6 0v14h4V5h-4z", Html.Attributes.attribute "fill" fill ] [] ]


book : String -> Svg.Svg msg
book fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M4 6.633c.14-.056.308-.118.503-.181A9.77 9.77 0 0 1 7.5 6a9.77 9.77 0 0 1 2.997.452c.195.063.363.125.503.181v10.88A11.817 11.817 0 0 0 7.5 17c-1.46 0-2.649.248-3.5.513V6.633zm8-1.748a9.257 9.257 0 0 0-.888-.337A11.769 11.769 0 0 0 7.5 4c-1.526 0-2.755.271-3.612.548a8.889 8.889 0 0 0-1.001.389 5.905 5.905 0 0 0-.357.18l-.025.014-.009.005-.003.002h-.001c-.002.002-.247.147-.002.002A1 1 0 0 0 2 6v13a1 1 0 0 0 1.51.86l-.005.003h.001l.002-.001.001-.001.037-.02c.037-.02.098-.05.182-.09.17-.078.43-.188.775-.3A9.77 9.77 0 0 1 7.5 19a9.77 9.77 0 0 1 2.997.451 6.9 6.9 0 0 1 .775.3 3.976 3.976 0 0 1 .223.112m0 0h-.001l-.002-.001-.001-.001c.314.185.704.185 1.018 0l.037-.02c.037-.02.098-.05.182-.09a6.9 6.9 0 0 1 .775-.3A9.77 9.77 0 0 1 16.5 19a9.77 9.77 0 0 1 2.997.451 6.9 6.9 0 0 1 .775.3 3.976 3.976 0 0 1 .219.11A1 1 0 0 0 22 19V6a1 1 0 0 0-.49-.86l-.002-.001h-.001l-.003-.003-.01-.005-.024-.014a5.883 5.883 0 0 0-.357-.18 8.897 8.897 0 0 0-1-.389A11.769 11.769 0 0 0 16.5 4c-1.525 0-2.755.271-3.612.548a9.112 9.112 0 0 0-.888.337m8 1.748v10.88A11.817 11.817 0 0 0 16.5 17c-1.46 0-2.649.248-3.5.513V6.633c.14-.056.308-.118.503-.181A9.77 9.77 0 0 1 16.5 6a9.77 9.77 0 0 1 2.997.452c.195.063.363.125.503.181zm.49.228l.005.002h-.001l-.003-.002zm0 13l.004.002-.002-.002", Html.Attributes.attribute "fill" fill ] [] ]


bookmark : String -> Svg.Svg msg
bookmark fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M4 4a2 2 0 0 1 2-2h12a2 2 0 0 1 2 2v17a1 1 0 0 1-1.581.814L12 17.229l-6.419 4.585A1 1 0 0 1 4 21V4zm14 0H6v15.057l5.419-3.87a1 1 0 0 1 1.162 0L18 19.056V4z", Html.Attributes.attribute "fill" fill ] [] ]


calendar : String -> Svg.Svg msg
calendar fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M9 2a1 1 0 0 1 1 1v1h4V3a1 1 0 1 1 2 0v1h3a2 2 0 0 1 2 2v13a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h3V3a1 1 0 0 1 1-1zM8 6H5v3h14V6h-3v1a1 1 0 1 1-2 0V6h-4v1a1 1 0 0 1-2 0V6zm11 5H5v8h14v-8z", Html.Attributes.attribute "fill" fill ] [] ]


call : String -> Svg.Svg msg
call fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M3.833 4h4.49L9.77 7.618l-2.325 1.55A1 1 0 0 0 7 10c.003.094 0 .001 0 .001v.021a2.026 2.026 0 0 0 .006.134c.006.082.016.193.035.33.039.27.114.642.26 1.08.294.88.87 2.019 1.992 3.141 1.122 1.122 2.261 1.698 3.14 1.992.439.146.81.22 1.082.26a4.43 4.43 0 0 0 .463.04l.013.001h.008s.112-.006.001 0a1 1 0 0 0 .894-.553l.67-1.34 4.436.74v4.32c-2.111.305-7.813.606-12.293-3.874C3.227 11.813 3.527 6.11 3.833 4zm5.24 6.486l1.807-1.204a2 2 0 0 0 .747-2.407L10.18 3.257A2 2 0 0 0 8.323 2H3.781c-.909 0-1.764.631-1.913 1.617-.34 2.242-.801 8.864 4.425 14.09 5.226 5.226 11.848 4.764 14.09 4.425.986-.15 1.617-1.004 1.617-1.913v-4.372a2 2 0 0 0-1.671-1.973l-4.436-.739a2 2 0 0 0-2.118 1.078l-.346.693a4.71 4.71 0 0 1-.363-.105c-.62-.206-1.481-.63-2.359-1.508-.878-.878-1.302-1.739-1.508-2.36a4.59 4.59 0 0 1-.125-.447z", Html.Attributes.attribute "fill" fill ] [] ]


camera : String -> Svg.Svg msg
camera fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M8.293 4.293A1 1 0 0 1 9 4h6a1 1 0 0 1 .707.293L17.414 6H20a2 2 0 0 1 2 2v10a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h2.586l1.707-1.707zM9.414 6L7.707 7.707A1 1 0 0 1 7 8H4v10h16V8h-3a1 1 0 0 1-.707-.293L14.586 6H9.414zM12 10.5a2 2 0 1 0 0 4 2 2 0 0 0 0-4zm-4 2a4 4 0 1 1 8 0 4 4 0 0 1-8 0z", Html.Attributes.attribute "fill" fill ] [] ]


caretDown : String -> Svg.Svg msg
caretDown fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M17 10l-5 6-5-6h10z", Html.Attributes.attribute "fill" fill ] [] ]


caretLeft : String -> Svg.Svg msg
caretLeft fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M14 17l-6-5 6-5v10z", Html.Attributes.attribute "fill" fill ] [] ]


caretRight : String -> Svg.Svg msg
caretRight fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M10 7l6 5-6 5V7z", Html.Attributes.attribute "fill" fill ] [] ]


caretUp : String -> Svg.Svg msg
caretUp fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M7 14l5-6 5 6H7z", Html.Attributes.attribute "fill" fill ] [] ]


check : String -> Svg.Svg msg
check fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M20.664 5.253a1 1 0 0 1 .083 1.411l-10.666 12a1 1 0 0 1-1.495 0l-5.333-6a1 1 0 0 1 1.494-1.328l4.586 5.159 9.92-11.16a1 1 0 0 1 1.411-.082z", Html.Attributes.attribute "fill" fill ] [] ]


chevronDoubleDown : String -> Svg.Svg msg
chevronDoubleDown fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M5.293 6.293a1 1 0 0 1 1.414 0L12 11.586l5.293-5.293a1 1 0 1 1 1.414 1.414l-6 6a1 1 0 0 1-1.414 0l-6-6a1 1 0 0 1 0-1.414zm0 6a1 1 0 0 1 1.414 0L12 17.586l5.293-5.293a1 1 0 0 1 1.414 1.414l-6 6a1 1 0 0 1-1.414 0l-6-6a1 1 0 0 1 0-1.414z", Html.Attributes.attribute "fill" fill ] [] ]


chevronDoubleLeft : String -> Svg.Svg msg
chevronDoubleLeft fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M17.707 5.293a1 1 0 0 1 0 1.414L12.414 12l5.293 5.293a1 1 0 0 1-1.414 1.414l-6-6a1 1 0 0 1 0-1.414l6-6a1 1 0 0 1 1.414 0zm-6 0a1 1 0 0 1 0 1.414L6.414 12l5.293 5.293a1 1 0 0 1-1.414 1.414l-6-6a1 1 0 0 1 0-1.414l6-6a1 1 0 0 1 1.414 0z", Html.Attributes.attribute "fill" fill ] [] ]


chevronDoubleRight : String -> Svg.Svg msg
chevronDoubleRight fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12.293 5.293a1 1 0 0 1 1.414 0l6 6a1 1 0 0 1 0 1.414l-6 6a1 1 0 0 1-1.414-1.414L17.586 12l-5.293-5.293a1 1 0 0 1 0-1.414zm-6 0a1 1 0 0 1 1.414 0l6 6a1 1 0 0 1 0 1.414l-6 6a1 1 0 0 1-1.414-1.414L11.586 12 6.293 6.707a1 1 0 0 1 0-1.414z", Html.Attributes.attribute "fill" fill ] [] ]


chevronDoubleUp : String -> Svg.Svg msg
chevronDoubleUp fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M11.293 4.293a1 1 0 0 1 1.414 0l6 6a1 1 0 0 1-1.414 1.414L12 6.414l-5.293 5.293a1 1 0 0 1-1.414-1.414l6-6zM12 12.414l-5.293 5.293a1 1 0 0 1-1.414-1.414l6-6a1 1 0 0 1 1.414 0l6 6a1 1 0 0 1-1.414 1.414L12 12.414z", Html.Attributes.attribute "fill" fill ] [] ]


chevronDown : String -> Svg.Svg msg
chevronDown fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M5.293 9.293a1 1 0 0 1 1.414 0L12 14.586l5.293-5.293a1 1 0 1 1 1.414 1.414l-6 6a1 1 0 0 1-1.414 0l-6-6a1 1 0 0 1 0-1.414z", Html.Attributes.attribute "fill" fill ] [] ]


chevronLeft : String -> Svg.Svg msg
chevronLeft fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M14.707 5.293a1 1 0 0 1 0 1.414L9.414 12l5.293 5.293a1 1 0 0 1-1.414 1.414l-6-6a1 1 0 0 1 0-1.414l6-6a1 1 0 0 1 1.414 0z", Html.Attributes.attribute "fill" fill ] [] ]


chevronRight : String -> Svg.Svg msg
chevronRight fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M9.293 18.707a1 1 0 0 1 0-1.414L14.586 12 9.293 6.707a1 1 0 0 1 1.414-1.414l6 6a1 1 0 0 1 0 1.414l-6 6a1 1 0 0 1-1.414 0z", Html.Attributes.attribute "fill" fill ] [] ]


chevronUp : String -> Svg.Svg msg
chevronUp fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M11.293 7.293a1 1 0 0 1 1.414 0l6 6a1 1 0 0 1-1.414 1.414L12 9.414l-5.293 5.293a1 1 0 0 1-1.414-1.414l6-6z", Html.Attributes.attribute "fill" fill ] [] ]


circleAdd : String -> Svg.Svg msg
circleAdd fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4a8 8 0 1 0 0 16 8 8 0 0 0 0-16zM2 12C2 6.477 6.477 2 12 2s10 4.477 10 10-4.477 10-10 10S2 17.523 2 12zm10-5a1 1 0 0 1 1 1v3h3a1 1 0 1 1 0 2h-3v3a1 1 0 1 1-2 0v-3H8a1 1 0 1 1 0-2h3V8a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


circleArrowDown : String -> Svg.Svg msg
circleArrowDown fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4a8 8 0 1 0 0 16 8 8 0 0 0 0-16zM2 12C2 6.477 6.477 2 12 2s10 4.477 10 10-4.477 10-10 10S2 17.523 2 12zm10-6a1 1 0 0 1 1 1v7.586l2.293-2.293a1 1 0 0 1 1.414 1.414l-4 4a1 1 0 0 1-1.414 0l-4-4a1 1 0 1 1 1.414-1.414L11 14.586V7a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


circleArrowLeft : String -> Svg.Svg msg
circleArrowLeft fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4a8 8 0 1 0 0 16 8 8 0 0 0 0-16zM2 12C2 6.477 6.477 2 12 2s10 4.477 10 10-4.477 10-10 10S2 17.523 2 12zm9.707-4.707a1 1 0 0 1 0 1.414L9.414 11H17a1 1 0 1 1 0 2H9.414l2.293 2.293a1 1 0 0 1-1.414 1.414l-4-4a1 1 0 0 1 0-1.414l4-4a1 1 0 0 1 1.414 0z", Html.Attributes.attribute "fill" fill ] [] ]


circleArrowRight : String -> Svg.Svg msg
circleArrowRight fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4a8 8 0 1 0 0 16 8 8 0 0 0 0-16zM2 12C2 6.477 6.477 2 12 2s10 4.477 10 10-4.477 10-10 10S2 17.523 2 12zm10.293-4.707a1 1 0 0 1 1.414 0l4 4a1 1 0 0 1 0 1.414l-4 4a1 1 0 0 1-1.414-1.414L14.586 13H7a1 1 0 1 1 0-2h7.586l-2.293-2.293a1 1 0 0 1 0-1.414z", Html.Attributes.attribute "fill" fill ] [] ]


circleArrowUp : String -> Svg.Svg msg
circleArrowUp fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4a8 8 0 1 0 0 16 8 8 0 0 0 0-16zM2 12C2 6.477 6.477 2 12 2s10 4.477 10 10-4.477 10-10 10S2 17.523 2 12zm9-2.586l-2.293 2.293a1 1 0 0 1-1.414-1.414l4-4a1 1 0 0 1 1.414 0l4 4a1 1 0 0 1-1.414 1.414L13 9.414V17a1 1 0 1 1-2 0V9.414z", Html.Attributes.attribute "fill" fill ] [] ]


circleCheck : String -> Svg.Svg msg
circleCheck fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4a8 8 0 1 0 0 16 8 8 0 0 0 0-16zM2 12C2 6.477 6.477 2 12 2s10 4.477 10 10-4.477 10-10 10S2 17.523 2 12zm14.664-3.247a1 1 0 0 1 .083 1.411l-5.333 6a1 1 0 0 1-1.495 0l-2.666-3a1 1 0 0 1 1.494-1.328l1.92 2.159 4.586-5.16a1 1 0 0 1 1.411-.082z", Html.Attributes.attribute "fill" fill ] [] ]


circleError : String -> Svg.Svg msg
circleError fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4a8 8 0 1 0 0 16 8 8 0 0 0 0-16zM2 12C2 6.477 6.477 2 12 2s10 4.477 10 10-4.477 10-10 10S2 17.523 2 12zm5.793-4.207a1 1 0 0 1 1.414 0L12 10.586l2.793-2.793a1 1 0 1 1 1.414 1.414L13.414 12l2.793 2.793a1 1 0 0 1-1.414 1.414L12 13.414l-2.793 2.793a1 1 0 0 1-1.414-1.414L10.586 12 7.793 9.207a1 1 0 0 1 0-1.414z", Html.Attributes.attribute "fill" fill ] [] ]


circleHelp : String -> Svg.Svg msg
circleHelp fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4a8 8 0 1 0 0 16 8 8 0 0 0 0-16zM2 12C2 6.477 6.477 2 12 2s10 4.477 10 10-4.477 10-10 10S2 17.523 2 12z", Html.Attributes.attribute "fill" fill ] [], Svg.node "path" [ Html.Attributes.attribute "d" "M12 14a1 1 0 0 1-1-1v-1a1 1 0 1 1 2 0v1a1 1 0 0 1-1 1zm-1.5 2.5a1.5 1.5 0 1 1 3 0 1.5 1.5 0 0 1-3 0z", Html.Attributes.attribute "fill" fill ] [], Svg.node "path" [ Html.Attributes.attribute "d" "M12.39 7.811c-.957-.045-1.76.49-1.904 1.353a1 1 0 0 1-1.972-.328c.356-2.136 2.303-3.102 3.971-3.022.854.04 1.733.347 2.409.979C15.587 7.44 16 8.368 16 9.5c0 1.291-.508 2.249-1.383 2.832-.803.535-1.788.668-2.617.668a1 1 0 1 1 0-2c.67 0 1.186-.117 1.508-.332.25-.167.492-.46.492-1.168 0-.618-.212-1.003-.472-1.246-.277-.259-.68-.42-1.138-.443z", Html.Attributes.attribute "fill" fill ] [] ]


circleInformation : String -> Svg.Svg msg
circleInformation fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4a8 8 0 1 0 0 16 8 8 0 0 0 0-16zM2 12C2 6.477 6.477 2 12 2s10 4.477 10 10-4.477 10-10 10S2 17.523 2 12z", Html.Attributes.attribute "fill" fill ] [], Svg.node "path" [ Html.Attributes.attribute "d" "M12 10a1 1 0 0 1 1 1v6a1 1 0 1 1-2 0v-6a1 1 0 0 1 1-1zm1.5-2.5a1.5 1.5 0 1 1-3 0 1.5 1.5 0 0 1 3 0z", Html.Attributes.attribute "fill" fill ] [] ]


circleRemove : String -> Svg.Svg msg
circleRemove fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4a8 8 0 1 0 0 16 8 8 0 0 0 0-16zM2 12C2 6.477 6.477 2 12 2s10 4.477 10 10-4.477 10-10 10S2 17.523 2 12zm5 0a1 1 0 0 1 1-1h8a1 1 0 1 1 0 2H8a1 1 0 0 1-1-1z", Html.Attributes.attribute "fill" fill ] [] ]


circleWarning : String -> Svg.Svg msg
circleWarning fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4a8 8 0 1 0 0 16 8 8 0 0 0 0-16zM2 12C2 6.477 6.477 2 12 2s10 4.477 10 10-4.477 10-10 10S2 17.523 2 12z", Html.Attributes.attribute "fill" fill ] [], Svg.node "path" [ Html.Attributes.attribute "d" "M12 14a1 1 0 0 1-1-1V7a1 1 0 1 1 2 0v6a1 1 0 0 1-1 1zm-1.5 2.5a1.5 1.5 0 1 1 3 0 1.5 1.5 0 0 1-3 0z", Html.Attributes.attribute "fill" fill ] [] ]


circle : String -> Svg.Svg msg
circle fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "fill-rule" "evenodd", Html.Attributes.attribute "clip-rule" "evenodd", Html.Attributes.attribute "d" "M12 4a8 8 0 1 0 0 16 8 8 0 0 0 0-16zM2 12C2 6.477 6.477 2 12 2s10 4.477 10 10-4.477 10-10 10S2 17.523 2 12z", Html.Attributes.attribute "fill" fill ] [] ]


clipboardCheck : String -> Svg.Svg msg
clipboardCheck fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M8 3a1 1 0 0 1 1-1h6a1 1 0 0 1 1 1h2a2 2 0 0 1 2 2v15a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h2zm0 2H6v15h12V5h-2v1a1 1 0 0 1-1 1H9a1 1 0 0 1-1-1V5zm6-1h-4v1h4V4zm1.707 6.793a1 1 0 0 1 0 1.414l-4 4a1 1 0 0 1-1.414 0l-2-2a1 1 0 1 1 1.414-1.414L11 14.086l3.293-3.293a1 1 0 0 1 1.414 0z", Html.Attributes.attribute "fill" fill ] [] ]


clipboardList : String -> Svg.Svg msg
clipboardList fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M9 11.5a1 1 0 1 0 0-2 1 1 0 0 0 0 2zm2-1a1 1 0 0 1 1-1h3a1 1 0 1 1 0 2h-3a1 1 0 0 1-1-1zm1 2a1 1 0 1 0 0 2h3a1 1 0 1 0 0-2h-3zm0 3a1 1 0 1 0 0 2h3a1 1 0 1 0 0-2h-3zm-2-2a1 1 0 1 1-2 0 1 1 0 0 1 2 0zm-1 4a1 1 0 1 0 0-2 1 1 0 0 0 0 2z", Html.Attributes.attribute "fill" fill ] [], Svg.node "path" [ Html.Attributes.attribute "d" "M9 2a1 1 0 0 0-1 1H6a2 2 0 0 0-2 2v15a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V5a2 2 0 0 0-2-2h-2a1 1 0 0 0-1-1H9zm7 3h2v15H6V5h2v1a1 1 0 0 0 1 1h6a1 1 0 0 0 1-1V5zm-6 0V4h4v1h-4z", Html.Attributes.attribute "fill" fill ] [] ]


clipboard : String -> Svg.Svg msg
clipboard fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M8 3a1 1 0 0 1 1-1h6a1 1 0 0 1 1 1h2a2 2 0 0 1 2 2v15a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h2zm0 2H6v15h12V5h-2v1a1 1 0 0 1-1 1H9a1 1 0 0 1-1-1V5zm6-1h-4v1h4V4z", Html.Attributes.attribute "fill" fill ] [] ]


clock : String -> Svg.Svg msg
clock fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4a8 8 0 1 0 0 16 8 8 0 0 0 0-16zM2 12C2 6.477 6.477 2 12 2s10 4.477 10 10-4.477 10-10 10S2 17.523 2 12zm10-6a1 1 0 0 1 1 1v4.586l2.707 2.707a1 1 0 0 1-1.414 1.414l-3-3A1 1 0 0 1 11 12V7a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


close : String -> Svg.Svg msg
close fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M5.293 5.293a1 1 0 0 1 1.414 0L12 10.586l5.293-5.293a1 1 0 1 1 1.414 1.414L13.414 12l5.293 5.293a1 1 0 0 1-1.414 1.414L12 13.414l-5.293 5.293a1 1 0 0 1-1.414-1.414L10.586 12 5.293 6.707a1 1 0 0 1 0-1.414z", Html.Attributes.attribute "fill" fill ] [] ]


cloudDownload : String -> Svg.Svg msg
cloudDownload fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M11 4a4 4 0 0 0-3.999 4.102 1 1 0 0 1-.75.992A3.002 3.002 0 0 0 7 15h1a1 1 0 1 1 0 2H7a5 5 0 0 1-1.97-9.596 6 6 0 0 1 11.169-2.4A6 6 0 0 1 16 17a1 1 0 1 1 0-2 4 4 0 1 0-.328-7.987 1 1 0 0 1-.999-.6A4.001 4.001 0 0 0 11 4zm1 6a1 1 0 0 1 1 1v7.586l.293-.293a1 1 0 0 1 1.414 1.414l-2 2a1 1 0 0 1-1.414 0l-2-2a1 1 0 1 1 1.414-1.414l.293.293V11a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


cloudUpload : String -> Svg.Svg msg
cloudUpload fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M11 4a4 4 0 0 0-3.999 4.102 1 1 0 0 1-.75.992A3.002 3.002 0 0 0 7 15h1a1 1 0 1 1 0 2H7a5 5 0 0 1-1.97-9.596 6 6 0 0 1 11.169-2.4A6 6 0 0 1 16 17a1 1 0 1 1 0-2 4 4 0 1 0-.328-7.987 1 1 0 0 1-.999-.6A4.001 4.001 0 0 0 11 4zm.293 5.293a1 1 0 0 1 1.414 0l2 2a1 1 0 0 1-1.414 1.414L13 12.414V20a1 1 0 1 1-2 0v-7.586l-.293.293a1 1 0 0 1-1.414-1.414l2-2z", Html.Attributes.attribute "fill" fill ] [] ]


cloud : String -> Svg.Svg msg
cloud fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M5 10a6 6 0 0 1 11.671-1.963A6 6 0 0 1 16 20H7a5 5 0 0 1-1.986-9.59A6.071 6.071 0 0 1 5 10zm6-4a4 4 0 0 0-3.903 4.879 1 1 0 0 1-.757 1.194A3.002 3.002 0 0 0 7 18h9a4 4 0 1 0-.08-8 1 1 0 0 1-1-.8A4.002 4.002 0 0 0 11 6z", Html.Attributes.attribute "fill" fill ] [] ]


computer : String -> Svg.Svg msg
computer fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M2 5a2 2 0 0 1 2-2h16a2 2 0 0 1 2 2v11a2 2 0 0 1-2 2h-7v2h3a1 1 0 1 1 0 2H8a1 1 0 1 1 0-2h3v-2H4a2 2 0 0 1-2-2V5zm18 11V5H4v11h16z", Html.Attributes.attribute "fill" fill ] [] ]


copy : String -> Svg.Svg msg
copy fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M2 4a2 2 0 0 1 2-2h10a2 2 0 0 1 2 2v4h4a2 2 0 0 1 2 2v10a2 2 0 0 1-2 2H10a2 2 0 0 1-2-2v-4H4a2 2 0 0 1-2-2V4zm8 12v4h10V10h-4v4a2 2 0 0 1-2 2h-4zm4-2V4H4v10h10z", Html.Attributes.attribute "fill" fill ] [] ]


creditCard : String -> Svg.Svg msg
creditCard fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M22 6a2 2 0 0 0-2-2H4a2 2 0 0 0-2 2v12a2 2 0 0 0 2 2h16a2 2 0 0 0 2-2V6zm-2 2H4V6h16v2zM4 11h16v7H4v-7z", Html.Attributes.attribute "fill" fill ] [] ]


deleteAlt : String -> Svg.Svg msg
deleteAlt fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M7 4a2 2 0 0 1 2-2h6a2 2 0 0 1 2 2v2h4a1 1 0 1 1 0 2h-1.069l-.867 12.142A2 2 0 0 1 17.069 22H6.93a2 2 0 0 1-1.995-1.858L4.07 8H3a1 1 0 0 1 0-2h4V4zm2 2h6V4H9v2zM6.074 8l.857 12H17.07l.857-12H6.074z", Html.Attributes.attribute "fill" fill ] [] ]


delete : String -> Svg.Svg msg
delete fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M7 4a2 2 0 0 1 2-2h6a2 2 0 0 1 2 2v2h4a1 1 0 1 1 0 2h-1.069l-.867 12.142A2 2 0 0 1 17.069 22H6.93a2 2 0 0 1-1.995-1.858L4.07 8H3a1 1 0 0 1 0-2h4V4zm2 2h6V4H9v2zM6.074 8l.857 12H17.07l.857-12H6.074zM10 10a1 1 0 0 1 1 1v6a1 1 0 1 1-2 0v-6a1 1 0 0 1 1-1zm4 0a1 1 0 0 1 1 1v6a1 1 0 1 1-2 0v-6a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


documentAdd : String -> Svg.Svg msg
documentAdd fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M4 4a2 2 0 0 1 2-2h8a1 1 0 0 1 .707.293l5 5A1 1 0 0 1 20 8v12a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V4zm13.586 4L14 4.414V8h3.586zM12 4H6v16h12V10h-5a1 1 0 0 1-1-1V4zm0 8a1 1 0 0 1 1 1v1h1a1 1 0 1 1 0 2h-1v1a1 1 0 1 1-2 0v-1h-1a1 1 0 1 1 0-2h1v-1a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


documentCheck : String -> Svg.Svg msg
documentCheck fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M4 4a2 2 0 0 1 2-2h8a1 1 0 0 1 .707.293l5 5A1 1 0 0 1 20 8v12a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V4zm13.586 4L14 4.414V8h3.586zM12 4H6v16h12V10h-5a1 1 0 0 1-1-1V4zm3.707 8.293a1 1 0 0 1 0 1.414l-4 4a1 1 0 0 1-1.414 0l-2-2a1 1 0 1 1 1.414-1.414L11 15.586l3.293-3.293a1 1 0 0 1 1.414 0z", Html.Attributes.attribute "fill" fill ] [] ]


documentDownload : String -> Svg.Svg msg
documentDownload fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M4 4a2 2 0 0 1 2-2h8a1 1 0 0 1 .707.293l5 5A1 1 0 0 1 20 8v12a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V4zm13.586 4L14 4.414V8h3.586zM12 4H6v16h12V10h-5a1 1 0 0 1-1-1V4zm0 7.5a1 1 0 0 1 1 1v2.586l.293-.293a1 1 0 0 1 1.414 1.414l-2 2a1 1 0 0 1-1.414 0l-2-2a1 1 0 1 1 1.414-1.414l.293.293V12.5a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


documentEmpty : String -> Svg.Svg msg
documentEmpty fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M4 4a2 2 0 0 1 2-2h8a1 1 0 0 1 .707.293l5 5A1 1 0 0 1 20 8v12a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V4zm13.586 4L14 4.414V8h3.586zM12 4H6v16h12V10h-5a1 1 0 0 1-1-1V4z", Html.Attributes.attribute "fill" fill ] [] ]


documentRemove : String -> Svg.Svg msg
documentRemove fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M4 4a2 2 0 0 1 2-2h8a1 1 0 0 1 .707.293l5 5A1 1 0 0 1 20 8v12a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V4zm13.586 4L14 4.414V8h3.586zM12 4H6v16h12V10h-5a1 1 0 0 1-1-1V4zM9 15a1 1 0 0 1 1-1h4a1 1 0 1 1 0 2h-4a1 1 0 0 1-1-1z", Html.Attributes.attribute "fill" fill ] [] ]


document : String -> Svg.Svg msg
document fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M4 4a2 2 0 0 1 2-2h8a1 1 0 0 1 .707.293l5 5A1 1 0 0 1 20 8v12a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V4zm13.586 4L14 4.414V8h3.586zM12 4H6v16h12V10h-5a1 1 0 0 1-1-1V4zm-4 9a1 1 0 0 1 1-1h6a1 1 0 1 1 0 2H9a1 1 0 0 1-1-1zm0 4a1 1 0 0 1 1-1h6a1 1 0 1 1 0 2H9a1 1 0 0 1-1-1z", Html.Attributes.attribute "fill" fill ] [] ]


download : String -> Svg.Svg msg
download fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 2a1 1 0 0 1 1 1v10.586l2.293-2.293a1 1 0 0 1 1.414 1.414l-4 4a1 1 0 0 1-1.414 0l-4-4a1 1 0 1 1 1.414-1.414L11 13.586V3a1 1 0 0 1 1-1zM5 17a1 1 0 0 1 1 1v2h12v-2a1 1 0 1 1 2 0v2a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2v-2a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


drag : String -> Svg.Svg msg
drag fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M4 6a1 1 0 0 1 1-1h14a1 1 0 1 1 0 2H5a1 1 0 0 1-1-1zm0 4a1 1 0 0 1 1-1h14a1 1 0 1 1 0 2H5a1 1 0 0 1-1-1zm0 4a1 1 0 0 1 1-1h14a1 1 0 1 1 0 2H5a1 1 0 0 1-1-1zm0 4a1 1 0 0 1 1-1h14a1 1 0 1 1 0 2H5a1 1 0 0 1-1-1z", Html.Attributes.attribute "fill" fill ] [] ]


editAlt : String -> Svg.Svg msg
editAlt fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M16.293 3.293a1 1 0 0 1 1.414 0l3 3a1 1 0 0 1 0 1.414l-9 9A1 1 0 0 1 11 17H8a1 1 0 0 1-1-1v-3a1 1 0 0 1 .293-.707l9-9zM9 13.414V15h1.586l8-8L17 5.414l-8 8zM3 7a2 2 0 0 1 2-2h5a1 1 0 1 1 0 2H5v12h12v-5a1 1 0 1 1 2 0v5a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V7z", Html.Attributes.attribute "fill" fill ] [] ]


edit : String -> Svg.Svg msg
edit fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M16.293 2.293a1 1 0 0 1 1.414 0l4 4a1 1 0 0 1 0 1.414l-13 13A1 1 0 0 1 8 21H4a1 1 0 0 1-1-1v-4a1 1 0 0 1 .293-.707l10-10 3-3zM14 7.414l-9 9V19h2.586l9-9L14 7.414zm4 1.172L19.586 7 17 4.414 15.414 6 18 8.586z", Html.Attributes.attribute "fill" fill ] [] ]


email : String -> Svg.Svg msg
email fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M2 6a2 2 0 0 1 2-2h16a2 2 0 0 1 2 2v12a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V6zm3.519 0L12 11.671 18.481 6H5.52zM20 7.329l-7.341 6.424a1 1 0 0 1-1.318 0L4 7.329V18h16V7.329z", Html.Attributes.attribute "fill" fill ] [] ]


enter : String -> Svg.Svg msg
enter fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "fill-rule" "evenodd", Html.Attributes.attribute "clip-rule" "evenodd", Html.Attributes.attribute "d" "M3 14a1 1 0 0 1 1-1h12a3 3 0 0 0 3-3V6a1 1 0 1 1 2 0v4a5 5 0 0 1-5 5H4a1 1 0 0 1-1-1z", Html.Attributes.attribute "fill" fill ] [], Svg.node "path" [ Html.Attributes.attribute "fill-rule" "evenodd", Html.Attributes.attribute "clip-rule" "evenodd", Html.Attributes.attribute "d" "M3.293 14.707a1 1 0 0 1 0-1.414l4-4a1 1 0 0 1 1.414 1.414L5.414 14l3.293 3.293a1 1 0 1 1-1.414 1.414l-4-4z", Html.Attributes.attribute "fill" fill ] [] ]


expand : String -> Svg.Svg msg
expand fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M6 7.414V9a1 1 0 1 1-2 0V5a1 1 0 0 1 1-1h4a1 1 0 1 1 0 2H7.414l2.293 2.293a1 1 0 0 1-1.414 1.414L6 7.414zM15 6a1 1 0 1 1 0-2h4a1 1 0 0 1 1 1v4a1 1 0 1 1-2 0V7.414l-2.293 2.293a1 1 0 0 1-1.414-1.414L16.586 6H15zM5 14a1 1 0 0 1 1 1v1.586l2.293-2.293a1 1 0 0 1 1.414 1.414L7.414 18H9a1 1 0 1 1 0 2H5a1 1 0 0 1-1-1v-4a1 1 0 0 1 1-1zm9.293 1.707a1 1 0 0 1 1.414-1.414L18 16.586V15a1 1 0 1 1 2 0v4a1 1 0 0 1-1 1h-4a1 1 0 1 1 0-2h1.586l-2.293-2.293z", Html.Attributes.attribute "fill" fill ] [] ]


export : String -> Svg.Svg msg
export fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M11.293 2.293a1 1 0 0 1 1.414 0l4 4a1 1 0 0 1-1.414 1.414L13 5.414V16a1 1 0 1 1-2 0V5.414L8.707 7.707a1 1 0 0 1-1.414-1.414l4-4zM5 17a1 1 0 0 1 1 1v2h12v-2a1 1 0 1 1 2 0v2a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2v-2a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


externalLink : String -> Svg.Svg msg
externalLink fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M14 5a1 1 0 1 1 0-2h6a1 1 0 0 1 1 1v6a1 1 0 1 1-2 0V6.414l-9.293 9.293a1 1 0 0 1-1.414-1.414L17.586 5H14zM3 7a2 2 0 0 1 2-2h5a1 1 0 1 1 0 2H5v12h12v-5a1 1 0 1 1 2 0v5a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V7z", Html.Attributes.attribute "fill" fill ] [] ]


eyeOff : String -> Svg.Svg msg
eyeOff fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M4.707 3.293a1 1 0 0 0-1.414 1.414l2.424 2.424c-1.43 1.076-2.678 2.554-3.611 4.422a1 1 0 0 0 0 .894C4.264 16.764 8.096 19 12 19c1.555 0 3.1-.355 4.53-1.055l2.763 2.762a1 1 0 0 0 1.414-1.414l-16-16zm10.307 13.135c-.98.383-2 .572-3.014.572-2.969 0-6.002-1.62-7.87-5 .817-1.479 1.858-2.62 3.018-3.437l2.144 2.144a3 3 0 0 0 4.001 4.001l1.72 1.72zm3.538-2.532c.483-.556.926-1.187 1.318-1.896-1.868-3.38-4.9-5-7.87-5-.112 0-.224.002-.336.007L9.879 5.223A10.215 10.215 0 0 1 12 5c3.903 0 7.736 2.236 9.894 6.553a1 1 0 0 1 0 .894 13.106 13.106 0 0 1-1.925 2.865l-1.417-1.416z", Html.Attributes.attribute "fill" fill ] [] ]


eye : String -> Svg.Svg msg
eye fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M15 12a3 3 0 1 1-6 0 3 3 0 0 1 6 0z", Html.Attributes.attribute "fill" fill ] [], Svg.node "path" [ Html.Attributes.attribute "d" "M21.894 11.553C19.736 7.236 15.904 5 12 5c-3.903 0-7.736 2.236-9.894 6.553a1 1 0 0 0 0 .894C4.264 16.764 8.096 19 12 19c3.903 0 7.736-2.236 9.894-6.553a1 1 0 0 0 0-.894zM12 17c-2.969 0-6.002-1.62-7.87-5C5.998 8.62 9.03 7 12 7c2.969 0 6.002 1.62 7.87 5-1.868 3.38-4.901 5-7.87 5z", Html.Attributes.attribute "fill" fill ] [] ]


favorite : String -> Svg.Svg msg
favorite fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 2.5a1 1 0 0 1 .894.553l2.58 5.158 5.67.824a1 1 0 0 1 .554 1.706l-4.127 4.024.928 5.674a1 1 0 0 1-1.455 1.044L12 18.807l-5.044 2.676a1 1 0 0 1-1.455-1.044l.928-5.674-4.127-4.024a1 1 0 0 1 .554-1.706l5.67-.824 2.58-5.158A1 1 0 0 1 12 2.5zm0 3.236l-1.918 3.836a1 1 0 0 1-.75.543l-4.184.608 3.05 2.973a1 1 0 0 1 .289.878L7.8 18.771l3.731-1.98a1 1 0 0 1 .938 0l3.731 1.98-.687-4.197a1 1 0 0 1 .289-.877l3.05-2.974-4.183-.608a1 1 0 0 1-.75-.543L12 5.736z", Html.Attributes.attribute "fill" fill ] [] ]


filter1 : String -> Svg.Svg msg
filter1 fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M4 7a1 1 0 0 1 1-1h14a1 1 0 1 1 0 2H5a1 1 0 0 1-1-1zm2 5a1 1 0 0 1 1-1h10a1 1 0 1 1 0 2H7a1 1 0 0 1-1-1zm2 5a1 1 0 0 1 1-1h6a1 1 0 1 1 0 2H9a1 1 0 0 1-1-1z", Html.Attributes.attribute "fill" fill ] [] ]


filterAlt : String -> Svg.Svg msg
filterAlt fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M4 7a1 1 0 0 1 1-1h14a1 1 0 1 1 0 2H5a1 1 0 0 1-1-1zm2 5a1 1 0 0 1 1-1h10a1 1 0 1 1 0 2H7a1 1 0 0 1-1-1zm2 5a1 1 0 0 1 1-1h6a1 1 0 1 1 0 2H9a1 1 0 0 1-1-1z", Html.Attributes.attribute "fill" fill ] [] ]


filter : String -> Svg.Svg msg
filter fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M9 5a1 1 0 1 0 0 2 1 1 0 0 0 0-2zM6.17 5a3.001 3.001 0 0 1 5.66 0H19a1 1 0 1 1 0 2h-7.17a3.001 3.001 0 0 1-5.66 0H5a1 1 0 0 1 0-2h1.17zM15 11a1 1 0 1 0 0 2 1 1 0 0 0 0-2zm-2.83 0a3.001 3.001 0 0 1 5.66 0H19a1 1 0 1 1 0 2h-1.17a3.001 3.001 0 0 1-5.66 0H5a1 1 0 1 1 0-2h7.17zM9 17a1 1 0 1 0 0 2 1 1 0 0 0 0-2zm-2.83 0a3.001 3.001 0 0 1 5.66 0H19a1 1 0 1 1 0 2h-7.17a3.001 3.001 0 0 1-5.66 0H5a1 1 0 1 1 0-2h1.17z", Html.Attributes.attribute "fill" fill ] [] ]


folderAdd : String -> Svg.Svg msg
folderAdd fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M2 6a2 2 0 0 1 2-2h5a1 1 0 0 1 .707.293L11.414 6H20a2 2 0 0 1 2 2v10a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V6zm6.586 0H4v12h16V8h-9a1 1 0 0 1-.707-.293L8.586 6zM12 10a1 1 0 0 1 1 1v1h1a1 1 0 1 1 0 2h-1v1a1 1 0 1 1-2 0v-1h-1a1 1 0 1 1 0-2h1v-1a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


folderCheck : String -> Svg.Svg msg
folderCheck fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M2 6a2 2 0 0 1 2-2h5a1 1 0 0 1 .707.293L11.414 6H20a2 2 0 0 1 2 2v10a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V6zm6.586 0H4v12h16V8h-9a1 1 0 0 1-.707-.293L8.586 6zm7.121 4.293a1 1 0 0 1 0 1.414l-4 4a1 1 0 0 1-1.414 0l-2-2a1 1 0 1 1 1.414-1.414L11 13.586l3.293-3.293a1 1 0 0 1 1.414 0z", Html.Attributes.attribute "fill" fill ] [] ]


folderDownload : String -> Svg.Svg msg
folderDownload fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M2 6a2 2 0 0 1 2-2h5a1 1 0 0 1 .707.293L11.414 6H20a2 2 0 0 1 2 2v10a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V6zm6.586 0H4v12h16V8h-9a1 1 0 0 1-.707-.293L8.586 6zM12 9.5a1 1 0 0 1 1 1v2.586l.293-.293a1 1 0 0 1 1.414 1.414l-2 2a1 1 0 0 1-1.414 0l-2-2a1 1 0 1 1 1.414-1.414l.293.293V10.5a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


folderRemove : String -> Svg.Svg msg
folderRemove fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M2 6a2 2 0 0 1 2-2h5a1 1 0 0 1 .707.293L11.414 6H20a2 2 0 0 1 2 2v10a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V6zm6.586 0H4v12h16V8h-9a1 1 0 0 1-.707-.293L8.586 6zM9 13a1 1 0 0 1 1-1h4a1 1 0 1 1 0 2h-4a1 1 0 0 1-1-1z", Html.Attributes.attribute "fill" fill ] [] ]


folder : String -> Svg.Svg msg
folder fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M2 6a2 2 0 0 1 2-2h5a1 1 0 0 1 .707.293L11.414 6H20a2 2 0 0 1 2 2v10a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V6zm6.586 0H4v12h16V8h-9a1 1 0 0 1-.707-.293L8.586 6z", Html.Attributes.attribute "fill" fill ] [] ]


grid : String -> Svg.Svg msg
grid fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M3 5a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V5zm6 0H5v4h4V5zm4 0a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v4a2 2 0 0 1-2 2h-4a2 2 0 0 1-2-2V5zm6 0h-4v4h4V5zM3 15a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4zm6 0H5v4h4v-4zm4 0a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v4a2 2 0 0 1-2 2h-4a2 2 0 0 1-2-2v-4zm6 0h-4v4h4v-4z", Html.Attributes.attribute "fill" fill ] [] ]


heart : String -> Svg.Svg msg
heart fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4.528a6 6 0 0 0-8.243 8.715l6.829 6.828a2 2 0 0 0 2.828 0l6.829-6.828A6 6 0 0 0 12 4.528zm-1.172 1.644l.465.464a1 1 0 0 0 1.414 0l.465-.464a4 4 0 1 1 5.656 5.656L12 18.657l-6.828-6.829a4 4 0 0 1 5.656-5.656z", Html.Attributes.attribute "fill" fill ] [] ]


home : String -> Svg.Svg msg
home fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M11.336 2.253a1 1 0 0 1 1.328 0l9 8a1 1 0 0 1-1.328 1.494L20 11.45V19a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2v-7.55l-.336.297a1 1 0 0 1-1.328-1.494l9-8zM6 9.67V19h3v-5a1 1 0 0 1 1-1h4a1 1 0 0 1 1 1v5h3V9.671l-6-5.333-6 5.333zM13 19v-4h-2v4h2z", Html.Attributes.attribute "fill" fill ] [] ]


image : String -> Svg.Svg msg
image fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M15.5 10a1.5 1.5 0 1 0 0-3 1.5 1.5 0 0 0 0 3z", Html.Attributes.attribute "fill" fill ] [], Svg.node "path" [ Html.Attributes.attribute "d" "M3 5a2 2 0 0 1 2-2h14a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V5zm16 0H5v7.92l3.375-2.7a1 1 0 0 1 1.25 0l4.3 3.44 1.368-1.367a1 1 0 0 1 1.414 0L19 14.586V5zM5 19h14v-1.586l-3-3-1.293 1.293a1 1 0 0 1-1.332.074L9 12.28l-4 3.2V19z", Html.Attributes.attribute "fill" fill ] [] ]


inbox : String -> Svg.Svg msg
inbox fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M3 5a2 2 0 0 1 2-2h14a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V5zm2 9v5h14v-5h-2.28l-.771 2.316A1 1 0 0 1 15 17H9a1 1 0 0 1-.949-.684L7.28 14H5zm14-2V5H5v7h2.28a2 2 0 0 1 1.897 1.367L9.72 15h4.558l.544-1.633A2 2 0 0 1 16.721 12H19z", Html.Attributes.attribute "fill" fill ] [] ]


laptop : String -> Svg.Svg msg
laptop fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M2 5a2 2 0 0 1 2-2h16a2 2 0 0 1 2 2v11a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V5zm18 0H4v11h16V5zm2 15a1 1 0 0 1-1 1H3a1 1 0 1 1 0-2h18a1 1 0 0 1 1 1z", Html.Attributes.attribute "fill" fill ] [] ]


linkAlt : String -> Svg.Svg msg
linkAlt fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M16.2 6a1 1 0 0 1 1-1c.637 0 1.262.128 1.871.372.663.265 1.173.658 1.636 1.12.463.464.856.974 1.121 1.637.244.609.372 1.234.372 1.871s-.128 1.262-.372 1.871c-.261.655-.648 1.16-1.104 1.619l-.984 1.083a.967.967 0 0 1-.033.034l-3.2 3.2c-.463.463-.973.856-1.636 1.122A5.012 5.012 0 0 1 13 19.3a5.012 5.012 0 0 1-1.871-.372c-.663-.265-1.173-.658-1.636-1.12-.463-.464-.856-.974-1.121-1.637A5.145 5.145 0 0 1 8 14.2c0-.637.128-1.262.372-1.871.265-.663.658-1.173 1.12-1.636l1.1-1.1a1 1 0 1 1 1.415 1.414l-1.1 1.1c-.337.337-.544.627-.678.964A3.014 3.014 0 0 0 10 14.2c0 .476.077.85.229 1.229.134.337.341.627.678.964.337.337.627.544.964.678.391.157.766.229 1.129.229s.738-.072 1.129-.229c.337-.134.627-.341.964-.678l3.183-3.183.984-1.083a.967.967 0 0 1 .033-.034c.337-.337.544-.627.678-.964.157-.391.229-.766.229-1.129s-.072-.738-.229-1.129c-.134-.337-.341-.627-.678-.964-.337-.337-.627-.544-.964-.679A3.014 3.014 0 0 0 17.2 7a1 1 0 0 1-1-1zm-4.9 1.5c-.363 0-.738.072-1.129.228-.337.135-.627.342-.964.68L5.924 11.69l-.984 1.083-.033.034c-.337.337-.544.627-.679.964A3.014 3.014 0 0 0 4 14.9c0 .363.072.738.228 1.129.135.337.342.627.68.964.336.337.626.544.963.679.391.156.766.228 1.129.228a1 1 0 1 1 0 2 5.011 5.011 0 0 1-1.871-.371c-.663-.266-1.173-.659-1.636-1.122-.463-.463-.856-.973-1.121-1.636A5.012 5.012 0 0 1 2 14.9c0-.637.128-1.262.372-1.871.261-.655.648-1.16 1.104-1.619l.984-1.083.033-.034 3.3-3.3c.463-.463.973-.856 1.636-1.121A5.012 5.012 0 0 1 11.3 5.5c.637 0 1.262.128 1.871.372.663.265 1.173.658 1.636 1.12.463.464.856.974 1.121 1.637.244.609.372 1.234.372 1.871s-.128 1.262-.372 1.871c-.262.655-.649 1.162-1.105 1.62l-1.086 1.185a1 1 0 0 1-1.474-1.352l1.1-1.2.03-.031c.337-.337.544-.627.678-.964.157-.391.229-.766.229-1.129s-.072-.738-.229-1.129c-.134-.337-.341-.627-.678-.964-.337-.337-.627-.544-.964-.679A3.014 3.014 0 0 0 11.3 7.5z", Html.Attributes.attribute "fill" fill ] [] ]


link : String -> Svg.Svg msg
link fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M8 8c-2.248 0-4 1.752-4 4s1.752 4 4 4h2a1 1 0 1 1 0 2H8c-3.352 0-6-2.648-6-6s2.648-6 6-6h2a1 1 0 1 1 0 2H8zm5-1a1 1 0 0 1 1-1h2c3.352 0 6 2.648 6 6s-2.648 6-6 6h-2a1 1 0 1 1 0-2h2c2.248 0 4-1.752 4-4s-1.752-4-4-4h-2a1 1 0 0 1-1-1zm-6 5a1 1 0 0 1 1-1h8a1 1 0 1 1 0 2H8a1 1 0 0 1-1-1z", Html.Attributes.attribute "fill" fill ] [] ]


list : String -> Svg.Svg msg
list fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M4 7a1 1 0 0 1 1-1h1a1 1 0 0 1 0 2H5a1 1 0 0 1-1-1zm5 0a1 1 0 0 1 1-1h9a1 1 0 1 1 0 2h-9a1 1 0 0 1-1-1zm-5 5a1 1 0 0 1 1-1h1a1 1 0 1 1 0 2H5a1 1 0 0 1-1-1zm5 0a1 1 0 0 1 1-1h9a1 1 0 1 1 0 2h-9a1 1 0 0 1-1-1zm-5 5a1 1 0 0 1 1-1h1a1 1 0 1 1 0 2H5a1 1 0 0 1-1-1zm5 0a1 1 0 0 1 1-1h9a1 1 0 1 1 0 2h-9a1 1 0 0 1-1-1z", Html.Attributes.attribute "fill" fill ] [] ]


location : String -> Svg.Svg msg
location fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 2c-4.4 0-8 3.6-8 8 0 5.4 7 11.5 7.3 11.8.2.1.5.2.7.2.2 0 .5-.1.7-.2.3-.3 7.3-6.4 7.3-11.8 0-4.4-3.6-8-8-8zm0 17.7c-2.1-2-6-6.3-6-9.7 0-3.3 2.7-6 6-6s6 2.7 6 6-3.9 7.7-6 9.7zM12 6c-2.2 0-4 1.8-4 4s1.8 4 4 4 4-1.8 4-4-1.8-4-4-4zm0 6c-1.1 0-2-.9-2-2s.9-2 2-2 2 .9 2 2-.9 2-2 2z", Html.Attributes.attribute "fill" fill ] [] ]


lock : String -> Svg.Svg msg
lock fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4c1.648 0 3 1.352 3 3v3H9V7c0-1.648 1.352-3 3-3zm5 6V7c0-2.752-2.248-5-5-5S7 4.248 7 7v3H6a2 2 0 0 0-2 2v8a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2v-8a2 2 0 0 0-2-2h-1zM6 12h12v8H6v-8z", Html.Attributes.attribute "fill" fill ] [] ]


logOut : String -> Svg.Svg msg
logOut fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M2 6a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v2a1 1 0 1 1-2 0V6H4v12h9v-2a1 1 0 1 1 2 0v2a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V6zm15.293 2.293a1 1 0 0 1 1.414 0l3 3a1 1 0 0 1 0 1.414l-3 3a1 1 0 0 1-1.414-1.414L18.586 13H9a1 1 0 1 1 0-2h9.586l-1.293-1.293a1 1 0 0 1 0-1.414z", Html.Attributes.attribute "fill" fill ] [] ]


map : String -> Svg.Svg msg
map fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M8.684 3.051a1 1 0 0 1 .632 0L15 4.946l4.367-1.456A2 2 0 0 1 22 5.387V17.28a2 2 0 0 1-1.367 1.898l-5.317 1.772a1 1 0 0 1-.632 0L9 19.054 4.632 20.51A2 2 0 0 1 2 18.613V6.72a2 2 0 0 1 1.368-1.898L8.684 3.05zM10 17.28l4 1.334V6.72l-4-1.334V17.28zM8 5.387L4 6.721v11.892l4-1.334V5.387zm8 1.334v11.892l4-1.334V5.387l-4 1.334z", Html.Attributes.attribute "fill" fill ] [] ]


megaphone : String -> Svg.Svg msg
megaphone fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M17.502 2.135A1 1 0 0 1 18 3v4a3.99 3.99 0 0 1 2.981 1.333A3.989 3.989 0 0 1 22 11c0 1.024-.386 1.96-1.019 2.667A3.993 3.993 0 0 1 18 15v4a1 1 0 0 1-1.496.868L10 16.152V21a1 1 0 0 1-1 1H5a1 1 0 0 1-1-1v-5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h5.734l6.77-3.868a1 1 0 0 1 .998.003zM10 14a1 1 0 0 1 .496.132L16 17.277V4.723l-5.504 3.145A1 1 0 0 1 10 8H4v6h6zm-4 2v4h2v-4H6zm12-3c.592 0 1.123-.256 1.491-.667.317-.354.509-.82.509-1.333s-.192-.979-.509-1.333A1.993 1.993 0 0 0 18 9v4z", Html.Attributes.attribute "fill" fill ] [] ]


menu : String -> Svg.Svg msg
menu fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M4 7a1 1 0 0 1 1-1h14a1 1 0 1 1 0 2H5a1 1 0 0 1-1-1zm0 5a1 1 0 0 1 1-1h14a1 1 0 1 1 0 2H5a1 1 0 0 1-1-1zm0 5a1 1 0 0 1 1-1h14a1 1 0 1 1 0 2H5a1 1 0 0 1-1-1z", Html.Attributes.attribute "fill" fill ] [] ]


messageAlt : String -> Svg.Svg msg
messageAlt fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M2 6a2 2 0 0 1 2-2h16a2 2 0 0 1 2 2v11a2 2 0 0 1-2 2h-4.586l-2.707 2.707a1 1 0 0 1-1.414 0L8.586 19H4a2 2 0 0 1-2-2V6zm18 0H4v11h5a1 1 0 0 1 .707.293L12 19.586l2.293-2.293A1 1 0 0 1 15 17h5V6z", Html.Attributes.attribute "fill" fill ] [], Svg.node "path" [ Html.Attributes.attribute "d" "M13.5 11.5a1.5 1.5 0 1 1-3 0 1.5 1.5 0 0 1 3 0zm4 0a1.5 1.5 0 1 1-3 0 1.5 1.5 0 0 1 3 0zm-8 0a1.5 1.5 0 1 1-3 0 1.5 1.5 0 0 1 3 0z", Html.Attributes.attribute "fill" fill ] [] ]


message : String -> Svg.Svg msg
message fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M2 6a2 2 0 0 1 2-2h16a2 2 0 0 1 2 2v11a2 2 0 0 1-2 2h-4.586l-2.707 2.707a1 1 0 0 1-1.414 0L8.586 19H4a2 2 0 0 1-2-2V6zm18 0H4v11h5a1 1 0 0 1 .707.293L12 19.586l2.293-2.293A1 1 0 0 1 15 17h5V6zM6 9.5a1 1 0 0 1 1-1h10a1 1 0 1 1 0 2H7a1 1 0 0 1-1-1zm0 4a1 1 0 0 1 1-1h6a1 1 0 1 1 0 2H7a1 1 0 0 1-1-1z", Html.Attributes.attribute "fill" fill ] [] ]


mobile : String -> Svg.Svg msg
mobile fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M6 5a2 2 0 0 1 2-2h8a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H8a2 2 0 0 1-2-2V5zm10 0H8v14h8V5z", Html.Attributes.attribute "fill" fill ] [], Svg.node "path" [ Html.Attributes.attribute "d" "M13 17a1 1 0 1 1-2 0 1 1 0 0 1 2 0z", Html.Attributes.attribute "fill" fill ] [] ]


moon : String -> Svg.Svg msg
moon fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M9.36 3.293a1 1 0 0 1 .187 1.157A7.45 7.45 0 0 0 19.55 14.453a1 1 0 0 1 1.343 1.343 9.45 9.45 0 1 1-12.69-12.69 1 1 0 0 1 1.158.187zM6.823 6.67A7.45 7.45 0 0 0 17.33 17.179 9.45 9.45 0 0 1 6.821 6.67z", Html.Attributes.attribute "fill" fill ] [] ]


next : String -> Svg.Svg msg
next fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "fill-rule" "evenodd", Html.Attributes.attribute "clip-rule" "evenodd", Html.Attributes.attribute "d" "M5 7.766c0-1.554 1.696-2.515 3.029-1.715l7.056 4.234c1.295.777 1.295 2.653 0 3.43L8.03 17.949c-1.333.8-3.029-.16-3.029-1.715V7.766zM14.056 12L7 7.766v8.468L14.056 12zM18 6a1 1 0 0 1 1 1v10a1 1 0 1 1-2 0V7a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


notificationOff : String -> Svg.Svg msg
notificationOff fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M19 10c0-3.224-2.18-5.94-5.146-6.752a2 2 0 0 0-3.708 0 6.957 6.957 0 0 0-1.587.655l1.492 1.491A5 5 0 0 1 17 10v2.343l2 2V10zM3.175 17.434L5 14.697V10c0-1.05.231-2.046.646-2.94L3.293 4.707a1 1 0 0 1 1.414-1.414l16 16a1 1 0 0 1-1.414 1.414L17.586 19h-2.121a3.501 3.501 0 0 1-6.93 0H4.013a.994.994 0 0 1-.633-.215.999.999 0 0 1-.205-1.35zM5.87 17h9.717l-8.39-8.39A5.003 5.003 0 0 0 7 10v5a1 1 0 0 1-.168.555L5.869 17zm4.716 2a1.5 1.5 0 0 0 2.83 0h-2.83z", Html.Attributes.attribute "fill" fill ] [] ]


notification : String -> Svg.Svg msg
notification fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M10.146 3.248a2 2 0 0 1 3.708 0A7.003 7.003 0 0 1 19 10v4.697l1.832 2.748A1 1 0 0 1 20 19h-4.535a3.501 3.501 0 0 1-6.93 0H4a1 1 0 0 1-.832-1.555L5 14.697V10c0-3.224 2.18-5.94 5.146-6.752zM10.586 19a1.5 1.5 0 0 0 2.829 0h-2.83zM12 5a5 5 0 0 0-5 5v5a1 1 0 0 1-.168.555L5.869 17H18.13l-.963-1.445A1 1 0 0 1 17 15v-5a5 5 0 0 0-5-5z", Html.Attributes.attribute "fill" fill ] [] ]


optionsHorizontal : String -> Svg.Svg msg
optionsHorizontal fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 14a2 2 0 1 0 0-4 2 2 0 0 0 0 4zm-6 0a2 2 0 1 0 0-4 2 2 0 0 0 0 4zm12 0a2 2 0 1 0 0-4 2 2 0 0 0 0 4z", Html.Attributes.attribute "fill" fill ] [] ]


optionsVertical : String -> Svg.Svg msg
optionsVertical fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M10 12a2 2 0 1 0 4 0 2 2 0 0 0-4 0zm0-6a2 2 0 1 0 4 0 2 2 0 0 0-4 0zm0 12a2 2 0 1 0 4 0 2 2 0 0 0-4 0z", Html.Attributes.attribute "fill" fill ] [] ]


pause : String -> Svg.Svg msg
pause fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M9 6a1 1 0 0 1 1 1v10a1 1 0 1 1-2 0V7a1 1 0 0 1 1-1zm6 0a1 1 0 0 1 1 1v10a1 1 0 1 1-2 0V7a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


percentage : String -> Svg.Svg msg
percentage fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M18.707 5.293a1 1 0 0 1 0 1.414l-12 12a1 1 0 0 1-1.414-1.414l12-12a1 1 0 0 1 1.414 0zM17 19a2 2 0 1 0 0-4 2 2 0 0 0 0 4zM7 9a2 2 0 1 0 0-4 2 2 0 0 0 0 4z", Html.Attributes.attribute "fill" fill ] [] ]


pin : String -> Svg.Svg msg
pin fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M6 10.6V4a1 1 0 0 1 0-2h12a1 1 0 1 1 0 2v6.6c.932 1.02 1.432 2.034 1.699 2.834.146.438.22.81.26 1.08a4.43 4.43 0 0 1 .04.43v.034l.001.013v.008s-.005-.131 0 .001a1 1 0 0 1-1 1h-6v5a1 1 0 1 1-2 0v-5H5a1 1 0 0 1-1-1v-.022a2.013 2.013 0 0 1 .006-.134 5.07 5.07 0 0 1 .035-.33c.04-.27.114-.642.26-1.08.267-.8.767-1.814 1.699-2.835zM16 4H8v7a1 1 0 0 1-.293.707c-.847.847-1.271 1.678-1.486 2.293H17.78c-.215-.615-.64-1.446-1.486-2.293A1 1 0 0 1 16 11V4z", Html.Attributes.attribute "fill" fill ] [] ]


play : String -> Svg.Svg msg
play fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M6 6.741c0-1.544 1.674-2.505 3.008-1.728l9.015 5.26c1.323.771 1.323 2.683 0 3.455l-9.015 5.258C7.674 19.764 6 18.803 6 17.26V6.741zM17.015 12L8 6.741V17.26L17.015 12z", Html.Attributes.attribute "fill" fill ] [] ]


previous : String -> Svg.Svg msg
previous fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "fill-rule" "evenodd", Html.Attributes.attribute "clip-rule" "evenodd", Html.Attributes.attribute "d" "M19 7.766c0-1.554-1.696-2.515-3.029-1.715l-7.056 4.234c-1.295.777-1.295 2.653 0 3.43l7.056 4.234c1.333.8 3.029-.16 3.029-1.715V7.766zM9.944 12L17 7.766v8.468L9.944 12zM6 6a1 1 0 0 1 1 1v10a1 1 0 1 1-2 0V7a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


refresh : String -> Svg.Svg msg
refresh fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12.793 2.293a1 1 0 0 1 1.414 0l3 3a1 1 0 0 1 0 1.414l-3 3a1 1 0 0 1-1.414-1.414L14.086 7H12.5C8.952 7 6 9.952 6 13.5S8.952 20 12.5 20s6.5-2.952 6.5-6.5a1 1 0 1 1 2 0c0 4.652-3.848 8.5-8.5 8.5S4 18.152 4 13.5 7.848 5 12.5 5h1.586l-1.293-1.293a1 1 0 0 1 0-1.414z", Html.Attributes.attribute "fill" fill ] [] ]


remove : String -> Svg.Svg msg
remove fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M4 12a1 1 0 0 1 1-1h14a1 1 0 1 1 0 2H5a1 1 0 0 1-1-1z", Html.Attributes.attribute "fill" fill ] [] ]


repeat : String -> Svg.Svg msg
repeat fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M20.924 5.617a.997.997 0 0 0-.217-.324l-3-3a1 1 0 1 0-1.414 1.414L17.586 5H8a5 5 0 0 0-5 5v2a1 1 0 1 0 2 0v-2a3 3 0 0 1 3-3h9.586l-1.293 1.293a1 1 0 0 0 1.414 1.414l3-3A.997.997 0 0 0 21 6m-.076-.383a.996.996 0 0 1 .076.38l-.076-.38zm-17.848 12a.997.997 0 0 0 .217 1.09l3 3a1 1 0 0 0 1.414-1.414L6.414 19H16a5 5 0 0 0 5-5v-2a1 1 0 1 0-2 0v2a3 3 0 0 1-3 3H6.414l1.293-1.293a1 1 0 1 0-1.414-1.414l-3 3m-.217.324a.997.997 0 0 1 .215-.322l-.215.322z", Html.Attributes.attribute "fill" fill ] [] ]


search : String -> Svg.Svg msg
search fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M10 4a6 6 0 1 0 0 12 6 6 0 0 0 0-12zm-8 6a8 8 0 1 1 14.32 4.906l5.387 5.387a1 1 0 0 1-1.414 1.414l-5.387-5.387A8 8 0 0 1 2 10z", Html.Attributes.attribute "fill" fill ] [] ]


select : String -> Svg.Svg msg
select fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4a1 1 0 0 1 .707.293l4 4a1 1 0 0 1-1.414 1.414L12 6.414 8.707 9.707a1 1 0 0 1-1.414-1.414l4-4A1 1 0 0 1 12 4zM7.293 14.293a1 1 0 0 1 1.414 0L12 17.586l3.293-3.293a1 1 0 0 1 1.414 1.414l-4 4a1 1 0 0 1-1.414 0l-4-4a1 1 0 0 1 0-1.414z", Html.Attributes.attribute "fill" fill ] [] ]


send : String -> Svg.Svg msg
send fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 2a1 1 0 0 1 .894.553l9 18a1 1 0 0 1-1.11 1.423L12 20.024l-8.783 1.952a1 1 0 0 1-1.111-1.423l9-18A1 1 0 0 1 12 2zm1 16.198l6.166 1.37L13 7.236v10.962zM11 7.236L4.834 19.568 11 18.198V7.236z", Html.Attributes.attribute "fill" fill ] [] ]


settings : String -> Svg.Svg msg
settings fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4a1 1 0 0 0-1 1c0 1.692-2.046 2.54-3.243 1.343a1 1 0 1 0-1.414 1.414C7.54 8.954 6.693 11 5 11a1 1 0 1 0 0 2c1.692 0 2.54 2.046 1.343 3.243a1 1 0 0 0 1.414 1.414C8.954 16.46 11 17.307 11 19a1 1 0 1 0 2 0c0-1.692 2.046-2.54 3.243-1.343a1 1 0 1 0 1.414-1.414C16.46 15.046 17.307 13 19 13a1 1 0 1 0 0-2c-1.692 0-2.54-2.046-1.343-3.243a1 1 0 0 0-1.414-1.414C15.046 7.54 13 6.693 13 5a1 1 0 0 0-1-1zm-2.992.777a3 3 0 0 1 5.984 0 3 3 0 0 1 4.23 4.231 3 3 0 0 1 .001 5.984 3 3 0 0 1-4.231 4.23 3 3 0 0 1-5.984 0 3 3 0 0 1-4.231-4.23 3 3 0 0 1 0-5.984 3 3 0 0 1 4.231-4.231z", Html.Attributes.attribute "fill" fill ] [], Svg.node "path" [ Html.Attributes.attribute "d" "M12 10a2 2 0 1 0 0 4 2 2 0 0 0 0-4zm-2.828-.828a4 4 0 1 1 5.656 5.656 4 4 0 0 1-5.656-5.656z", Html.Attributes.attribute "fill" fill ] [] ]


share : String -> Svg.Svg msg
share fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M11.293 2.293a1 1 0 0 1 1.414 0l3 3a1 1 0 0 1-1.414 1.414L13 5.414V15a1 1 0 1 1-2 0V5.414L9.707 6.707a1 1 0 0 1-1.414-1.414l3-3zM4 11a2 2 0 0 1 2-2h2a1 1 0 0 1 0 2H6v9h12v-9h-2a1 1 0 1 1 0-2h2a2 2 0 0 1 2 2v9a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2v-9z", Html.Attributes.attribute "fill" fill ] [] ]


shoppingCartAdd : String -> Svg.Svg msg
shoppingCartAdd fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M6.01 16.136L4.141 4H3a1 1 0 0 1 0-2h1.985a.993.993 0 0 1 .66.235.997.997 0 0 1 .346.627L6.319 5H14v2H6.627l1.23 8h9.399l1.5-5h2.088l-1.886 6.287A1 1 0 0 1 18 17H7.016a.993.993 0 0 1-.675-.248.999.999 0 0 1-.332-.616zM10 20a2 2 0 1 1-4 0 2 2 0 0 1 4 0zm9 0a2 2 0 1 1-4 0 2 2 0 0 1 4 0zm0-18a1 1 0 0 1 1 1v1h1a1 1 0 1 1 0 2h-1v1a1 1 0 1 1-2 0V6h-1a1 1 0 1 1 0-2h1V3a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


shoppingCart : String -> Svg.Svg msg
shoppingCart fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M4.153 4L6.01 15.146a.993.993 0 0 0 .327.603.997.997 0 0 0 .679.251H18a1 1 0 0 0 .949-.684l3-9A1 1 0 0 0 21 5H6.347L5.99 2.85a.993.993 0 0 0-.357-.625A.998.998 0 0 0 4.984 2H3a1 1 0 0 0 0 2h1.153zm3.694 10L6.68 7h12.933l-2.334 7H7.847zM10 20a2 2 0 1 1-4 0 2 2 0 0 1 4 0zm9 0a2 2 0 1 1-4 0 2 2 0 0 1 4 0z", Html.Attributes.attribute "fill" fill ] [] ]


shuffle : String -> Svg.Svg msg
shuffle fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M21.924 6.617a.997.997 0 0 0-.217-.324l-3-3a1 1 0 1 0-1.414 1.414L18.586 6h-3.321a5 5 0 0 0-4.288 2.428l-3.67 6.115A3 3 0 0 1 4.736 16H3a1 1 0 1 0 0 2h1.735a5 5 0 0 0 4.288-2.428l3.67-6.115A3 3 0 0 1 15.264 8h3.32l-1.292 1.293a1 1 0 0 0 1.414 1.414l3-3A.997.997 0 0 0 22 7m-.076-.383a.996.996 0 0 1 .076.38l-.076-.38z", Html.Attributes.attribute "fill" fill ] [], Svg.node "path" [ Html.Attributes.attribute "d" "M21.706 17.708l-2.999 3a1 1 0 0 1-1.414-1.415L18.586 18h-3.321a5 5 0 0 1-4.288-2.428l-3.67-6.115A3 3 0 0 0 4.736 8H3a1 1 0 0 1 0-2h1.735a5 5 0 0 1 4.288 2.428l3.67 6.115A3 3 0 0 0 15.264 16h3.32l-1.292-1.293a1 1 0 0 1 1.414-1.414l3 3c.195.194.292.45.293.704V17a.997.997 0 0 1-.294.708z", Html.Attributes.attribute "fill" fill ] [] ]


sort : String -> Svg.Svg msg
sort fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M6.293 4.293a1 1 0 0 1 1.414 0l4 4a1 1 0 0 1-1.414 1.414L8 7.414V19a1 1 0 1 1-2 0V7.414L3.707 9.707a1 1 0 0 1-1.414-1.414l4-4zM16 16.586V5a1 1 0 1 1 2 0v11.586l2.293-2.293a1 1 0 0 1 1.414 1.414l-4 4a1 1 0 0 1-1.414 0l-4-4a1 1 0 0 1 1.414-1.414L16 16.586z", Html.Attributes.attribute "fill" fill ] [] ]


speakers : String -> Svg.Svg msg
speakers fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M4 4a2 2 0 0 1 2-2h12a2 2 0 0 1 2 2v16a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V4zm14 0H6v16h12V4z", Html.Attributes.attribute "fill" fill ] [], Svg.node "path" [ Html.Attributes.attribute "d" "M12 12a2 2 0 1 0 0 4 2 2 0 0 0 0-4zm-4 2a4 4 0 1 1 8 0 4 4 0 0 1-8 0zm5.5-6.5a1.5 1.5 0 1 1-3 0 1.5 1.5 0 0 1 3 0z", Html.Attributes.attribute "fill" fill ] [] ]


stop : String -> Svg.Svg msg
stop fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M5 7a2 2 0 0 1 2-2h10a2 2 0 0 1 2 2v10a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V7zm12 0H7v10h10V7z", Html.Attributes.attribute "fill" fill ] [] ]


sun : String -> Svg.Svg msg
sun fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 2a1 1 0 0 1 1 1v1a1 1 0 1 1-2 0V3a1 1 0 0 1 1-1zm7.071 2.929a1 1 0 0 1 0 1.414l-.707.707a1 1 0 1 1-1.414-1.414l.707-.707a1 1 0 0 1 1.414 0zm-14.142 0a1 1 0 0 1 1.414 0l.707.707A1 1 0 0 1 5.636 7.05l-.707-.707a1 1 0 0 1 0-1.414zM12 8a4 4 0 1 0 0 8 4 4 0 0 0 0-8zm-6 4a6 6 0 1 1 12 0 6 6 0 0 1-12 0zm-4 0a1 1 0 0 1 1-1h1a1 1 0 1 1 0 2H3a1 1 0 0 1-1-1zm17 0a1 1 0 0 1 1-1h1a1 1 0 1 1 0 2h-1a1 1 0 0 1-1-1zM5.636 16.95a1 1 0 0 1 1.414 1.414l-.707.707a1 1 0 0 1-1.414-1.414l.707-.707zm11.314 1.414a1 1 0 0 1 1.414-1.414l.707.707a1 1 0 0 1-1.414 1.414l-.707-.707zM12 19a1 1 0 0 1 1 1v1a1 1 0 1 1-2 0v-1a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


sunrise : String -> Svg.Svg msg
sunrise fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M13 5a1 1 0 1 0-2 0v1a1 1 0 1 0 2 0V5z", Html.Attributes.attribute "fill" fill ] [], Svg.node "path" [ Html.Attributes.attribute "fill-rule" "evenodd", Html.Attributes.attribute "clip-rule" "evenodd", Html.Attributes.attribute "d" "M9.028 19H3a1 1 0 1 1 0-2h3.803a6 6 0 1 1 10.394 0H21a1 1 0 1 1 0 2H9.028zM12 10a4 4 0 0 0-2.646 7h5.292A4 4 0 0 0 12 10z", Html.Attributes.attribute "fill" fill ] [], Svg.node "path" [ Html.Attributes.attribute "d" "M19.071 8.343l-.707.707a1 1 0 1 1-1.414-1.414l.707-.707a1 1 0 1 1 1.414 1.414zM4 15a1 1 0 1 0 0-2H3a1 1 0 1 0 0 2h1zm18-1a1 1 0 0 1-1 1h-1a1 1 0 1 1 0-2h1a1 1 0 0 1 1 1zM5.636 9.05A1 1 0 0 0 7.05 7.636l-.707-.707A1 1 0 1 0 4.93 8.343l.707.707z", Html.Attributes.attribute "fill" fill ] [] ]


switch : String -> Svg.Svg msg
switch fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M14.293 2.293a1 1 0 0 1 1.414 0l4 4a1 1 0 0 1 0 1.414l-4 4a1 1 0 0 1-1.414-1.414L16.586 8H5a1 1 0 0 1 0-2h11.586l-2.293-2.293a1 1 0 0 1 0-1.414zm-4.586 10a1 1 0 0 1 0 1.414L7.414 16H19a1 1 0 1 1 0 2H7.414l2.293 2.293a1 1 0 0 1-1.414 1.414l-4-4a1 1 0 0 1 0-1.414l4-4a1 1 0 0 1 1.414 0z", Html.Attributes.attribute "fill" fill ] [] ]


table : String -> Svg.Svg msg
table fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M2 5.5a2 2 0 0 1 2-2h16a2 2 0 0 1 2 2v13a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2v-13zm9 0H4v3h7v-3zm2 0v3h7v-3h-7zm7 5h-7v3h7v-3zm0 5h-7v3h7v-3zm-9 3v-3H4v3h7zm-7-5h7v-3H4v3z", Html.Attributes.attribute "fill" fill ] [] ]


tablet : String -> Svg.Svg msg
tablet fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M4 4a2 2 0 0 1 2-2h12a2 2 0 0 1 2 2v16a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V4zm14 0H6v16h12V4z", Html.Attributes.attribute "fill" fill ] [], Svg.node "path" [ Html.Attributes.attribute "d" "M13 18a1 1 0 1 1-2 0 1 1 0 0 1 2 0z", Html.Attributes.attribute "fill" fill ] [] ]


tag : String -> Svg.Svg msg
tag fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M2 3a1 1 0 0 1 1-1h8a1 1 0 0 1 .707.293l10 10a1 1 0 0 1 0 1.414l-8 8a1 1 0 0 1-1.414 0l-10-10A1 1 0 0 1 2 11V3zm2 1v6.586l9 9L19.586 13l-9-9H4z", Html.Attributes.attribute "fill" fill ] [], Svg.node "path" [ Html.Attributes.attribute "d" "M9 7.5a1.5 1.5 0 1 1-3 0 1.5 1.5 0 0 1 3 0z", Html.Attributes.attribute "fill" fill ] [] ]


undo : String -> Svg.Svg msg
undo fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12.207 2.293a1 1 0 0 1 0 1.414L10.914 5H12.5c4.652 0 8.5 3.848 8.5 8.5S17.152 22 12.5 22 4 18.152 4 13.5a1 1 0 1 1 2 0c0 3.548 2.952 6.5 6.5 6.5s6.5-2.952 6.5-6.5S16.048 7 12.5 7h-1.586l1.293 1.293a1 1 0 0 1-1.414 1.414l-3-3a1 1 0 0 1 0-1.414l3-3a1 1 0 0 1 1.414 0z", Html.Attributes.attribute "fill" fill ] [] ]


unlock : String -> Svg.Svg msg
unlock fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4c-1.648 0-3 1.352-3 3v3h9a2 2 0 0 1 2 2v8a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2v-8a2 2 0 0 1 2-2h1V7c0-2.752 2.248-5 5-5s5 2.248 5 5a1 1 0 1 1-2 0c0-1.648-1.352-3-3-3zm-6 8v8h12v-8H6z", Html.Attributes.attribute "fill" fill ] [] ]


userAdd : String -> Svg.Svg msg
userAdd fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M10 4a4 4 0 1 0 0 8 4 4 0 0 0 0-8zM4 8a6 6 0 1 1 12 0A6 6 0 0 1 4 8zm15 3a1 1 0 0 1 1 1v1h1a1 1 0 1 1 0 2h-1v1a1 1 0 1 1-2 0v-1h-1a1 1 0 1 1 0-2h1v-1a1 1 0 0 1 1-1zM6.5 18C5.24 18 4 19.213 4 21a1 1 0 1 1-2 0c0-2.632 1.893-5 4.5-5h7c2.607 0 4.5 2.368 4.5 5a1 1 0 1 1-2 0c0-1.787-1.24-3-2.5-3h-7z", Html.Attributes.attribute "fill" fill ] [] ]


userCheck : String -> Svg.Svg msg
userCheck fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M10 4a4 4 0 1 0 0 8 4 4 0 0 0 0-8zM4 8a6 6 0 1 1 12 0A6 6 0 0 1 4 8zm17.664 3.253a1 1 0 0 1 .083 1.411l-2.666 3a1 1 0 0 1-1.495 0l-1.333-1.5a1 1 0 0 1 1.494-1.328l.586.659 1.92-2.16a1 1 0 0 1 1.411-.082zM6.5 18C5.24 18 4 19.213 4 21a1 1 0 1 1-2 0c0-2.632 1.893-5 4.5-5h7c2.607 0 4.5 2.368 4.5 5a1 1 0 1 1-2 0c0-1.787-1.24-3-2.5-3h-7z", Html.Attributes.attribute "fill" fill ] [] ]


userRemove : String -> Svg.Svg msg
userRemove fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M10 4a4 4 0 1 0 0 8 4 4 0 0 0 0-8zM4 8a6 6 0 1 1 12 0A6 6 0 0 1 4 8zm12 6a1 1 0 0 1 1-1h4a1 1 0 1 1 0 2h-4a1 1 0 0 1-1-1zm-9.5 4C5.24 18 4 19.213 4 21a1 1 0 1 1-2 0c0-2.632 1.893-5 4.5-5h7c2.607 0 4.5 2.368 4.5 5a1 1 0 1 1-2 0c0-1.787-1.24-3-2.5-3h-7z", Html.Attributes.attribute "fill" fill ] [] ]


user : String -> Svg.Svg msg
user fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 4a4 4 0 1 0 0 8 4 4 0 0 0 0-8zM6 8a6 6 0 1 1 12 0A6 6 0 0 1 6 8zm2 10a3 3 0 0 0-3 3 1 1 0 1 1-2 0 5 5 0 0 1 5-5h8a5 5 0 0 1 5 5 1 1 0 1 1-2 0 3 3 0 0 0-3-3H8z", Html.Attributes.attribute "fill" fill ] [] ]


users : String -> Svg.Svg msg
users fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M10 4a4 4 0 1 0 0 8 4 4 0 0 0 0-8zM4 8a6 6 0 1 1 12 0A6 6 0 0 1 4 8zm12.828-4.243a1 1 0 0 1 1.415 0 6 6 0 0 1 0 8.486 1 1 0 1 1-1.415-1.415 4 4 0 0 0 0-5.656 1 1 0 0 1 0-1.415zm.702 13a1 1 0 0 1 1.212-.727c1.328.332 2.169 1.18 2.652 2.148.468.935.606 1.98.606 2.822a1 1 0 1 1-2 0c0-.657-.112-1.363-.394-1.928-.267-.533-.677-.934-1.349-1.102a1 1 0 0 1-.727-1.212zM6.5 18C5.24 18 4 19.213 4 21a1 1 0 1 1-2 0c0-2.632 1.893-5 4.5-5h7c2.607 0 4.5 2.368 4.5 5a1 1 0 1 1-2 0c0-1.787-1.24-3-2.5-3h-7z", Html.Attributes.attribute "fill" fill ] [] ]


volumeOff : String -> Svg.Svg msg
volumeOff fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M13.418 2.091A1 1 0 0 1 14 3v18a1 1 0 0 1-1.65.76L5.63 16H3a1 1 0 0 1-1-1V9a1 1 0 0 1 1-1h2.63l6.72-5.76a1 1 0 0 1 1.068-.149zM12 5.175L6.65 9.76A1 1 0 0 1 6 10H4v4h2a1 1 0 0 1 .65.24L12 18.827V5.174zm4.293 4.119a1 1 0 0 1 1.414 0L19 10.586l1.293-1.293a1 1 0 1 1 1.414 1.414L20.414 12l1.293 1.293a1 1 0 0 1-1.414 1.414L19 13.414l-1.293 1.293a1 1 0 0 1-1.414-1.414L17.586 12l-1.293-1.293a1 1 0 0 1 0-1.414z", Html.Attributes.attribute "fill" fill ] [] ]


volumeUp : String -> Svg.Svg msg
volumeUp fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M13.418 2.091A1 1 0 0 1 14 3v18a1 1 0 0 1-1.65.76L5.63 16H3a1 1 0 0 1-1-1V9a1 1 0 0 1 1-1h2.63l6.72-5.76a1 1 0 0 1 1.068-.149zM12 5.175L6.65 9.76A1 1 0 0 1 6 10H4v4h2a1 1 0 0 1 .65.24L12 18.827V5.174zm5.293.119a1 1 0 0 1 1.414 0l.002.001.001.002.004.004.01.01a2.499 2.499 0 0 1 .114.125c.07.08.165.194.274.34.22.292.503.718.782 1.278C20.456 8.175 21 9.827 21 12s-.544 3.825-1.106 4.947c-.28.56-.562.986-.781 1.278a5.847 5.847 0 0 1-.389.465l-.01.01-.004.004-.001.002h-.001v.001a1 1 0 0 1-1.42-1.41l.005-.004.04-.045c.04-.045.102-.12.18-.223a6.39 6.39 0 0 0 .593-.972c.438-.878.894-2.226.894-4.053s-.456-3.175-.894-4.053a6.393 6.393 0 0 0-.594-.972 3.888 3.888 0 0 0-.22-.268l-.004-.005a1 1 0 0 1 .005-1.41zm-2 3a1 1 0 0 1 1.414 0l.002.001.001.002.003.003.008.008.02.02.055.061a4.697 4.697 0 0 1 .599.914c.31.623.605 1.525.605 2.698s-.294 2.075-.606 2.697c-.154.31-.312.548-.438.716a3.383 3.383 0 0 1-.215.26l-.02.02-.008.008-.003.003-.002.002a1 1 0 0 1-1.418-1.411 2.691 2.691 0 0 0 .315-.492c.19-.378.395-.976.395-1.803s-.206-1.425-.394-1.803a2.693 2.693 0 0 0-.316-.492 1 1 0 0 1 .002-1.412z", Html.Attributes.attribute "fill" fill ] [] ]


warning : String -> Svg.Svg msg
warning fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M12 14a1 1 0 0 1-1-1v-3a1 1 0 1 1 2 0v3a1 1 0 0 1-1 1zm-1.5 2.5a1.5 1.5 0 1 1 3 0 1.5 1.5 0 0 1-3 0z", Html.Attributes.attribute "fill" fill ] [], Svg.node "path" [ Html.Attributes.attribute "d" "M10.23 3.216c.75-1.425 2.79-1.425 3.54 0l8.343 15.852C22.814 20.4 21.85 22 20.343 22H3.657c-1.505 0-2.47-1.6-1.77-2.931L10.23 3.216zM20.344 20L12 4.147 3.656 20h16.688z", Html.Attributes.attribute "fill" fill ] [] ]


webcam : String -> Svg.Svg msg
webcam fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M21.5 6.1c-.3-.2-.7-.2-1 0l-4.4 3V7c0-1.1-.9-2-2-2H4c-1.1 0-2 .9-2 2v10c0 1.1.9 2 2 2h10c1.1 0 2-.9 2-2v-2.1l4.4 3c.2.1.4.2.6.2.2 0 .3 0 .5-.1.3-.2.5-.5.5-.9V7c0-.4-.2-.7-.5-.9zM14 17H4V7h10v10zm6-1.9l-4-2.7v-.9l4-2.7v6.3z", Html.Attributes.attribute "fill" fill ] [] ]


zoomIn : String -> Svg.Svg msg
zoomIn fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M10 4a6 6 0 1 0 0 12 6 6 0 0 0 0-12zm-8 6a8 8 0 1 1 14.32 4.906l5.387 5.387a1 1 0 0 1-1.414 1.414l-5.387-5.387A8 8 0 0 1 2 10zm8-3a1 1 0 0 1 1 1v1h1a1 1 0 1 1 0 2h-1v1a1 1 0 1 1-2 0v-1H8a1 1 0 1 1 0-2h1V8a1 1 0 0 1 1-1z", Html.Attributes.attribute "fill" fill ] [] ]


zoomOut : String -> Svg.Svg msg
zoomOut fill =
    Svg.node "svg" [ Svg.Attributes.fill fill, Html.Attributes.attribute "width" "24", Html.Attributes.attribute "height" "24", Html.Attributes.attribute "viewBox" "0 0 24 24", Html.Attributes.attribute "fill" "none", Html.Attributes.attribute "xmlns" "http://www.w3.org/2000/svg" ] [ Svg.node "path" [ Html.Attributes.attribute "d" "M10 4a6 6 0 1 0 0 12 6 6 0 0 0 0-12zm-8 6a8 8 0 1 1 14.32 4.906l5.387 5.387a1 1 0 0 1-1.414 1.414l-5.387-5.387A8 8 0 0 1 2 10zm5 0a1 1 0 0 1 1-1h4a1 1 0 1 1 0 2H8a1 1 0 0 1-1-1z", Html.Attributes.attribute "fill" fill ] [] ]
