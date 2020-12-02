module Emoji exposing (default, filter, find, list, view)

import Html exposing (Html, div)
import Html.Attributes exposing (class, style)


type alias EmojiData =
    { name : String
    , x : Int
    , y : Int
    }


default : EmojiData
default =
    EmojiData "smiley" 30 38


recommended : List EmojiData
recommended =
    [ EmojiData "smiley" 30 38
    , EmojiData "sweat smile" 30 40
    , EmojiData "relieved" 30 47
    , EmojiData "disappointed" 31 8
    , EmojiData "worried" 31 9
    , EmojiData "tired face" 31 21
    , EmojiData "grimacing" 31 22
    , EmojiData "dizzy face" 31 31
    , EmojiData "mask" 31 33
    , EmojiData "face with head bandage" 37 28
    ]


filter : String -> List EmojiData
filter name =
    if String.isEmpty name then
        recommended

    else
        List.filter (\emojiData -> String.contains name emojiData.name) list
            |> List.sortBy (\e -> String.length e.name)


find : String -> EmojiData
find name =
    filter name |> List.head |> Maybe.withDefault default


view : EmojiData -> Html msg
view { name, x, y } =
    div
        [ class "emoji"
        , style "background-position"
            (String.concat
                [ String.fromInt <| (x * -22)
                , "px "
                , String.fromInt <| (y * -22)
                , "px"
                ]
            )
        ]
        []


list : List EmojiData
list =
    [ EmojiData "hash" 0 0
    , EmojiData "keycap star" 0 1
    , EmojiData "zero" 0 2
    , EmojiData "one" 0 3
    , EmojiData "two" 0 4
    , EmojiData "three" 0 5
    , EmojiData "four" 0 6
    , EmojiData "five" 0 7
    , EmojiData "six" 0 8
    , EmojiData "seven" 0 9
    , EmojiData "eight" 0 10
    , EmojiData "nine" 0 11
    , EmojiData "copyright" 0 12
    , EmojiData "registered" 0 13
    , EmojiData "mahjong" 0 14
    , EmojiData "black joker" 0 15
    , EmojiData "a" 0 16
    , EmojiData "b" 0 17
    , EmojiData "o2" 0 18
    , EmojiData "parking" 0 19
    , EmojiData "ab" 0 20
    , EmojiData "cl" 0 21
    , EmojiData "cool" 0 22
    , EmojiData "free" 0 23
    , EmojiData "id" 0 24
    , EmojiData "new" 0 25
    , EmojiData "ng" 0 26
    , EmojiData "ok" 0 27
    , EmojiData "sos" 0 28
    , EmojiData "up" 0 29
    , EmojiData "vs" 0 30
    , EmojiData "flag ac" 0 31
    , EmojiData "flag ad" 0 32
    , EmojiData "flag ae" 0 33
    , EmojiData "flag af" 0 34
    , EmojiData "flag ag" 0 35
    , EmojiData "flag ai" 0 36
    , EmojiData "flag al" 0 37
    , EmojiData "flag am" 0 38
    , EmojiData "flag ao" 0 39
    , EmojiData "flag aq" 0 40
    , EmojiData "flag ar" 0 41
    , EmojiData "flag as" 0 42
    , EmojiData "flag at" 0 43
    , EmojiData "flag au" 0 44
    , EmojiData "flag aw" 0 45
    , EmojiData "flag ax" 0 46
    , EmojiData "flag az" 0 47
    , EmojiData "flag ba" 0 48
    , EmojiData "flag bb" 0 49
    , EmojiData "flag bd" 0 50
    , EmojiData "flag be" 0 51
    , EmojiData "flag bf" 0 52
    , EmojiData "flag bg" 0 53
    , EmojiData "flag bh" 0 54
    , EmojiData "flag bi" 0 55
    , EmojiData "flag bj" 0 56
    , EmojiData "flag bl" 1 0
    , EmojiData "flag bm" 1 1
    , EmojiData "flag bn" 1 2
    , EmojiData "flag bo" 1 3
    , EmojiData "flag bq" 1 4
    , EmojiData "flag br" 1 5
    , EmojiData "flag bs" 1 6
    , EmojiData "flag bt" 1 7
    , EmojiData "flag bv" 1 8
    , EmojiData "flag bw" 1 9
    , EmojiData "flag by" 1 10
    , EmojiData "flag bz" 1 11
    , EmojiData "flag ca" 1 12
    , EmojiData "flag cc" 1 13
    , EmojiData "flag cd" 1 14
    , EmojiData "flag cf" 1 15
    , EmojiData "flag cg" 1 16
    , EmojiData "flag ch" 1 17
    , EmojiData "flag ci" 1 18
    , EmojiData "flag ck" 1 19
    , EmojiData "flag cl" 1 20
    , EmojiData "flag cm" 1 21
    , EmojiData "cn" 1 22
    , EmojiData "flag co" 1 23
    , EmojiData "flag cp" 1 24
    , EmojiData "flag cr" 1 25
    , EmojiData "flag cu" 1 26
    , EmojiData "flag cv" 1 27
    , EmojiData "flag cw" 1 28
    , EmojiData "flag cx" 1 29
    , EmojiData "flag cy" 1 30
    , EmojiData "flag cz" 1 31
    , EmojiData "de" 1 32
    , EmojiData "flag dg" 1 33
    , EmojiData "flag dj" 1 34
    , EmojiData "flag dk" 1 35
    , EmojiData "flag dm" 1 36
    , EmojiData "flag do" 1 37
    , EmojiData "flag dz" 1 38
    , EmojiData "flag ea" 1 39
    , EmojiData "flag ec" 1 40
    , EmojiData "flag ee" 1 41
    , EmojiData "flag eg" 1 42
    , EmojiData "flag eh" 1 43
    , EmojiData "flag er" 1 44
    , EmojiData "es" 1 45
    , EmojiData "flag et" 1 46
    , EmojiData "flag eu" 1 47
    , EmojiData "flag fi" 1 48
    , EmojiData "flag fj" 1 49
    , EmojiData "flag fk" 1 50
    , EmojiData "flag fm" 1 51
    , EmojiData "flag fo" 1 52
    , EmojiData "fr" 1 53
    , EmojiData "flag ga" 1 54
    , EmojiData "gb" 1 55
    , EmojiData "flag gd" 1 56
    , EmojiData "flag ge" 2 0
    , EmojiData "flag gf" 2 1
    , EmojiData "flag gg" 2 2
    , EmojiData "flag gh" 2 3
    , EmojiData "flag gi" 2 4
    , EmojiData "flag gl" 2 5
    , EmojiData "flag gm" 2 6
    , EmojiData "flag gn" 2 7
    , EmojiData "flag gp" 2 8
    , EmojiData "flag gq" 2 9
    , EmojiData "flag gr" 2 10
    , EmojiData "flag gs" 2 11
    , EmojiData "flag gt" 2 12
    , EmojiData "flag gu" 2 13
    , EmojiData "flag gw" 2 14
    , EmojiData "flag gy" 2 15
    , EmojiData "flag hk" 2 16
    , EmojiData "flag hm" 2 17
    , EmojiData "flag hn" 2 18
    , EmojiData "flag hr" 2 19
    , EmojiData "flag ht" 2 20
    , EmojiData "flag hu" 2 21
    , EmojiData "flag ic" 2 22
    , EmojiData "flag id" 2 23
    , EmojiData "flag ie" 2 24
    , EmojiData "flag il" 2 25
    , EmojiData "flag im" 2 26
    , EmojiData "flag in" 2 27
    , EmojiData "flag io" 2 28
    , EmojiData "flag iq" 2 29
    , EmojiData "flag ir" 2 30
    , EmojiData "flag is" 2 31
    , EmojiData "it" 2 32
    , EmojiData "flag je" 2 33
    , EmojiData "flag jm" 2 34
    , EmojiData "flag jo" 2 35
    , EmojiData "jp" 2 36
    , EmojiData "flag ke" 2 37
    , EmojiData "flag kg" 2 38
    , EmojiData "flag kh" 2 39
    , EmojiData "flag ki" 2 40
    , EmojiData "flag km" 2 41
    , EmojiData "flag kn" 2 42
    , EmojiData "flag kp" 2 43
    , EmojiData "kr" 2 44
    , EmojiData "flag kw" 2 45
    , EmojiData "flag ky" 2 46
    , EmojiData "flag kz" 2 47
    , EmojiData "flag la" 2 48
    , EmojiData "flag lb" 2 49
    , EmojiData "flag lc" 2 50
    , EmojiData "flag li" 2 51
    , EmojiData "flag lk" 2 52
    , EmojiData "flag lr" 2 53
    , EmojiData "flag ls" 2 54
    , EmojiData "flag lt" 2 55
    , EmojiData "flag lu" 2 56
    , EmojiData "flag lv" 3 0
    , EmojiData "flag ly" 3 1
    , EmojiData "flag ma" 3 2
    , EmojiData "flag mc" 3 3
    , EmojiData "flag md" 3 4
    , EmojiData "flag me" 3 5
    , EmojiData "flag mf" 3 6
    , EmojiData "flag mg" 3 7
    , EmojiData "flag mh" 3 8
    , EmojiData "flag mk" 3 9
    , EmojiData "flag ml" 3 10
    , EmojiData "flag mm" 3 11
    , EmojiData "flag mn" 3 12
    , EmojiData "flag mo" 3 13
    , EmojiData "flag mp" 3 14
    , EmojiData "flag mq" 3 15
    , EmojiData "flag mr" 3 16
    , EmojiData "flag ms" 3 17
    , EmojiData "flag mt" 3 18
    , EmojiData "flag mu" 3 19
    , EmojiData "flag mv" 3 20
    , EmojiData "flag mw" 3 21
    , EmojiData "flag mx" 3 22
    , EmojiData "flag my" 3 23
    , EmojiData "flag mz" 3 24
    , EmojiData "flag na" 3 25
    , EmojiData "flag nc" 3 26
    , EmojiData "flag ne" 3 27
    , EmojiData "flag nf" 3 28
    , EmojiData "flag ng" 3 29
    , EmojiData "flag ni" 3 30
    , EmojiData "flag nl" 3 31
    , EmojiData "flag no" 3 32
    , EmojiData "flag np" 3 33
    , EmojiData "flag nr" 3 34
    , EmojiData "flag nu" 3 35
    , EmojiData "flag nz" 3 36
    , EmojiData "flag om" 3 37
    , EmojiData "flag pa" 3 38
    , EmojiData "flag pe" 3 39
    , EmojiData "flag pf" 3 40
    , EmojiData "flag pg" 3 41
    , EmojiData "flag ph" 3 42
    , EmojiData "flag pk" 3 43
    , EmojiData "flag pl" 3 44
    , EmojiData "flag pm" 3 45
    , EmojiData "flag pn" 3 46
    , EmojiData "flag pr" 3 47
    , EmojiData "flag ps" 3 48
    , EmojiData "flag pt" 3 49
    , EmojiData "flag pw" 3 50
    , EmojiData "flag py" 3 51
    , EmojiData "flag qa" 3 52
    , EmojiData "flag re" 3 53
    , EmojiData "flag ro" 3 54
    , EmojiData "flag rs" 3 55
    , EmojiData "ru" 3 56
    , EmojiData "flag rw" 4 0
    , EmojiData "flag sa" 4 1
    , EmojiData "flag sb" 4 2
    , EmojiData "flag sc" 4 3
    , EmojiData "flag sd" 4 4
    , EmojiData "flag se" 4 5
    , EmojiData "flag sg" 4 6
    , EmojiData "flag sh" 4 7
    , EmojiData "flag si" 4 8
    , EmojiData "flag sj" 4 9
    , EmojiData "flag sk" 4 10
    , EmojiData "flag sl" 4 11
    , EmojiData "flag sm" 4 12
    , EmojiData "flag sn" 4 13
    , EmojiData "flag so" 4 14
    , EmojiData "flag sr" 4 15
    , EmojiData "flag ss" 4 16
    , EmojiData "flag st" 4 17
    , EmojiData "flag sv" 4 18
    , EmojiData "flag sx" 4 19
    , EmojiData "flag sy" 4 20
    , EmojiData "flag sz" 4 21
    , EmojiData "flag ta" 4 22
    , EmojiData "flag tc" 4 23
    , EmojiData "flag td" 4 24
    , EmojiData "flag tf" 4 25
    , EmojiData "flag tg" 4 26
    , EmojiData "flag th" 4 27
    , EmojiData "flag tj" 4 28
    , EmojiData "flag tk" 4 29
    , EmojiData "flag tl" 4 30
    , EmojiData "flag tm" 4 31
    , EmojiData "flag tn" 4 32
    , EmojiData "flag to" 4 33
    , EmojiData "flag tr" 4 34
    , EmojiData "flag tt" 4 35
    , EmojiData "flag tv" 4 36
    , EmojiData "flag tw" 4 37
    , EmojiData "flag tz" 4 38
    , EmojiData "flag ua" 4 39
    , EmojiData "flag ug" 4 40
    , EmojiData "flag um" 4 41
    , EmojiData "flag un" 4 42
    , EmojiData "us" 4 43
    , EmojiData "flag uy" 4 44
    , EmojiData "flag uz" 4 45
    , EmojiData "flag va" 4 46
    , EmojiData "flag vc" 4 47
    , EmojiData "flag ve" 4 48
    , EmojiData "flag vg" 4 49
    , EmojiData "flag vi" 4 50
    , EmojiData "flag vn" 4 51
    , EmojiData "flag vu" 4 52
    , EmojiData "flag wf" 4 53
    , EmojiData "flag ws" 4 54
    , EmojiData "flag xk" 4 55
    , EmojiData "flag ye" 4 56
    , EmojiData "flag yt" 5 0
    , EmojiData "flag za" 5 1
    , EmojiData "flag zm" 5 2
    , EmojiData "flag zw" 5 3
    , EmojiData "koko" 5 4
    , EmojiData "sa" 5 5
    , EmojiData "u7121" 5 6
    , EmojiData "u6307" 5 7
    , EmojiData "u7981" 5 8
    , EmojiData "u7a7a" 5 9
    , EmojiData "u5408" 5 10
    , EmojiData "u6e80" 5 11
    , EmojiData "u6709" 5 12
    , EmojiData "u6708" 5 13
    , EmojiData "u7533" 5 14
    , EmojiData "u5272" 5 15
    , EmojiData "u55b6" 5 16
    , EmojiData "ideograph advantage" 5 17
    , EmojiData "accept" 5 18
    , EmojiData "cyclone" 5 19
    , EmojiData "foggy" 5 20
    , EmojiData "closed umbrella" 5 21
    , EmojiData "night with stars" 5 22
    , EmojiData "sunrise over mountains" 5 23
    , EmojiData "sunrise" 5 24
    , EmojiData "city sunset" 5 25
    , EmojiData "city sunrise" 5 26
    , EmojiData "rainbow" 5 27
    , EmojiData "bridge at night" 5 28
    , EmojiData "ocean" 5 29
    , EmojiData "volcano" 5 30
    , EmojiData "milky way" 5 31
    , EmojiData "earth africa" 5 32
    , EmojiData "earth americas" 5 33
    , EmojiData "earth asia" 5 34
    , EmojiData "globe with meridians" 5 35
    , EmojiData "new moon" 5 36
    , EmojiData "waxing crescent moon" 5 37
    , EmojiData "first quarter moon" 5 38
    , EmojiData "moon" 5 39
    , EmojiData "full moon" 5 40
    , EmojiData "waning gibbous moon" 5 41
    , EmojiData "last quarter moon" 5 42
    , EmojiData "waning crescent moon" 5 43
    , EmojiData "crescent moon" 5 44
    , EmojiData "new moon with face" 5 45
    , EmojiData "first quarter moon with face" 5 46
    , EmojiData "last quarter moon with face" 5 47
    , EmojiData "full moon with face" 5 48
    , EmojiData "sun with face" 5 49
    , EmojiData "star2" 5 50
    , EmojiData "stars" 5 51
    , EmojiData "thermometer" 5 52
    , EmojiData "mostly sunny" 5 53
    , EmojiData "barely sunny" 5 54
    , EmojiData "partly sunny rain" 5 55
    , EmojiData "rain cloud" 5 56
    , EmojiData "snow cloud" 6 0
    , EmojiData "lightning" 6 1
    , EmojiData "tornado" 6 2
    , EmojiData "fog" 6 3
    , EmojiData "wind blowing face" 6 4
    , EmojiData "hotdog" 6 5
    , EmojiData "taco" 6 6
    , EmojiData "burrito" 6 7
    , EmojiData "chestnut" 6 8
    , EmojiData "seedling" 6 9
    , EmojiData "evergreen tree" 6 10
    , EmojiData "deciduous tree" 6 11
    , EmojiData "palm tree" 6 12
    , EmojiData "cactus" 6 13
    , EmojiData "hot pepper" 6 14
    , EmojiData "tulip" 6 15
    , EmojiData "cherry blossom" 6 16
    , EmojiData "rose" 6 17
    , EmojiData "hibiscus" 6 18
    , EmojiData "sunflower" 6 19
    , EmojiData "blossom" 6 20
    , EmojiData "corn" 6 21
    , EmojiData "ear of rice" 6 22
    , EmojiData "herb" 6 23
    , EmojiData "four leaf clover" 6 24
    , EmojiData "maple leaf" 6 25
    , EmojiData "fallen leaf" 6 26
    , EmojiData "leaves" 6 27
    , EmojiData "mushroom" 6 28
    , EmojiData "tomato" 6 29
    , EmojiData "eggplant" 6 30
    , EmojiData "grapes" 6 31
    , EmojiData "melon" 6 32
    , EmojiData "watermelon" 6 33
    , EmojiData "tangerine" 6 34
    , EmojiData "lemon" 6 35
    , EmojiData "banana" 6 36
    , EmojiData "pineapple" 6 37
    , EmojiData "apple" 6 38
    , EmojiData "green apple" 6 39
    , EmojiData "pear" 6 40
    , EmojiData "peach" 6 41
    , EmojiData "cherries" 6 42
    , EmojiData "strawberry" 6 43
    , EmojiData "hamburger" 6 44
    , EmojiData "pizza" 6 45
    , EmojiData "meat on bone" 6 46
    , EmojiData "poultry leg" 6 47
    , EmojiData "rice cracker" 6 48
    , EmojiData "rice ball" 6 49
    , EmojiData "rice" 6 50
    , EmojiData "curry" 6 51
    , EmojiData "ramen" 6 52
    , EmojiData "spaghetti" 6 53
    , EmojiData "bread" 6 54
    , EmojiData "fries" 6 55
    , EmojiData "sweet potato" 6 56
    , EmojiData "dango" 7 0
    , EmojiData "oden" 7 1
    , EmojiData "sushi" 7 2
    , EmojiData "fried shrimp" 7 3
    , EmojiData "fish cake" 7 4
    , EmojiData "icecream" 7 5
    , EmojiData "shaved ice" 7 6
    , EmojiData "ice cream" 7 7
    , EmojiData "doughnut" 7 8
    , EmojiData "cookie" 7 9
    , EmojiData "chocolate bar" 7 10
    , EmojiData "candy" 7 11
    , EmojiData "lollipop" 7 12
    , EmojiData "custard" 7 13
    , EmojiData "honey pot" 7 14
    , EmojiData "cake" 7 15
    , EmojiData "bento" 7 16
    , EmojiData "stew" 7 17
    , EmojiData "fried egg" 7 18
    , EmojiData "fork and knife" 7 19
    , EmojiData "tea" 7 20
    , EmojiData "sake" 7 21
    , EmojiData "wine glass" 7 22
    , EmojiData "cocktail" 7 23
    , EmojiData "tropical drink" 7 24
    , EmojiData "beer" 7 25
    , EmojiData "beers" 7 26
    , EmojiData "baby bottle" 7 27
    , EmojiData "knife fork plate" 7 28
    , EmojiData "champagne" 7 29
    , EmojiData "popcorn" 7 30
    , EmojiData "ribbon" 7 31
    , EmojiData "gift" 7 32
    , EmojiData "birthday" 7 33
    , EmojiData "jack o lantern" 7 34
    , EmojiData "christmas tree" 7 35
    , EmojiData "santa" 7 36
    , EmojiData "fireworks" 7 42
    , EmojiData "sparkler" 7 43
    , EmojiData "balloon" 7 44
    , EmojiData "tada" 7 45
    , EmojiData "confetti ball" 7 46
    , EmojiData "tanabata tree" 7 47
    , EmojiData "crossed flags" 7 48
    , EmojiData "bamboo" 7 49
    , EmojiData "dolls" 7 50
    , EmojiData "flags" 7 51
    , EmojiData "wind chime" 7 52
    , EmojiData "rice scene" 7 53
    , EmojiData "school satchel" 7 54
    , EmojiData "mortar board" 7 55
    , EmojiData "medal" 7 56
    , EmojiData "reminder ribbon" 8 0
    , EmojiData "studio microphone" 8 1
    , EmojiData "level slider" 8 2
    , EmojiData "control knobs" 8 3
    , EmojiData "film frames" 8 4
    , EmojiData "admission tickets" 8 5
    , EmojiData "carousel horse" 8 6
    , EmojiData "ferris wheel" 8 7
    , EmojiData "roller coaster" 8 8
    , EmojiData "fishing pole and fish" 8 9
    , EmojiData "microphone" 8 10
    , EmojiData "movie camera" 8 11
    , EmojiData "cinema" 8 12
    , EmojiData "headphones" 8 13
    , EmojiData "art" 8 14
    , EmojiData "tophat" 8 15
    , EmojiData "circus tent" 8 16
    , EmojiData "ticket" 8 17
    , EmojiData "clapper" 8 18
    , EmojiData "performing arts" 8 19
    , EmojiData "video game" 8 20
    , EmojiData "dart" 8 21
    , EmojiData "slot machine" 8 22
    , EmojiData "8ball" 8 23
    , EmojiData "game die" 8 24
    , EmojiData "bowling" 8 25
    , EmojiData "flower playing cards" 8 26
    , EmojiData "musical note" 8 27
    , EmojiData "notes" 8 28
    , EmojiData "saxophone" 8 29
    , EmojiData "guitar" 8 30
    , EmojiData "musical keyboard" 8 31
    , EmojiData "trumpet" 8 32
    , EmojiData "violin" 8 33
    , EmojiData "musical score" 8 34
    , EmojiData "running shirt with sash" 8 35
    , EmojiData "tennis" 8 36
    , EmojiData "ski" 8 37
    , EmojiData "basketball" 8 38
    , EmojiData "checkered flag" 8 39
    , EmojiData "snowboarder" 8 40
    , EmojiData "woman running" 8 46
    , EmojiData "man running" 8 52
    , EmojiData "runner" 9 1
    , EmojiData "woman surfing" 9 7
    , EmojiData "man surfing" 9 13
    , EmojiData "surfer" 9 19
    , EmojiData "sports medal" 9 25
    , EmojiData "trophy" 9 26
    , EmojiData "horse racing" 9 27
    , EmojiData "football" 9 33
    , EmojiData "rugby football" 9 34
    , EmojiData "woman swimming" 9 35
    , EmojiData "man swimming" 9 41
    , EmojiData "swimmer" 9 47
    , EmojiData "woman lifting weights" 9 53
    , EmojiData "man lifting weights" 10 2
    , EmojiData "weight lifter" 10 8
    , EmojiData "woman golfing" 10 14
    , EmojiData "man golfing" 10 20
    , EmojiData "golfer" 10 26
    , EmojiData "racing motorcycle" 10 32
    , EmojiData "racing car" 10 33
    , EmojiData "cricket bat and ball" 10 34
    , EmojiData "volleyball" 10 35
    , EmojiData "field hockey stick and ball" 10 36
    , EmojiData "ice hockey stick and puck" 10 37
    , EmojiData "table tennis paddle and ball" 10 38
    , EmojiData "snow capped mountain" 10 39
    , EmojiData "camping" 10 40
    , EmojiData "beach with umbrella" 10 41
    , EmojiData "building construction" 10 42
    , EmojiData "house buildings" 10 43
    , EmojiData "cityscape" 10 44
    , EmojiData "derelict house building" 10 45
    , EmojiData "classical building" 10 46
    , EmojiData "desert" 10 47
    , EmojiData "desert island" 10 48
    , EmojiData "national park" 10 49
    , EmojiData "stadium" 10 50
    , EmojiData "house" 10 51
    , EmojiData "house with garden" 10 52
    , EmojiData "office" 10 53
    , EmojiData "post office" 10 54
    , EmojiData "european post office" 10 55
    , EmojiData "hospital" 10 56
    , EmojiData "bank" 11 0
    , EmojiData "atm" 11 1
    , EmojiData "hotel" 11 2
    , EmojiData "love hotel" 11 3
    , EmojiData "convenience store" 11 4
    , EmojiData "school" 11 5
    , EmojiData "department store" 11 6
    , EmojiData "factory" 11 7
    , EmojiData "izakaya lantern" 11 8
    , EmojiData "japanese castle" 11 9
    , EmojiData "european castle" 11 10
    , EmojiData "rainbow flag" 11 11
    , EmojiData "waving white flag" 11 12
    , EmojiData "pirate flag" 11 13
    , EmojiData "flag england" 11 14
    , EmojiData "flag scotland" 11 15
    , EmojiData "flag wales" 11 16
    , EmojiData "waving black flag" 11 17
    , EmojiData "rosette" 11 18
    , EmojiData "label" 11 19
    , EmojiData "badminton racquet and shuttlecock" 11 20
    , EmojiData "bow and arrow" 11 21
    , EmojiData "amphora" 11 22
    , EmojiData "skin tone 2" 11 23
    , EmojiData "skin tone 3" 11 24
    , EmojiData "skin tone 4" 11 25
    , EmojiData "skin tone 5" 11 26
    , EmojiData "skin tone 6" 11 27
    , EmojiData "rat" 11 28
    , EmojiData "mouse2" 11 29
    , EmojiData "ox" 11 30
    , EmojiData "water buffalo" 11 31
    , EmojiData "cow2" 11 32
    , EmojiData "tiger2" 11 33
    , EmojiData "leopard" 11 34
    , EmojiData "rabbit2" 11 35
    , EmojiData "cat2" 11 36
    , EmojiData "dragon" 11 37
    , EmojiData "crocodile" 11 38
    , EmojiData "whale2" 11 39
    , EmojiData "snail" 11 40
    , EmojiData "snake" 11 41
    , EmojiData "racehorse" 11 42
    , EmojiData "ram" 11 43
    , EmojiData "goat" 11 44
    , EmojiData "sheep" 11 45
    , EmojiData "monkey" 11 46
    , EmojiData "rooster" 11 47
    , EmojiData "chicken" 11 48
    , EmojiData "service dog" 11 49
    , EmojiData "dog2" 11 50
    , EmojiData "pig2" 11 51
    , EmojiData "boar" 11 52
    , EmojiData "elephant" 11 53
    , EmojiData "octopus" 11 54
    , EmojiData "shell" 11 55
    , EmojiData "bug" 11 56
    , EmojiData "ant" 12 0
    , EmojiData "bee" 12 1
    , EmojiData "beetle" 12 2
    , EmojiData "fish" 12 3
    , EmojiData "tropical fish" 12 4
    , EmojiData "blowfish" 12 5
    , EmojiData "turtle" 12 6
    , EmojiData "hatching chick" 12 7
    , EmojiData "baby chick" 12 8
    , EmojiData "hatched chick" 12 9
    , EmojiData "bird" 12 10
    , EmojiData "penguin" 12 11
    , EmojiData "koala" 12 12
    , EmojiData "poodle" 12 13
    , EmojiData "dromedary camel" 12 14
    , EmojiData "camel" 12 15
    , EmojiData "dolphin" 12 16
    , EmojiData "mouse" 12 17
    , EmojiData "cow" 12 18
    , EmojiData "tiger" 12 19
    , EmojiData "rabbit" 12 20
    , EmojiData "cat" 12 21
    , EmojiData "dragon face" 12 22
    , EmojiData "whale" 12 23
    , EmojiData "horse" 12 24
    , EmojiData "monkey face" 12 25
    , EmojiData "dog" 12 26
    , EmojiData "pig" 12 27
    , EmojiData "frog" 12 28
    , EmojiData "hamster" 12 29
    , EmojiData "wolf" 12 30
    , EmojiData "bear" 12 31
    , EmojiData "panda face" 12 32
    , EmojiData "pig nose" 12 33
    , EmojiData "feet" 12 34
    , EmojiData "chipmunk" 12 35
    , EmojiData "eyes" 12 36
    , EmojiData "eye in speech bubble" 12 37
    , EmojiData "eye" 12 38
    , EmojiData "ear" 12 39
    , EmojiData "nose" 12 45
    , EmojiData "lips" 12 51
    , EmojiData "tongue" 12 52
    , EmojiData "point up 2" 12 53
    , EmojiData "point down" 13 2
    , EmojiData "point left" 13 8
    , EmojiData "point right" 13 14
    , EmojiData "facepunch" 13 20
    , EmojiData "wave" 13 26
    , EmojiData "ok hand" 13 32
    , EmojiData "+1" 13 38
    , EmojiData " 1" 13 44
    , EmojiData "clap" 13 50
    , EmojiData "open hands" 13 56
    , EmojiData "crown" 14 5
    , EmojiData "womans hat" 14 6
    , EmojiData "eyeglasses" 14 7
    , EmojiData "necktie" 14 8
    , EmojiData "shirt" 14 9
    , EmojiData "jeans" 14 10
    , EmojiData "dress" 14 11
    , EmojiData "kimono" 14 12
    , EmojiData "bikini" 14 13
    , EmojiData "womans clothes" 14 14
    , EmojiData "purse" 14 15
    , EmojiData "handbag" 14 16
    , EmojiData "pouch" 14 17
    , EmojiData "mans shoe" 14 18
    , EmojiData "athletic shoe" 14 19
    , EmojiData "high heel" 14 20
    , EmojiData "sandal" 14 21
    , EmojiData "boot" 14 22
    , EmojiData "footprints" 14 23
    , EmojiData "bust in silhouette" 14 24
    , EmojiData "busts in silhouette" 14 25
    , EmojiData "boy" 14 26
    , EmojiData "girl" 14 32
    , EmojiData "male farmer" 14 38
    , EmojiData "male cook" 14 44
    , EmojiData "male student" 14 50
    , EmojiData "male singer" 14 56
    , EmojiData "male artist" 15 5
    , EmojiData "male teacher" 15 11
    , EmojiData "male factory worker" 15 17
    , EmojiData "man boy boy" 15 23
    , EmojiData "man boy" 15 24
    , EmojiData "man girl boy" 15 25
    , EmojiData "man girl girl" 15 26
    , EmojiData "man girl" 15 27
    , EmojiData "man man boy" 15 28
    , EmojiData "man man boy boy" 15 29
    , EmojiData "man man girl" 15 30
    , EmojiData "man man girl boy" 15 31
    , EmojiData "man man girl girl" 15 32
    , EmojiData "man woman boy" 15 33
    , EmojiData "man woman boy boy" 15 34
    , EmojiData "man woman girl" 15 35
    , EmojiData "man woman girl boy" 15 36
    , EmojiData "man woman girl girl" 15 37
    , EmojiData "male technologist" 15 38
    , EmojiData "male office worker" 15 44
    , EmojiData "male mechanic" 15 50
    , EmojiData "male scientist" 15 56
    , EmojiData "male astronaut" 16 5
    , EmojiData "male firefighter" 16 11
    , EmojiData "man with probing cane" 16 17
    , EmojiData "red haired man" 16 23
    , EmojiData "curly haired man" 16 29
    , EmojiData "bald man" 16 35
    , EmojiData "white haired man" 16 41
    , EmojiData "man in motorized wheelchair" 16 47
    , EmojiData "man in manual wheelchair" 16 53
    , EmojiData "male doctor" 17 2
    , EmojiData "male judge" 17 8
    , EmojiData "male pilot" 17 14
    , EmojiData "man heart man" 17 20
    , EmojiData "man kiss man" 17 21
    , EmojiData "man" 17 22
    , EmojiData "female farmer" 17 28
    , EmojiData "female cook" 17 34
    , EmojiData "female student" 17 40
    , EmojiData "female singer" 17 46
    , EmojiData "female artist" 17 52
    , EmojiData "female teacher" 18 1
    , EmojiData "female factory worker" 18 7
    , EmojiData "woman boy boy" 18 13
    , EmojiData "woman boy" 18 14
    , EmojiData "woman girl boy" 18 15
    , EmojiData "woman girl girl" 18 16
    , EmojiData "woman girl" 18 17
    , EmojiData "woman woman boy" 18 18
    , EmojiData "woman woman boy boy" 18 19
    , EmojiData "woman woman girl" 18 20
    , EmojiData "woman woman girl boy" 18 21
    , EmojiData "woman woman girl girl" 18 22
    , EmojiData "female technologist" 18 23
    , EmojiData "female office worker" 18 29
    , EmojiData "female mechanic" 18 35
    , EmojiData "female scientist" 18 41
    , EmojiData "female astronaut" 18 47
    , EmojiData "female firefighter" 18 53
    , EmojiData "woman with probing cane" 19 2
    , EmojiData "red haired woman" 19 8
    , EmojiData "curly haired woman" 19 14
    , EmojiData "bald woman" 19 20
    , EmojiData "white haired woman" 19 26
    , EmojiData "woman in motorized wheelchair" 19 32
    , EmojiData "woman in manual wheelchair" 19 38
    , EmojiData "female doctor" 19 44
    , EmojiData "female judge" 19 50
    , EmojiData "female pilot" 19 56
    , EmojiData "woman heart man" 20 5
    , EmojiData "woman heart woman" 20 6
    , EmojiData "woman kiss man" 20 7
    , EmojiData "woman kiss woman" 20 8
    , EmojiData "woman" 20 9
    , EmojiData "family" 20 15
    , EmojiData "couple" 20 16
    , EmojiData "two men holding hands" 20 42
    , EmojiData "two women holding hands" 21 11
    , EmojiData "female police officer" 21 37
    , EmojiData "male police officer" 21 43
    , EmojiData "cop" 21 49
    , EmojiData "woman with bunny ears partying" 21 55
    , EmojiData "man with bunny ears partying" 21 56
    , EmojiData "dancers" 22 0
    , EmojiData "bride with veil" 22 1
    , EmojiData "blond haired woman" 22 7
    , EmojiData "blond haired man" 22 13
    , EmojiData "person with blond hair" 22 19
    , EmojiData "man with gua pi mao" 22 25
    , EmojiData "woman wearing turban" 22 31
    , EmojiData "man wearing turban" 22 37
    , EmojiData "man with turban" 22 43
    , EmojiData "older man" 22 49
    , EmojiData "older woman" 22 55
    , EmojiData "baby" 23 4
    , EmojiData "female construction worker" 23 10
    , EmojiData "male construction worker" 23 16
    , EmojiData "construction worker" 23 22
    , EmojiData "princess" 23 28
    , EmojiData "japanese ogre" 23 34
    , EmojiData "japanese goblin" 23 35
    , EmojiData "ghost" 23 36
    , EmojiData "angel" 23 37
    , EmojiData "alien" 23 43
    , EmojiData "space invader" 23 44
    , EmojiData "imp" 23 45
    , EmojiData "skull" 23 46
    , EmojiData "woman tipping hand" 23 47
    , EmojiData "man tipping hand" 23 53
    , EmojiData "information desk person" 24 2
    , EmojiData "female guard" 24 8
    , EmojiData "male guard" 24 14
    , EmojiData "guardsman" 24 20
    , EmojiData "dancer" 24 26
    , EmojiData "lipstick" 24 32
    , EmojiData "nail care" 24 33
    , EmojiData "woman getting massage" 24 39
    , EmojiData "man getting massage" 24 45
    , EmojiData "massage" 24 51
    , EmojiData "woman getting haircut" 25 0
    , EmojiData "man getting haircut" 25 6
    , EmojiData "haircut" 25 12
    , EmojiData "barber" 25 18
    , EmojiData "syringe" 25 19
    , EmojiData "pill" 25 20
    , EmojiData "kiss" 25 21
    , EmojiData "love letter" 25 22
    , EmojiData "ring" 25 23
    , EmojiData "gem" 25 24
    , EmojiData "couplekiss" 25 25
    , EmojiData "bouquet" 25 26
    , EmojiData "couple with heart" 25 27
    , EmojiData "wedding" 25 28
    , EmojiData "heartbeat" 25 29
    , EmojiData "broken heart" 25 30
    , EmojiData "two hearts" 25 31
    , EmojiData "sparkling heart" 25 32
    , EmojiData "heartpulse" 25 33
    , EmojiData "cupid" 25 34
    , EmojiData "blue heart" 25 35
    , EmojiData "green heart" 25 36
    , EmojiData "yellow heart" 25 37
    , EmojiData "purple heart" 25 38
    , EmojiData "gift heart" 25 39
    , EmojiData "revolving hearts" 25 40
    , EmojiData "heart decoration" 25 41
    , EmojiData "diamond shape with a dot inside" 25 42
    , EmojiData "bulb" 25 43
    , EmojiData "anger" 25 44
    , EmojiData "bomb" 25 45
    , EmojiData "zzz" 25 46
    , EmojiData "boom" 25 47
    , EmojiData "sweat drops" 25 48
    , EmojiData "droplet" 25 49
    , EmojiData "dash" 25 50
    , EmojiData "hankey" 25 51
    , EmojiData "muscle" 25 52
    , EmojiData "dizzy" 26 1
    , EmojiData "speech balloon" 26 2
    , EmojiData "thought balloon" 26 3
    , EmojiData "white flower" 26 4
    , EmojiData "100" 26 5
    , EmojiData "moneybag" 26 6
    , EmojiData "currency exchange" 26 7
    , EmojiData "heavy dollar sign" 26 8
    , EmojiData "credit card" 26 9
    , EmojiData "yen" 26 10
    , EmojiData "dollar" 26 11
    , EmojiData "euro" 26 12
    , EmojiData "pound" 26 13
    , EmojiData "money with wings" 26 14
    , EmojiData "chart" 26 15
    , EmojiData "seat" 26 16
    , EmojiData "computer" 26 17
    , EmojiData "briefcase" 26 18
    , EmojiData "minidisc" 26 19
    , EmojiData "floppy disk" 26 20
    , EmojiData "cd" 26 21
    , EmojiData "dvd" 26 22
    , EmojiData "file folder" 26 23
    , EmojiData "open file folder" 26 24
    , EmojiData "page with curl" 26 25
    , EmojiData "page facing up" 26 26
    , EmojiData "date" 26 27
    , EmojiData "calendar" 26 28
    , EmojiData "card index" 26 29
    , EmojiData "chart with upwards trend" 26 30
    , EmojiData "chart with downwards trend" 26 31
    , EmojiData "bar chart" 26 32
    , EmojiData "clipboard" 26 33
    , EmojiData "pushpin" 26 34
    , EmojiData "round pushpin" 26 35
    , EmojiData "paperclip" 26 36
    , EmojiData "straight ruler" 26 37
    , EmojiData "triangular ruler" 26 38
    , EmojiData "bookmark tabs" 26 39
    , EmojiData "ledger" 26 40
    , EmojiData "notebook" 26 41
    , EmojiData "notebook with decorative cover" 26 42
    , EmojiData "closed book" 26 43
    , EmojiData "book" 26 44
    , EmojiData "green book" 26 45
    , EmojiData "blue book" 26 46
    , EmojiData "orange book" 26 47
    , EmojiData "books" 26 48
    , EmojiData "name badge" 26 49
    , EmojiData "scroll" 26 50
    , EmojiData "memo" 26 51
    , EmojiData "telephone receiver" 26 52
    , EmojiData "pager" 26 53
    , EmojiData "fax" 26 54
    , EmojiData "satellite antenna" 26 55
    , EmojiData "loudspeaker" 26 56
    , EmojiData "mega" 27 0
    , EmojiData "outbox tray" 27 1
    , EmojiData "inbox tray" 27 2
    , EmojiData "package" 27 3
    , EmojiData "e mail" 27 4
    , EmojiData "incoming envelope" 27 5
    , EmojiData "envelope with arrow" 27 6
    , EmojiData "mailbox closed" 27 7
    , EmojiData "mailbox" 27 8
    , EmojiData "mailbox with mail" 27 9
    , EmojiData "mailbox with no mail" 27 10
    , EmojiData "postbox" 27 11
    , EmojiData "postal horn" 27 12
    , EmojiData "newspaper" 27 13
    , EmojiData "iphone" 27 14
    , EmojiData "calling" 27 15
    , EmojiData "vibration mode" 27 16
    , EmojiData "mobile phone off" 27 17
    , EmojiData "no mobile phones" 27 18
    , EmojiData "signal strength" 27 19
    , EmojiData "camera" 27 20
    , EmojiData "camera with flash" 27 21
    , EmojiData "video camera" 27 22
    , EmojiData "tv" 27 23
    , EmojiData "radio" 27 24
    , EmojiData "vhs" 27 25
    , EmojiData "film projector" 27 26
    , EmojiData "prayer beads" 27 27
    , EmojiData "twisted rightwards arrows" 27 28
    , EmojiData "repeat" 27 29
    , EmojiData "repeat one" 27 30
    , EmojiData "arrows clockwise" 27 31
    , EmojiData "arrows counterclockwise" 27 32
    , EmojiData "low brightness" 27 33
    , EmojiData "high brightness" 27 34
    , EmojiData "mute" 27 35
    , EmojiData "speaker" 27 36
    , EmojiData "sound" 27 37
    , EmojiData "loud sound" 27 38
    , EmojiData "battery" 27 39
    , EmojiData "electric plug" 27 40
    , EmojiData "mag" 27 41
    , EmojiData "mag right" 27 42
    , EmojiData "lock with ink pen" 27 43
    , EmojiData "closed lock with key" 27 44
    , EmojiData "key" 27 45
    , EmojiData "lock" 27 46
    , EmojiData "unlock" 27 47
    , EmojiData "bell" 27 48
    , EmojiData "no bell" 27 49
    , EmojiData "bookmark" 27 50
    , EmojiData "link" 27 51
    , EmojiData "radio button" 27 52
    , EmojiData "back" 27 53
    , EmojiData "end" 27 54
    , EmojiData "on" 27 55
    , EmojiData "soon" 27 56
    , EmojiData "top" 28 0
    , EmojiData "underage" 28 1
    , EmojiData "keycap ten" 28 2
    , EmojiData "capital abcd" 28 3
    , EmojiData "abcd" 28 4
    , EmojiData "1234" 28 5
    , EmojiData "symbols" 28 6
    , EmojiData "abc" 28 7
    , EmojiData "fire" 28 8
    , EmojiData "flashlight" 28 9
    , EmojiData "wrench" 28 10
    , EmojiData "hammer" 28 11
    , EmojiData "nut and bolt" 28 12
    , EmojiData "hocho" 28 13
    , EmojiData "gun" 28 14
    , EmojiData "microscope" 28 15
    , EmojiData "telescope" 28 16
    , EmojiData "crystal ball" 28 17
    , EmojiData "six pointed star" 28 18
    , EmojiData "beginner" 28 19
    , EmojiData "trident" 28 20
    , EmojiData "black square button" 28 21
    , EmojiData "white square button" 28 22
    , EmojiData "red circle" 28 23
    , EmojiData "large blue circle" 28 24
    , EmojiData "large orange diamond" 28 25
    , EmojiData "large blue diamond" 28 26
    , EmojiData "small orange diamond" 28 27
    , EmojiData "small blue diamond" 28 28
    , EmojiData "small red triangle" 28 29
    , EmojiData "small red triangle down" 28 30
    , EmojiData "arrow up small" 28 31
    , EmojiData "arrow down small" 28 32
    , EmojiData "om symbol" 28 33
    , EmojiData "dove of peace" 28 34
    , EmojiData "kaaba" 28 35
    , EmojiData "mosque" 28 36
    , EmojiData "synagogue" 28 37
    , EmojiData "menorah with nine branches" 28 38
    , EmojiData "clock1" 28 39
    , EmojiData "clock2" 28 40
    , EmojiData "clock3" 28 41
    , EmojiData "clock4" 28 42
    , EmojiData "clock5" 28 43
    , EmojiData "clock6" 28 44
    , EmojiData "clock7" 28 45
    , EmojiData "clock8" 28 46
    , EmojiData "clock9" 28 47
    , EmojiData "clock10" 28 48
    , EmojiData "clock11" 28 49
    , EmojiData "clock12" 28 50
    , EmojiData "clock130" 28 51
    , EmojiData "clock230" 28 52
    , EmojiData "clock330" 28 53
    , EmojiData "clock430" 28 54
    , EmojiData "clock530" 28 55
    , EmojiData "clock630" 28 56
    , EmojiData "clock730" 29 0
    , EmojiData "clock830" 29 1
    , EmojiData "clock930" 29 2
    , EmojiData "clock1030" 29 3
    , EmojiData "clock1130" 29 4
    , EmojiData "clock1230" 29 5
    , EmojiData "candle" 29 6
    , EmojiData "mantelpiece clock" 29 7
    , EmojiData "hole" 29 8
    , EmojiData "man in business suit levitating" 29 9
    , EmojiData "female detective" 29 15
    , EmojiData "male detective" 29 21
    , EmojiData "sleuth or spy" 29 27
    , EmojiData "dark sunglasses" 29 33
    , EmojiData "spider" 29 34
    , EmojiData "spider web" 29 35
    , EmojiData "joystick" 29 36
    , EmojiData "man dancing" 29 37
    , EmojiData "linked paperclips" 29 43
    , EmojiData "lower left ballpoint pen" 29 44
    , EmojiData "lower left fountain pen" 29 45
    , EmojiData "lower left paintbrush" 29 46
    , EmojiData "lower left crayon" 29 47
    , EmojiData "raised hand with fingers splayed" 29 48
    , EmojiData "middle finger" 29 54
    , EmojiData "spock hand" 30 3
    , EmojiData "black heart" 30 9
    , EmojiData "desktop computer" 30 10
    , EmojiData "printer" 30 11
    , EmojiData "three button mouse" 30 12
    , EmojiData "trackball" 30 13
    , EmojiData "frame with picture" 30 14
    , EmojiData "card index dividers" 30 15
    , EmojiData "card file box" 30 16
    , EmojiData "file cabinet" 30 17
    , EmojiData "wastebasket" 30 18
    , EmojiData "spiral note pad" 30 19
    , EmojiData "spiral calendar pad" 30 20
    , EmojiData "compression" 30 21
    , EmojiData "old key" 30 22
    , EmojiData "rolled up newspaper" 30 23
    , EmojiData "dagger knife" 30 24
    , EmojiData "speaking head in silhouette" 30 25
    , EmojiData "left speech bubble" 30 26
    , EmojiData "right anger bubble" 30 27
    , EmojiData "ballot box with ballot" 30 28
    , EmojiData "world map" 30 29
    , EmojiData "mount fuji" 30 30
    , EmojiData "tokyo tower" 30 31
    , EmojiData "statue of liberty" 30 32
    , EmojiData "japan" 30 33
    , EmojiData "moyai" 30 34
    , EmojiData "grinning" 30 35
    , EmojiData "grin" 30 36
    , EmojiData "joy" 30 37
    , EmojiData "smiley" 30 38
    , EmojiData "smile" 30 39
    , EmojiData "sweat smile" 30 40
    , EmojiData "laughing" 30 41
    , EmojiData "innocent" 30 42
    , EmojiData "smiling imp" 30 43
    , EmojiData "wink" 30 44
    , EmojiData "blush" 30 45
    , EmojiData "yum" 30 46
    , EmojiData "relieved" 30 47
    , EmojiData "heart eyes" 30 48
    , EmojiData "sunglasses" 30 49
    , EmojiData "smirk" 30 50
    , EmojiData "neutral face" 30 51
    , EmojiData "expressionless" 30 52
    , EmojiData "unamused" 30 53
    , EmojiData "sweat" 30 54
    , EmojiData "pensive" 30 55
    , EmojiData "confused" 30 56
    , EmojiData "confounded" 31 0
    , EmojiData "kissing" 31 1
    , EmojiData "kissing heart" 31 2
    , EmojiData "kissing smiling eyes" 31 3
    , EmojiData "kissing closed eyes" 31 4
    , EmojiData "stuck out tongue" 31 5
    , EmojiData "stuck out tongue winking eye" 31 6
    , EmojiData "stuck out tongue closed eyes" 31 7
    , EmojiData "disappointed" 31 8
    , EmojiData "worried" 31 9
    , EmojiData "angry" 31 10
    , EmojiData "rage" 31 11
    , EmojiData "cry" 31 12
    , EmojiData "persevere" 31 13
    , EmojiData "triumph" 31 14
    , EmojiData "disappointed relieved" 31 15
    , EmojiData "frowning" 31 16
    , EmojiData "anguished" 31 17
    , EmojiData "fearful" 31 18
    , EmojiData "weary" 31 19
    , EmojiData "sleepy" 31 20
    , EmojiData "tired face" 31 21
    , EmojiData "grimacing" 31 22
    , EmojiData "sob" 31 23
    , EmojiData "open mouth" 31 24
    , EmojiData "hushed" 31 25
    , EmojiData "cold sweat" 31 26
    , EmojiData "scream" 31 27
    , EmojiData "astonished" 31 28
    , EmojiData "flushed" 31 29
    , EmojiData "sleeping" 31 30
    , EmojiData "dizzy face" 31 31
    , EmojiData "no mouth" 31 32
    , EmojiData "mask" 31 33
    , EmojiData "smile cat" 31 34
    , EmojiData "joy cat" 31 35
    , EmojiData "smiley cat" 31 36
    , EmojiData "heart eyes cat" 31 37
    , EmojiData "smirk cat" 31 38
    , EmojiData "kissing cat" 31 39
    , EmojiData "pouting cat" 31 40
    , EmojiData "crying cat face" 31 41
    , EmojiData "scream cat" 31 42
    , EmojiData "slightly frowning face" 31 43
    , EmojiData "slightly smiling face" 31 44
    , EmojiData "upside down face" 31 45
    , EmojiData "face with rolling eyes" 31 46
    , EmojiData "woman gesturing no" 31 47
    , EmojiData "man gesturing no" 31 53
    , EmojiData "no good" 32 2
    , EmojiData "woman gesturing ok" 32 8
    , EmojiData "man gesturing ok" 32 14
    , EmojiData "ok woman" 32 20
    , EmojiData "woman bowing" 32 26
    , EmojiData "man bowing" 32 32
    , EmojiData "bow" 32 38
    , EmojiData "see no evil" 32 44
    , EmojiData "hear no evil" 32 45
    , EmojiData "speak no evil" 32 46
    , EmojiData "woman raising hand" 32 47
    , EmojiData "man raising hand" 32 53
    , EmojiData "raising hand" 33 2
    , EmojiData "raised hands" 33 8
    , EmojiData "woman frowning" 33 14
    , EmojiData "man frowning" 33 20
    , EmojiData "person frowning" 33 26
    , EmojiData "woman pouting" 33 32
    , EmojiData "man pouting" 33 38
    , EmojiData "person with pouting face" 33 44
    , EmojiData "pray" 33 50
    , EmojiData "rocket" 33 56
    , EmojiData "helicopter" 34 0
    , EmojiData "steam locomotive" 34 1
    , EmojiData "railway car" 34 2
    , EmojiData "bullettrain side" 34 3
    , EmojiData "bullettrain front" 34 4
    , EmojiData "train2" 34 5
    , EmojiData "metro" 34 6
    , EmojiData "light rail" 34 7
    , EmojiData "station" 34 8
    , EmojiData "tram" 34 9
    , EmojiData "train" 34 10
    , EmojiData "bus" 34 11
    , EmojiData "oncoming bus" 34 12
    , EmojiData "trolleybus" 34 13
    , EmojiData "busstop" 34 14
    , EmojiData "minibus" 34 15
    , EmojiData "ambulance" 34 16
    , EmojiData "fire engine" 34 17
    , EmojiData "police car" 34 18
    , EmojiData "oncoming police car" 34 19
    , EmojiData "taxi" 34 20
    , EmojiData "oncoming taxi" 34 21
    , EmojiData "car" 34 22
    , EmojiData "oncoming automobile" 34 23
    , EmojiData "blue car" 34 24
    , EmojiData "truck" 34 25
    , EmojiData "articulated lorry" 34 26
    , EmojiData "tractor" 34 27
    , EmojiData "monorail" 34 28
    , EmojiData "mountain railway" 34 29
    , EmojiData "suspension railway" 34 30
    , EmojiData "mountain cableway" 34 31
    , EmojiData "aerial tramway" 34 32
    , EmojiData "ship" 34 33
    , EmojiData "woman rowing boat" 34 34
    , EmojiData "man rowing boat" 34 40
    , EmojiData "rowboat" 34 46
    , EmojiData "speedboat" 34 52
    , EmojiData "traffic light" 34 53
    , EmojiData "vertical traffic light" 34 54
    , EmojiData "construction" 34 55
    , EmojiData "rotating light" 34 56
    , EmojiData "triangular flag on post" 35 0
    , EmojiData "door" 35 1
    , EmojiData "no entry sign" 35 2
    , EmojiData "smoking" 35 3
    , EmojiData "no smoking" 35 4
    , EmojiData "put litter in its place" 35 5
    , EmojiData "do not litter" 35 6
    , EmojiData "potable water" 35 7
    , EmojiData "non potable water" 35 8
    , EmojiData "bike" 35 9
    , EmojiData "no bicycles" 35 10
    , EmojiData "woman biking" 35 11
    , EmojiData "man biking" 35 17
    , EmojiData "bicyclist" 35 23
    , EmojiData "woman mountain biking" 35 29
    , EmojiData "man mountain biking" 35 35
    , EmojiData "mountain bicyclist" 35 41
    , EmojiData "woman walking" 35 47
    , EmojiData "man walking" 35 53
    , EmojiData "walking" 36 2
    , EmojiData "no pedestrians" 36 8
    , EmojiData "children crossing" 36 9
    , EmojiData "mens" 36 10
    , EmojiData "womens" 36 11
    , EmojiData "restroom" 36 12
    , EmojiData "baby symbol" 36 13
    , EmojiData "toilet" 36 14
    , EmojiData "wc" 36 15
    , EmojiData "shower" 36 16
    , EmojiData "bath" 36 17
    , EmojiData "bathtub" 36 23
    , EmojiData "passport control" 36 24
    , EmojiData "customs" 36 25
    , EmojiData "baggage claim" 36 26
    , EmojiData "left luggage" 36 27
    , EmojiData "couch and lamp" 36 28
    , EmojiData "sleeping accommodation" 36 29
    , EmojiData "shopping bags" 36 35
    , EmojiData "bellhop bell" 36 36
    , EmojiData "bed" 36 37
    , EmojiData "place of worship" 36 38
    , EmojiData "octagonal sign" 36 39
    , EmojiData "shopping trolley" 36 40
    , EmojiData "hindu temple" 36 41
    , EmojiData "hammer and wrench" 36 42
    , EmojiData "shield" 36 43
    , EmojiData "oil drum" 36 44
    , EmojiData "motorway" 36 45
    , EmojiData "railway track" 36 46
    , EmojiData "motor boat" 36 47
    , EmojiData "small airplane" 36 48
    , EmojiData "airplane departure" 36 49
    , EmojiData "airplane arriving" 36 50
    , EmojiData "satellite" 36 51
    , EmojiData "passenger ship" 36 52
    , EmojiData "scooter" 36 53
    , EmojiData "motor scooter" 36 54
    , EmojiData "canoe" 36 55
    , EmojiData "sled" 36 56
    , EmojiData "flying saucer" 37 0
    , EmojiData "skateboard" 37 1
    , EmojiData "auto rickshaw" 37 2
    , EmojiData "large orange circle" 37 3
    , EmojiData "large yellow circle" 37 4
    , EmojiData "large green circle" 37 5
    , EmojiData "large purple circle" 37 6
    , EmojiData "large brown circle" 37 7
    , EmojiData "large red square" 37 8
    , EmojiData "large blue square" 37 9
    , EmojiData "large orange square" 37 10
    , EmojiData "large yellow square" 37 11
    , EmojiData "large green square" 37 12
    , EmojiData "large purple square" 37 13
    , EmojiData "large brown square" 37 14
    , EmojiData "white heart" 37 15
    , EmojiData "brown heart" 37 16
    , EmojiData "pinching hand" 37 17
    , EmojiData "zipper mouth face" 37 23
    , EmojiData "money mouth face" 37 24
    , EmojiData "face with thermometer" 37 25
    , EmojiData "nerd face" 37 26
    , EmojiData "thinking face" 37 27
    , EmojiData "face with head bandage" 37 28
    , EmojiData "robot face" 37 29
    , EmojiData "hugging face" 37 30
    , EmojiData "the horns" 37 31
    , EmojiData "call me hand" 37 37
    , EmojiData "raised back of hand" 37 43
    , EmojiData "left facing fist" 37 49
    , EmojiData "right facing fist" 37 55
    , EmojiData "handshake" 38 4
    , EmojiData "crossed fingers" 38 5
    , EmojiData "i love you hand sign" 38 11
    , EmojiData "face with cowboy hat" 38 17
    , EmojiData "clown face" 38 18
    , EmojiData "nauseated face" 38 19
    , EmojiData "rolling on the floor laughing" 38 20
    , EmojiData "drooling face" 38 21
    , EmojiData "lying face" 38 22
    , EmojiData "woman facepalming" 38 23
    , EmojiData "man facepalming" 38 29
    , EmojiData "face palm" 38 35
    , EmojiData "sneezing face" 38 41
    , EmojiData "face with raised eyebrow" 38 42
    , EmojiData "star struck" 38 43
    , EmojiData "zany face" 38 44
    , EmojiData "shushing face" 38 45
    , EmojiData "face with symbols on mouth" 38 46
    , EmojiData "face with hand over mouth" 38 47
    , EmojiData "face vomiting" 38 48
    , EmojiData "exploding head" 38 49
    , EmojiData "pregnant woman" 38 50
    , EmojiData "breast feeding" 38 56
    , EmojiData "palms up together" 39 5
    , EmojiData "selfie" 39 11
    , EmojiData "prince" 39 17
    , EmojiData "man in tuxedo" 39 23
    , EmojiData "mrs claus" 39 29
    , EmojiData "woman shrugging" 39 35
    , EmojiData "man shrugging" 39 41
    , EmojiData "shrug" 39 47
    , EmojiData "woman cartwheeling" 39 53
    , EmojiData "man cartwheeling" 40 2
    , EmojiData "person doing cartwheel" 40 8
    , EmojiData "woman juggling" 40 14
    , EmojiData "man juggling" 40 20
    , EmojiData "juggling" 40 26
    , EmojiData "fencer" 40 32
    , EmojiData "woman wrestling" 40 33
    , EmojiData "man wrestling" 40 34
    , EmojiData "wrestlers" 40 35
    , EmojiData "woman playing water polo" 40 36
    , EmojiData "man playing water polo" 40 42
    , EmojiData "water polo" 40 48
    , EmojiData "woman playing handball" 40 54
    , EmojiData "man playing handball" 41 3
    , EmojiData "handball" 41 9
    , EmojiData "diving mask" 41 15
    , EmojiData "wilted flower" 41 16
    , EmojiData "drum with drumsticks" 41 17
    , EmojiData "clinking glasses" 41 18
    , EmojiData "tumbler glass" 41 19
    , EmojiData "spoon" 41 20
    , EmojiData "goal net" 41 21
    , EmojiData "first place medal" 41 22
    , EmojiData "second place medal" 41 23
    , EmojiData "third place medal" 41 24
    , EmojiData "boxing glove" 41 25
    , EmojiData "martial arts uniform" 41 26
    , EmojiData "curling stone" 41 27
    , EmojiData "lacrosse" 41 28
    , EmojiData "softball" 41 29
    , EmojiData "flying disc" 41 30
    , EmojiData "croissant" 41 31
    , EmojiData "avocado" 41 32
    , EmojiData "cucumber" 41 33
    , EmojiData "bacon" 41 34
    , EmojiData "potato" 41 35
    , EmojiData "carrot" 41 36
    , EmojiData "baguette bread" 41 37
    , EmojiData "green salad" 41 38
    , EmojiData "shallow pan of food" 41 39
    , EmojiData "stuffed flatbread" 41 40
    , EmojiData "egg" 41 41
    , EmojiData "glass of milk" 41 42
    , EmojiData "peanuts" 41 43
    , EmojiData "kiwifruit" 41 44
    , EmojiData "pancakes" 41 45
    , EmojiData "dumpling" 41 46
    , EmojiData "fortune cookie" 41 47
    , EmojiData "takeout box" 41 48
    , EmojiData "chopsticks" 41 49
    , EmojiData "bowl with spoon" 41 50
    , EmojiData "cup with straw" 41 51
    , EmojiData "coconut" 41 52
    , EmojiData "broccoli" 41 53
    , EmojiData "pie" 41 54
    , EmojiData "pretzel" 41 55
    , EmojiData "cut of meat" 41 56
    , EmojiData "sandwich" 42 0
    , EmojiData "canned food" 42 1
    , EmojiData "leafy green" 42 2
    , EmojiData "mango" 42 3
    , EmojiData "moon cake" 42 4
    , EmojiData "bagel" 42 5
    , EmojiData "smiling face with 3 hearts" 42 6
    , EmojiData "yawning face" 42 7
    , EmojiData "partying face" 42 8
    , EmojiData "woozy face" 42 9
    , EmojiData "hot face" 42 10
    , EmojiData "cold face" 42 11
    , EmojiData "pleading face" 42 12
    , EmojiData "sari" 42 13
    , EmojiData "lab coat" 42 14
    , EmojiData "goggles" 42 15
    , EmojiData "hiking boot" 42 16
    , EmojiData "womans flat shoe" 42 17
    , EmojiData "crab" 42 18
    , EmojiData "lion face" 42 19
    , EmojiData "scorpion" 42 20
    , EmojiData "turkey" 42 21
    , EmojiData "unicorn face" 42 22
    , EmojiData "eagle" 42 23
    , EmojiData "duck" 42 24
    , EmojiData "bat" 42 25
    , EmojiData "shark" 42 26
    , EmojiData "owl" 42 27
    , EmojiData "fox face" 42 28
    , EmojiData "butterfly" 42 29
    , EmojiData "deer" 42 30
    , EmojiData "gorilla" 42 31
    , EmojiData "lizard" 42 32
    , EmojiData "rhinoceros" 42 33
    , EmojiData "shrimp" 42 34
    , EmojiData "squid" 42 35
    , EmojiData "giraffe face" 42 36
    , EmojiData "zebra face" 42 37
    , EmojiData "hedgehog" 42 38
    , EmojiData "sauropod" 42 39
    , EmojiData "t rex" 42 40
    , EmojiData "cricket" 42 41
    , EmojiData "kangaroo" 42 42
    , EmojiData "llama" 42 43
    , EmojiData "peacock" 42 44
    , EmojiData "hippopotamus" 42 45
    , EmojiData "parrot" 42 46
    , EmojiData "raccoon" 42 47
    , EmojiData "lobster" 42 48
    , EmojiData "mosquito" 42 49
    , EmojiData "microbe" 42 50
    , EmojiData "badger" 42 51
    , EmojiData "swan" 42 52
    , EmojiData "sloth" 42 53
    , EmojiData "otter" 42 54
    , EmojiData "orangutan" 42 55
    , EmojiData "skunk" 42 56
    , EmojiData "flamingo" 43 0
    , EmojiData "oyster" 43 1
    , EmojiData "guide dog" 43 2
    , EmojiData "probing cane" 43 3
    , EmojiData "bone" 43 4
    , EmojiData "leg" 43 5
    , EmojiData "foot" 43 11
    , EmojiData "tooth" 43 17
    , EmojiData "female superhero" 43 18
    , EmojiData "male superhero" 43 24
    , EmojiData "superhero" 43 30
    , EmojiData "female supervillain" 43 36
    , EmojiData "male supervillain" 43 42
    , EmojiData "supervillain" 43 48
    , EmojiData "safety vest" 43 54
    , EmojiData "ear with hearing aid" 43 55
    , EmojiData "motorized wheelchair" 44 4
    , EmojiData "manual wheelchair" 44 5
    , EmojiData "mechanical arm" 44 6
    , EmojiData "mechanical leg" 44 7
    , EmojiData "cheese wedge" 44 8
    , EmojiData "cupcake" 44 9
    , EmojiData "salt" 44 10
    , EmojiData "beverage box" 44 11
    , EmojiData "garlic" 44 12
    , EmojiData "onion" 44 13
    , EmojiData "falafel" 44 14
    , EmojiData "waffle" 44 15
    , EmojiData "butter" 44 16
    , EmojiData "mate drink" 44 17
    , EmojiData "ice cube" 44 18
    , EmojiData "woman standing" 44 19
    , EmojiData "man standing" 44 25
    , EmojiData "standing person" 44 31
    , EmojiData "woman kneeling" 44 37
    , EmojiData "man kneeling" 44 43
    , EmojiData "kneeling person" 44 49
    , EmojiData "deaf woman" 44 55
    , EmojiData "deaf man" 45 4
    , EmojiData "deaf person" 45 10
    , EmojiData "face with monocle" 45 16
    , EmojiData "farmer" 45 17
    , EmojiData "cook" 45 23
    , EmojiData "student" 45 29
    , EmojiData "singer" 45 35
    , EmojiData "artist" 45 41
    , EmojiData "teacher" 45 47
    , EmojiData "factory worker" 45 53
    , EmojiData "technologist" 46 2
    , EmojiData "office worker" 46 8
    , EmojiData "mechanic" 46 14
    , EmojiData "scientist" 46 20
    , EmojiData "astronaut" 46 26
    , EmojiData "firefighter" 46 32
    , EmojiData "people holding hands" 46 38
    , EmojiData "person with probing cane" 47 7
    , EmojiData "red haired person" 47 13
    , EmojiData "curly haired person" 47 19
    , EmojiData "bald person" 47 25
    , EmojiData "white haired person" 47 31
    , EmojiData "person in motorized wheelchair" 47 37
    , EmojiData "person in manual wheelchair" 47 43
    , EmojiData "health worker" 47 49
    , EmojiData "judge" 47 55
    , EmojiData "pilot" 48 4
    , EmojiData "adult" 48 10
    , EmojiData "child" 48 16
    , EmojiData "older adult" 48 22
    , EmojiData "bearded person" 48 28
    , EmojiData "person with headscarf" 48 34
    , EmojiData "woman in steamy room" 48 40
    , EmojiData "man in steamy room" 48 46
    , EmojiData "person in steamy room" 48 52
    , EmojiData "woman climbing" 49 1
    , EmojiData "man climbing" 49 7
    , EmojiData "person climbing" 49 13
    , EmojiData "woman in lotus position" 49 19
    , EmojiData "man in lotus position" 49 25
    , EmojiData "person in lotus position" 49 31
    , EmojiData "female mage" 49 37
    , EmojiData "male mage" 49 43
    , EmojiData "mage" 49 49
    , EmojiData "female fairy" 49 55
    , EmojiData "male fairy" 50 4
    , EmojiData "fairy" 50 10
    , EmojiData "female vampire" 50 16
    , EmojiData "male vampire" 50 22
    , EmojiData "vampire" 50 28
    , EmojiData "mermaid" 50 34
    , EmojiData "merman" 50 40
    , EmojiData "merperson" 50 46
    , EmojiData "female elf" 50 52
    , EmojiData "male elf" 51 1
    , EmojiData "elf" 51 7
    , EmojiData "female genie" 51 13
    , EmojiData "male genie" 51 14
    , EmojiData "genie" 51 15
    , EmojiData "female zombie" 51 16
    , EmojiData "male zombie" 51 17
    , EmojiData "zombie" 51 18
    , EmojiData "brain" 51 19
    , EmojiData "orange heart" 51 20
    , EmojiData "billed cap" 51 21
    , EmojiData "scarf" 51 22
    , EmojiData "gloves" 51 23
    , EmojiData "coat" 51 24
    , EmojiData "socks" 51 25
    , EmojiData "red envelope" 51 26
    , EmojiData "firecracker" 51 27
    , EmojiData "jigsaw" 51 28
    , EmojiData "test tube" 51 29
    , EmojiData "petri dish" 51 30
    , EmojiData "dna" 51 31
    , EmojiData "compass" 51 32
    , EmojiData "abacus" 51 33
    , EmojiData "fire extinguisher" 51 34
    , EmojiData "toolbox" 51 35
    , EmojiData "bricks" 51 36
    , EmojiData "magnet" 51 37
    , EmojiData "luggage" 51 38
    , EmojiData "lotion bottle" 51 39
    , EmojiData "thread" 51 40
    , EmojiData "yarn" 51 41
    , EmojiData "safety pin" 51 42
    , EmojiData "teddy bear" 51 43
    , EmojiData "broom" 51 44
    , EmojiData "basket" 51 45
    , EmojiData "roll of paper" 51 46
    , EmojiData "soap" 51 47
    , EmojiData "sponge" 51 48
    , EmojiData "receipt" 51 49
    , EmojiData "nazar amulet" 51 50
    , EmojiData "ballet shoes" 51 51
    , EmojiData "one piece swimsuit" 51 52
    , EmojiData "briefs" 51 53
    , EmojiData "shorts" 51 54
    , EmojiData "drop of blood" 51 55
    , EmojiData "adhesive bandage" 51 56
    , EmojiData "stethoscope" 52 0
    , EmojiData "yo yo" 52 1
    , EmojiData "kite" 52 2
    , EmojiData "parachute" 52 3
    , EmojiData "ringed planet" 52 4
    , EmojiData "chair" 52 5
    , EmojiData "razor" 52 6
    , EmojiData "axe" 52 7
    , EmojiData "diya lamp" 52 8
    , EmojiData "banjo" 52 9
    , EmojiData "bangbang" 52 10
    , EmojiData "interrobang" 52 11
    , EmojiData "tm" 52 12
    , EmojiData "information source" 52 13
    , EmojiData "left right arrow" 52 14
    , EmojiData "arrow up down" 52 15
    , EmojiData "arrow upper left" 52 16
    , EmojiData "arrow upper right" 52 17
    , EmojiData "arrow lower right" 52 18
    , EmojiData "arrow lower left" 52 19
    , EmojiData "leftwards arrow with hook" 52 20
    , EmojiData "arrow right hook" 52 21
    , EmojiData "watch" 52 22
    , EmojiData "hourglass" 52 23
    , EmojiData "keyboard" 52 24
    , EmojiData "eject" 52 25
    , EmojiData "fast forward" 52 26
    , EmojiData "rewind" 52 27
    , EmojiData "arrow double up" 52 28
    , EmojiData "arrow double down" 52 29
    , EmojiData "black right pointing double triangle with vertical bar" 52 30
    , EmojiData "black left pointing double triangle with vertical bar" 52 31
    , EmojiData "black right pointing triangle with double vertical bar" 52 32
    , EmojiData "alarm clock" 52 33
    , EmojiData "stopwatch" 52 34
    , EmojiData "timer clock" 52 35
    , EmojiData "hourglass flowing sand" 52 36
    , EmojiData "double vertical bar" 52 37
    , EmojiData "black square for stop" 52 38
    , EmojiData "black circle for record" 52 39
    , EmojiData "m" 52 40
    , EmojiData "black small square" 52 41
    , EmojiData "white small square" 52 42
    , EmojiData "arrow forward" 52 43
    , EmojiData "arrow backward" 52 44
    , EmojiData "white medium square" 52 45
    , EmojiData "black medium square" 52 46
    , EmojiData "white medium small square" 52 47
    , EmojiData "black medium small square" 52 48
    , EmojiData "sunny" 52 49
    , EmojiData "cloud" 52 50
    , EmojiData "umbrella" 52 51
    , EmojiData "snowman" 52 52
    , EmojiData "comet" 52 53
    , EmojiData "phone" 52 54
    , EmojiData "ballot box with check" 52 55
    , EmojiData "umbrella with rain drops" 52 56
    , EmojiData "coffee" 53 0
    , EmojiData "shamrock" 53 1
    , EmojiData "point up" 53 2
    , EmojiData "skull and crossbones" 53 8
    , EmojiData "radioactive sign" 53 9
    , EmojiData "biohazard sign" 53 10
    , EmojiData "orthodox cross" 53 11
    , EmojiData "star and crescent" 53 12
    , EmojiData "peace symbol" 53 13
    , EmojiData "yin yang" 53 14
    , EmojiData "wheel of dharma" 53 15
    , EmojiData "white frowning face" 53 16
    , EmojiData "relaxed" 53 17
    , EmojiData "female sign" 53 18
    , EmojiData "male sign" 53 19
    , EmojiData "aries" 53 20
    , EmojiData "taurus" 53 21
    , EmojiData "gemini" 53 22
    , EmojiData "cancer" 53 23
    , EmojiData "leo" 53 24
    , EmojiData "virgo" 53 25
    , EmojiData "libra" 53 26
    , EmojiData "scorpius" 53 27
    , EmojiData "sagittarius" 53 28
    , EmojiData "capricorn" 53 29
    , EmojiData "aquarius" 53 30
    , EmojiData "pisces" 53 31
    , EmojiData "chess pawn" 53 32
    , EmojiData "spades" 53 33
    , EmojiData "clubs" 53 34
    , EmojiData "hearts" 53 35
    , EmojiData "diamonds" 53 36
    , EmojiData "hotsprings" 53 37
    , EmojiData "recycle" 53 38
    , EmojiData "infinity" 53 39
    , EmojiData "wheelchair" 53 40
    , EmojiData "hammer and pick" 53 41
    , EmojiData "anchor" 53 42
    , EmojiData "crossed swords" 53 43
    , EmojiData "medical symbol" 53 44
    , EmojiData "scales" 53 45
    , EmojiData "alembic" 53 46
    , EmojiData "gear" 53 47
    , EmojiData "atom symbol" 53 48
    , EmojiData "fleur de lis" 53 49
    , EmojiData "warning" 53 50
    , EmojiData "zap" 53 51
    , EmojiData "white circle" 53 52
    , EmojiData "black circle" 53 53
    , EmojiData "coffin" 53 54
    , EmojiData "funeral urn" 53 55
    , EmojiData "soccer" 53 56
    , EmojiData "baseball" 54 0
    , EmojiData "snowman without snow" 54 1
    , EmojiData "partly sunny" 54 2
    , EmojiData "thunder cloud and rain" 54 3
    , EmojiData "ophiuchus" 54 4
    , EmojiData "pick" 54 5
    , EmojiData "helmet with white cross" 54 6
    , EmojiData "chains" 54 7
    , EmojiData "no entry" 54 8
    , EmojiData "shinto shrine" 54 9
    , EmojiData "church" 54 10
    , EmojiData "mountain" 54 11
    , EmojiData "umbrella on ground" 54 12
    , EmojiData "fountain" 54 13
    , EmojiData "golf" 54 14
    , EmojiData "ferry" 54 15
    , EmojiData "boat" 54 16
    , EmojiData "skier" 54 17
    , EmojiData "ice skate" 54 18
    , EmojiData "woman bouncing ball" 54 19
    , EmojiData "man bouncing ball" 54 25
    , EmojiData "person with ball" 54 31
    , EmojiData "tent" 54 37
    , EmojiData "fuelpump" 54 38
    , EmojiData "scissors" 54 39
    , EmojiData "white check mark" 54 40
    , EmojiData "airplane" 54 41
    , EmojiData "email" 54 42
    , EmojiData "fist" 54 43
    , EmojiData "hand" 54 49
    , EmojiData "v" 54 55
    , EmojiData "writing hand" 55 4
    , EmojiData "pencil2" 55 10
    , EmojiData "black nib" 55 11
    , EmojiData "heavy check mark" 55 12
    , EmojiData "heavy multiplication x" 55 13
    , EmojiData "latin cross" 55 14
    , EmojiData "star of david" 55 15
    , EmojiData "sparkles" 55 16
    , EmojiData "eight spoked asterisk" 55 17
    , EmojiData "eight pointed black star" 55 18
    , EmojiData "snowflake" 55 19
    , EmojiData "sparkle" 55 20
    , EmojiData "x" 55 21
    , EmojiData "negative squared cross mark" 55 22
    , EmojiData "question" 55 23
    , EmojiData "grey question" 55 24
    , EmojiData "grey exclamation" 55 25
    , EmojiData "exclamation" 55 26
    , EmojiData "heavy heart exclamation mark ornament" 55 27
    , EmojiData "heart" 55 28
    , EmojiData "heavy plus sign" 55 29
    , EmojiData "heavy minus sign" 55 30
    , EmojiData "heavy division sign" 55 31
    , EmojiData "arrow right" 55 32
    , EmojiData "curly loop" 55 33
    , EmojiData "loop" 55 34
    , EmojiData "arrow heading up" 55 35
    , EmojiData "arrow heading down" 55 36
    , EmojiData "arrow left" 55 37
    , EmojiData "arrow up" 55 38
    , EmojiData "arrow down" 55 39
    , EmojiData "black large square" 55 40
    , EmojiData "white large square" 55 41
    , EmojiData "star" 55 42
    , EmojiData "o" 55 43
    , EmojiData "wavy dash" 55 44
    , EmojiData "part alternation mark" 55 45
    , EmojiData "congratulations" 55 46
    , EmojiData "secret" 55 47
    ]
