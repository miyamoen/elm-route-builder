module RouteBuilderTest exposing (suite)

import Expect exposing (..)
import Fuzz
import RouteBuilder exposing (..)
import Test exposing (..)
import Url exposing (Url)
import Url.Builder exposing (crossOrigin)
import Url.Parser exposing (parse)


suite : Test
suite =
    concat
        [ describe "toString"
            [ fuzz (Fuzz.list Fuzz.string) "fuzz paths" <|
                \paths ->
                    List.foldl (\path rb -> rb |> s path) root paths
                        |> static
                        |> (\r -> r.toPaths ())
                        |> equal paths
            , test "top" <| \() -> topRoute.toString () |> equal "/"
            , test "testRoute1" <| \() -> testRoute.toString (TestRecord "str1" 5 "str2") |> equal "/start/str1/5/str2/end"
            , test "testRoute2" <| \() -> testRoute.toString (TestRecord "ham" 400 "spam") |> equal "/start/ham/400/spam/end"
            ]
        , describe "parse"
            [ test "top" <| parseTest topRoute [] ()
            , test "testRoute1" <| parseTest testRoute [ "start", "str1", "5", "str2", "end" ] (TestRecord "str1" 5 "str2")
            , test "testRoute2" <| parseTest testRoute [ "start", "ham", "400", "spam", "end" ] (TestRecord "ham" 400 "spam")
            ]
        ]


topRoute : Route () page
topRoute =
    root |> static


type alias TestRecord =
    { str1 : String, int : Int, str2 : String }


testRoute : Route TestRecord page
testRoute =
    root
        |> s "start"
        |> string .str1
        |> int .int
        |> string .str2
        |> s "end"
        |> dynamic TestRecord


toUrl : List String -> Maybe Url
toUrl paths =
    crossOrigin "https://example.com" paths [] |> Url.fromString


parseTest : Route a a -> List String -> a -> () -> Expectation
parseTest r paths expect () =
    Maybe.andThen (parse (r.toParser identity)) (toUrl paths) |> equal (Just expect)


type alias BookIds =
    { libraryId : String, bookId : String }


bookRoute : Route BookIds page
bookRoute =
    root |> s "libraries" |> string .libraryId |> s "books" |> string .bookId |> dynamic BookIds


bookUrl : String
bookUrl =
    bookRoute.toString { libraryId = "図書館", bookId = "本" }


bookParser : Url.Parser.Parser (BookPageModel -> BookPageModel) BookPageModel
bookParser =
    bookRoute.toParser bookPageInit


type alias BookPageModel =
    { ids : BookIds }


bookPageInit : BookIds -> BookPageModel
bookPageInit ids =
    { ids = ids }
