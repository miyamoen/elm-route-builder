module RouteBuilder exposing
    ( Route
    , parseUrl, onChild
    , RouteBuilder, root, s, string, int
    , static, dynamic1, dynamic2, dynamic3, dynamic4, dynamic5, dynamic
    , custom, customWithParser
    )

{-|

@docs Route
@docs parseUrl, onChild


## Builder

@docs RouteBuilder, root, s, string, int
@docs static, dynamic1, dynamic2, dynamic3, dynamic4, dynamic5, dynamic
@docs custom, customWithParser

-}

import Url exposing (Url)
import Url.Builder
import Url.Parser as Parser exposing ((</>), Parser)


type alias Route params page =
    { toString : params -> String
    , toPaths : params -> List String
    , toParser : (params -> page) -> Parser (page -> page) page
    }


type alias RouteBuilder params a b =
    { parser : Parser a b
    , toPaths : List (params -> String)
    }



-- builder


static : RouteBuilder () () () -> Route () page
static b =
    let
        toPaths () =
            List.map (\getter -> getter ()) b.toPaths
                |> List.reverse
    in
    { toString = \() -> Url.Builder.absolute (toPaths ()) []
    , toPaths = toPaths
    , toParser = \toPage -> Parser.map () b.parser |> Parser.map toPage
    }


dynamic : toParams -> RouteBuilder params toParams params -> Route params page
dynamic toParams b =
    let
        toPaths r =
            List.map (\getter -> getter r) b.toPaths
                |> List.reverse
    in
    { toString = \r -> Url.Builder.absolute (toPaths r) []
    , toPaths = toPaths
    , toParser = \toPage -> Parser.map toParams b.parser |> Parser.map toPage
    }


dynamic1 : (p1 -> params) -> RouteBuilder params (p1 -> params) params -> Route params page
dynamic1 =
    dynamic


dynamic2 : (p1 -> p2 -> params) -> RouteBuilder params (p1 -> p2 -> params) params -> Route params page
dynamic2 =
    dynamic


dynamic3 : (p1 -> p2 -> p3 -> params) -> RouteBuilder params (p1 -> p2 -> p3 -> params) params -> Route params page
dynamic3 =
    dynamic


dynamic4 : (p1 -> p2 -> p3 -> p4 -> params) -> RouteBuilder params (p1 -> p2 -> p3 -> p4 -> params) params -> Route params page
dynamic4 =
    dynamic


dynamic5 : (p1 -> p2 -> p3 -> p4 -> p5 -> params) -> RouteBuilder params (p1 -> p2 -> p3 -> p4 -> p5 -> params) params -> Route params page
dynamic5 =
    dynamic


root : RouteBuilder params a a
root =
    { parser = Parser.top, toPaths = [] }


s : String -> RouteBuilder params a b -> RouteBuilder params a b
s str b =
    { parser = b.parser </> Parser.s str, toPaths = always str :: b.toPaths }


string : (params -> String) -> RouteBuilder params a (String -> b) -> RouteBuilder params a b
string =
    customWithParser Parser.string identity


int : (params -> Int) -> RouteBuilder params a (Int -> b) -> RouteBuilder params a b
int =
    customWithParser Parser.int String.fromInt


custom : (String -> Maybe v) -> (v -> String) -> (params -> v) -> RouteBuilder params a (v -> b) -> RouteBuilder params a b
custom decoder encoder getter b =
    customWithParser (Parser.custom "CUSTOM" decoder) encoder getter b


customWithParser : Parser (v -> b) b -> (v -> String) -> (params -> v) -> RouteBuilder params a (v -> b) -> RouteBuilder params a b
customWithParser customParser encoder getter b =
    { parser = b.parser </> customParser
    , toPaths = (getter >> encoder) :: b.toPaths
    }



-- advanced


onChild :
    Route params ( model, Cmd msg )
    -> ( childModel -> model, childMsg -> msg )
    -> (params -> ( childModel, Cmd childMsg ))
    -> Parser (( model, Cmd msg ) -> ( model, Cmd msg )) ( model, Cmd msg )
onChild { toParser } ( modelTagger, msgTagger ) pageInit =
    toParser <| \params -> Tuple.mapBoth modelTagger (Cmd.map msgTagger) <| pageInit params


parseUrl : Url -> page -> List (Parser (page -> page) page) -> page
parseUrl url default routes =
    Parser.parse (Parser.oneOf routes) url |> Maybe.withDefault default
