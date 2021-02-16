elm-route-builder let us define both URL builder and parser at once.

I have no MP to write document. ﾜﾀｼ ﾆﾎﾝｼﾞﾝ. ｴｲｺﾞ ﾆｶﾞﾃ


```elm
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
```
