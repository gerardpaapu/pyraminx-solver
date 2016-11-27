module LazyList exposing (..)


type Cell a
    = Nil
    | Cons a (LazyList a)


type LazyList a
    = LazyList (() -> Cell a)


empty : LazyList a
empty =
    LazyList (\() -> Nil)


singleton : a -> LazyList a
singleton v =
    LazyList (\() -> Cons v empty)


map : (a -> b) -> LazyList a -> LazyList b
map f (LazyList ls) =
    LazyList
        (\() ->
            case ls () of
                Nil ->
                    Nil

                Cons x xs ->
                    Cons (f x) (map f xs)
        )


force : LazyList a -> Cell a
force (LazyList ls) =
    ls ()


ofList : List a -> LazyList a
ofList ls =
    LazyList
        (\() ->
            case ls of
                [] ->
                    Nil

                x :: xs ->
                    Cons x (ofList xs)
        )


find : LazyList (Maybe a) -> Maybe a
find (LazyList ls) =
    case ls () of
        Nil ->
            Nothing

        Cons (Just x) _ ->
            Just x

        Cons _ xs ->
            find xs


append : LazyList a -> LazyList a -> LazyList a
append a b =
    LazyList (\() -> append_ a b)


append_ : LazyList a -> LazyList a -> Cell a
append_ (LazyList a) (LazyList b) =
    case a () of
        Nil ->
            b ()

        Cons x xs ->
            Cons x (append xs (LazyList b))


join : LazyList (LazyList a) -> LazyList a
join (LazyList ls) =
    LazyList
        (\() ->
            case ls () of
                Nil ->
                    Nil

                Cons x xs ->
                    append_ x (join xs)
        )


flatMap : (a -> LazyList b) -> LazyList a -> LazyList b
flatMap f =
    map f >> join


foldl : (b -> a -> b) -> b -> LazyList a -> b
foldl f acc (LazyList ls) =
    case ls () of
        Nil ->
            acc

        Cons x xs ->
            foldl f (f acc x) xs


cons : a -> LazyList a -> LazyList a
cons a ls =
    LazyList (\() -> Cons a ls)
