module Utils.Crash exposing (crash)


crash : String -> a
crash =
    Debug.todo
