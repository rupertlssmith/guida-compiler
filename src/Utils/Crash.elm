module Utils.Crash exposing (crash, todo)


crash : String -> a
crash =
    todo


todo : String -> a
todo =
    Debug.todo
