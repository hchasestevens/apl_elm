import Mouse

header = plainText "This is a test"

main : Signal Element
main = 
    combine [
        constant header, 
        lift asText Mouse.position
    ] |> lift (flow down)
    

