import Mouse

header = plainText "This is a test"

main : Signal Element
main = merges [(lift asText Mouse.position), constant header]