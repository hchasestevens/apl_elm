import String
import Char
import Set
import List
import Dict
import Text
import Graphics.Input (Input, input)
import Graphics.Input.Field as Field

-- based on the example given at http://elm-lang.org/edit/examples/Reactive/TextField.elm (Accessed 4th November 2014):
searchBar : Input Field.Content
searchBar = input Field.noContent

main : Signal Element
main =
    lift scene searchBar.signal

scene : Field.Content -> Element
scene fieldContent = let sortedAttractions = tokens fieldContent.string |> attractionsByRelevance
                         result attraction = [(toText >> bold) attraction.title, toText attraction.body, toText "\n"] 
                                             |> map leftAligned 
                                             |> flow down
  in 
    flow down
    [ Field.field Field.defaultStyle searchBar.handle identity "Enter search" fieldContent
    , plainText "\n"
    , map result sortedAttractions |> flow down
    ]

type Attraction = { title: String, body: String }

-- taken from http://www.touropia.com/tourist-attractions-in-scotland/ (Accessed 4th November 2014):
attractions : [Attraction]
attractions = 
  [
    {title = "Broch of Mousa",
     body = "One of the most prestigious and well-preserved brochs in the 
Shetland Islands, this impressive structure is a rotund tower lined with 
stone internally and externally to provide the optimum strength as a 
defensive structure. The tower was built around 100 BC and is the only 
broch which is complete right to the top, including the original 
intramural stair."
     }
  , {title = "Melrose Abbey",
     body = "Melrose Abbey was founded in 1136 by Cistercian monks, on the 
request of King David I of Scotland. This grand ruin with lavish masonic 
decoration is thought to hold the embalmed heart of Robert the Bruce, 
another king of Scotland. Truly a place of legends, Melrose Abbey is one 
of the most historically significant architectural structures in Scotland."
     }
  , {title = "Cuillin Hills",
     body = "Located on the most northern island of Skye, the beauty of the 
rolling peaks of the Cuiillin Hills is undeniable. These hills are made up 
of two diverse formations. The Red Cuiillins are a red granite formation, 
which are softer and more inviting in appearance. In opposition, the Black 
Cuillins are more harsh in appearance with sharp, jagged peaks of volcanic 
rock that scale the skyline and warn off those who are unwelcome."
     }
  , {title = "Skara Brae",
     body = "Located on the main island of Orkney, Skara Brae is one of the 
best preserved Stone Age villages in Europe. It was covered for hundreds 
of years by a sand dune until a great storm exposed the site in 1850. The 
stone walls are relatively well preserved because the dwellings were 
filled by sand almost immediately after the site was abandoned. Older than
Stonehenge and the Great Pyramids, it has been called the \"Scottish 
Pompeii\" because of its excellent preservation."
     }
  , {title = "Stirling Castle",
     body = "Stirling Castle is one of the most spectacular castles in all of 
Scotland. High up on the vertical rock mass of Castle Hill, it rests, 
defensively positioned along the steep surrounding cliffs. Experience the 
art, culture and status that encompassed 16th century in Scotland. History 
lovers will not want to miss this popular tourist attraction."
     }
  , {title = "Luskentyre Beach",
     body = "Luskentyre beach is situated on the spectacular west coast of 
South Harris in the Outer Hebrides. One of the most beautiful 
color-washed coastal areas of Scotland, its blue-green seas shimmer 
against creamy sands and the vibrant green hillside. Peaceful and 
timeless, Luskentyre Beach has been voted Britain's best beach."
     }
  , {title = "Loch Ness",
     body = "One of the most famous lakes in the world, Loch Ness is the second 
largest loch in Scotland after Loch Lomond (and due to its great depth it 
is the largest by volume). About a mile wide at most places it holds the 
legend of an infamous sea monster. The most notorious mythical creature of 
modern time, Nessie, is said to dwell in the lake. With an air of mystery, 
the intriguing area of Loch Ness should not be missed. You might even get 
a glimpse of Nessie!"
     }
  , {title = "Ben Nevis",
     body = "Situated deep into the highlands of Scotland, Ben Nevis is the 
British Isle's highest summit. Offering stunningly spectacular views and 
historical malice, Ben Nevis attracts viewers, hikers and climbers alike 
to celebrate the tranquility of the surrounding nature. The mountain is 
readily accessible via a man-made path which zig zags up its south 
westerly face, while the rock face on the north west of the mountain is 
strictly for experienced mountaineers only."
     }
  , {title = "Eilean Donan",
     body = "Eilean Donan is a small island in Loch Duich in the western 
Highlands of Scotland. Connected to the mainland by a footbridge, the 
island is dominated by a picturesque medieval castle. The original castle 
was built in the early 13th century as a defense against the Vikings. 
Today, the castle is one of the most photographed monuments in Scotland 
and a popular venue for weddings and film locations. It has appeared in 
such films as Highlander and The World Is Not Enough."
     }
  , {title = "Edinburgh Castle",
     body = "Edinburgh Castle is a magnificent example of Scotland's 
architecture, ideology, political tact and military importance. High up on 
the summit of a dormant volcano lurks this dominating structure. Its 
presence is visible for miles in every direction. Intimidating all who 
would challenge them, the Scottish utilized Edinburgh Castle for all of 
their major battles and military strategizing. A strong standing symbol 
of their perseverance and struggle for independence, Edinburgh Castle is 
one of the top tourist attractions in Scotland."
     }
  ]
  
alphanumericCharacters = Set.fromList (String.toList "abcdefghijklmnopqrstuvwxyz0123456789")
  
startingLetters : String -> String
startingLetters string = String.toList string  
                         |> List.partition (flip Set.member alphanumericCharacters) 
                         |> fst >> String.fromList
  
tokens : String -> [String]
tokens string = String.words string 
                |> map String.toLower 
                |> map startingLetters

unique : [String] -> [String]
unique xs = Set.fromList xs |> Set.toList 

counts : [String] -> Dict.Dict String Int
counts tokens = let uniqueTokens = unique tokens
                    count item list = List.filter ((==) item) list |> List.length
                    tokenCount = flip count tokens
  in
    map tokenCount uniqueTokens 
    |> zip uniqueTokens 
    |> Dict.fromList
    
idf : String -> Float
idf token = let documents = map (.body >> tokens >> Set.fromList) attractions
                tokenFrequency = filter (Set.member token) documents |> List.length
  in 
     toFloat (List.length attractions) / toFloat tokenFrequency |> logBase 2
    
-- tf-idf
relevance : [String] -> [String] -> Float
relevance query document = let mutualTokens = (Set.fromList query, Set.fromList document) 
                                              |> uncurry Set.intersect 
                                              |> Set.toList
                               queryCounts = counts query
                               queryCount token = Dict.getOrElse 0 token queryCounts |> toFloat
                               documentCount token = Dict.getOrElse 0 token documentCounts |> toFloat
                               documentCounts = counts document
                               k = 2
                               documents = map (.body >> tokens) attractions
                               averageK = (sum >> toFloat) (map length documents) / (length >> toFloat) documents
                               documentSquash token = documentCount token + (averageK * toFloat (length document))
                               tfidf token = queryCount token * documentCount token / documentSquash token * idf token
  in
    map tfidf mutualTokens |> sum
    
attractionsByRelevance : [String] -> [Attraction]
attractionsByRelevance query = map (.body >> tokens >> relevance query) attractions
                               |> flip zip attractions
                               |> filter (fst >> (<) 0)
                               |> List.sortBy fst
                               |> List.reverse 
                               |> map snd

