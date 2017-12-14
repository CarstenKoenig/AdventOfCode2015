module Solution (lineStats, codeLength, contentLength, content, readInput, part1, part2)  where


type Input = [Line]

type Line = String


part1 :: Input -> Int
part1 = sum . map lineStats


part2 :: Input -> Int
part2 = sum . map encodeStats


lineStats :: Line -> Int
lineStats l = codeLength l - contentLength l


encodeStats :: Line -> Int
encodeStats l = encodedLength l - codeLength l


codeLength :: Line -> Int
codeLength = length


contentLength :: Line -> Int
contentLength = length . content


encodedLength :: Line -> Int
encodedLength = length . encode


encode :: Line -> String
encode = show


content :: Line -> String
content [] = []
content ['"'] = []
content ('"':rest) = content rest
content ('\\':'"':rest) = '"' : content rest
content ('\\':'x':_:_:rest) = '#' : content rest
content ('\\':c:rest) = c : content rest
content (c:rest) = c : content rest


readInput :: IO Input
readInput = lines <$> readFile "input.txt"
