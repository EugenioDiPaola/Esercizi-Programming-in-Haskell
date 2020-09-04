import Data.Char

convertCharToInt :: Char -> Int
convertCharToInt c | isLower c = [ i | (i, t) <- zip [0..25] ['a'..'z'], t == c] !! 0
                   | isUpper c = [ i | (i, t) <- zip [26..51] ['A'..'Z'], t == c] !! 0

convertCharToInt :: Char -> Int
convertCharToInt c | isLower c = length['a'..c] - 1
                   | isUpper c = 26 + length['A'..c] - 1

convertIntToChar :: Int -> Char
convertIntToChar i | i >= 0 && i <= 25 = [ c | (c, t) <- zip ['a'..'z'] [0..25], t == i] !! 0
                   | i >= 26 && i <= 51 = [ c | (c, t) <- zip ['A'..'Z'] [26..51], t == i] !! 0

convertIntToChar :: Int -> Char
convertIntToChar i =  (['a'..'z'] ++ ['A'..'Z']) !! i

caesarCypherChar :: Char -> Int -> Char
caesarCypherChar c d | isLower c = convertIntToChar (mod (convertCharToInt c + mod d 26) 26)
                     | isUpper c && (convertCharToInt c + mod d 26) <= 51 = convertIntToChar (convertCharToInt c + mod d 26)
                     | isUpper c && (convertCharToInt c + mod d 26) > 51 = convertIntToChar (26 + mod (convertCharToInt c + mod d 26) 52)

caesarCypherString :: String -> Int -> String
caesarCypherString s d = [caesarCypherChar x d | x <- s]

caesarDecypherChar :: Char -> Int -> Char
caesarDecypherChar c d | isLower c && (convertCharToInt c - mod d 26) >= 0 = convertIntToChar (convertCharToInt c - mod d 26)
                       | isLower c && (convertCharToInt c - mod d 26) < 0 = convertIntToChar (26 - (abs (convertCharToInt c - mod d 26)))
                       | isUpper c && (convertCharToInt c - mod d 26) >= 26 = convertIntToChar (convertCharToInt c - mod d 26)
                       | isUpper c && (convertCharToInt c - mod d 26) < 26 = convertIntToChar (52 - (26 - abs (convertCharToInt c - mod d 26)))

caesarDecypherString :: String -> Int -> String
caesarDecypherString s d = [caesarDecypherChar x d | x <- s]
