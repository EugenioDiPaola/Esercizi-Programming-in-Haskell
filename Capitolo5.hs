-- 1. Using a list comprehension, give an expression that calculates the sum
-- 1^2 + 2^2 + . . . 100^2 of the first one hundred integer squares.

sum [x * x | x <- [1..100]]


-- 2. In a similar way to the function length, show how the library function
-- replicate :: Int -> a -> [a] that produces a list of identical elements can
-- be defined using a list comprehension. For example:
-- > replicate 3 True
-- [True, True, True]

replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]


-- 3. A triple (x, y, z) of positive integers can be termed pythagorean if x^2 +
-- y^2 = z^2. Using a list comprehension, define a function pyths :: Int ->
-- [(Int, Int, Int)] that returns the list of all pythagorean triples whose
-- components are at most a given limit. For example:
-- > pyths 10
-- [(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]


-- 4. A positive integer is perfect if it equals the sum of its factors, excluding
-- the number itself. Using a list comprehension and the function factors,
-- define a function perfects :: Int -> [Int] that returns the list of all perfect
-- numbers up to a given limit. For example:
-- > perfects 500
-- [6, 28, 496]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], (mod) n x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (sum (factors x) - x)  == x]


-- 5. Show how the single comprehension [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]
-- with two generators can be re-expressed using two comprehensions with
-- single generators. Hint: make use of the library function concat and nest
-- one comprehension within the other.


-- 6. Redefine the function positions using the function find.

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
                 where n = length xs − 1

-- 7. The scalar product of two lists of integers xs and ys of length n is given
-- by the sum of the products of corresponding integers:
-- \sum_(i = 0)^(n - 1)(xs_i * xs_i)
-- In a similar manner to the function chisqr, show how a list comprehension 
-- can be used to define a function scalarproduct :: [Int] -> [Int] -> Int
-- that returns the scalar product of two lists. For example:
-- > scalarproduct [1, 2, 3] [4, 5, 6]
-- 32

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]

-- 8. Modify the Caesar cipher program to also handle upper-case letters.

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

-- versione in C++ esercizio 8

#include <iostream>
#include <vector>
#include <map>

using namespace std;

bool isCharLowercase (char CHAR){
	
	vector<char> lowercase_english_alphabet = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'}; 
	bool result_bool = false;
	for(auto & x : lowercase_english_alphabet){if(x == CHAR) result_bool = true;}
	
	return result_bool;	
}


bool isCharUppercase (char CHAR){
	
	vector<char> uppercase_english_alphabet = {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'}; 
	bool result_bool = false;
	for(auto & x : uppercase_english_alphabet){if(x == CHAR) result_bool = true;}
	
	return result_bool;	
}


int convertCharToInt (char CHAR){
    
    int result_int = 0;
    map<char, int> numbered_alphabet_char_int_map = {{'a', 0}, {'b', 1}, {'c', 2}, {'d', 3}, {'e', 4}, {'f', 5}, {'g', 6}, {'h', 7}, {'i', 8},
                                                    {'j', 9}, {'k', 10}, {'l', 11}, {'m', 12}, {'n', 13}, {'o', 14}, {'p', 15}, {'q', 16}, {'r', 17}, {'s', 18}, {'t', 19}, {'u', 20}, {'v', 21}, {'w', 22}, {'x', 23}, {'y', 24}, {'z', 25}, 
                                                    {'A', 26},{'B', 27}, {'C', 28}, {'D', 29}, {'E', 30}, {'F', 31}, {'G', 32}, {'H', 33}, {'I', 34}, {'J', 35}, {'K', 36}, { 'L', 37}, {'M', 38}, 
                                                    {'N', 39}, {'O', 40}, {'P', 41}, {'Q', 42}, {'R', 43}, {'S', 44}, {'T', 45}, {'U', 46}, {'V', 47}, {'W', 48}, {'X', 49}, {'Y', 50}, {'Z', 51}};
    result_int = numbered_alphabet_char_int_map[CHAR];
    return result_int;
}


char convertIntToChar (int INT){
    
    char result_char;
    map<char, int> numbered_alphabet_int_char_map = {{0, 'a'},{1, 'b'}, {2, 'c'}, {3, 'd'}, {4, 'e'}, {5, 'f'}, {6, 'g'}, {7, 'h'}, {8, 'i'}, {9, 'j'}, {10, 'k'}, {11, 'l'}, {12, 'm'}, {13, 'n'}, {14, 'o'}, {15, 'p'},
                                                    {16, 'q'}, {17, 'r'}, {18, 's'}, {19, 't'}, {20, 'u'}, {21, 'v'}, {22, 'w'}, {23, 'x'}, {24, 'y'}, {25, 'z'}, {26, 'A'},{27, 'B'}, {28, 'C'}, {29, 'D'}, {30, 'E'}, {31, 'F'}, {32, 'G'}, {33, 'H'}, {34, 'I'}, {35, 'J'}, {36, 'K'}, {37, 'L'}, {38, 'M'}, {39, 'N'}, {40, 'O'}, {41, 'P'},
                                                    {42, 'Q'}, {43, 'R'}, {44, 'S'}, {45, 'T'}, {46, 'U'}, {47, 'V'}, {48, 'W'}, {49, 'X'}, {50, 'Y'}, {51, 'Z'}};
    result_char = numbered_alphabet_int_char_map[INT];
    return result_char;
}


char caesarCypherChar (char CHAR, int D){

    char result_char;     
    if(isCharLowercase(CHAR)) result_char = convertIntToChar((convertCharToInt(CHAR) + D % 26) % 26);
    if(isCharUppercase(CHAR) && (convertCharToInt(CHAR) + (D % 26) <= 51)) result_char = convertIntToChar (convertCharToInt(CHAR) + (D % 26));
    if(isCharUppercase(CHAR) && (convertCharToInt(CHAR) + (D % 26) > 51)) result_char = convertIntToChar (26 + ((convertCharToInt(CHAR) + (D % 26)) % 52));
    
    return result_char;
}


string & caesarCypherString (string & STRING, int D){
    for(auto & s: STRING){s = caesarCypherChar(s, D); cout << s << endl;}
    return STRING;
}


char caesarDecypherChar (char CHAR, int D){
    char result_char;
    if(isCharLowercase(CHAR) && convertCharToInt(CHAR) - (D % 26) >= 0){result_char = convertIntToChar(convertCharToInt(CHAR) - (D % 26));}
    if(isCharLowercase(CHAR) && convertCharToInt(CHAR) - (D % 26) < 0){result_char = convertIntToChar(26 - (abs (convertCharToInt(CHAR) - (D % 26))));}
    if(isCharUppercase(CHAR) && convertCharToInt(CHAR) - (D % 26) >= 26){result_char = convertIntToChar(convertCharToInt(CHAR) - (D % 26));}
    if(isCharUppercase(CHAR) && convertCharToInt(CHAR) - (D % 26) < 26){ cout << "entered"<< endl; result_char = convertIntToChar (52 - (26 - abs (convertCharToInt(CHAR) - (D % 26))));}
    
    return result_char;
}

string & caesarDecypherString (string & STRING, int D){
    for(auto & s: STRING){s = caesarDecypherChar(s, D); cout << s << endl;}
    return STRING;
}

int main(){
        
    int d = 21;//20
    string nmdcdnv_string ("NelMezzoDelCamminDiNostraVita");
    cout << "caesarCypherString('NelMezzoDelCamminDiNostraVita', d)" << endl;
    string s2 = caesarCypherString(nmdcdnv_string, d);
    cout << s2 << endl;
    
    string s3 = caesarDecypherString(s2, d);
    cout << s3 << endl;    
}


