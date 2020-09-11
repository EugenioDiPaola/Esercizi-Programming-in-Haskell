#include <iostream>
#include <vector>
#include <map>

using namespace std;

bool isCharLowercase (char CHAR){
	
	vector<char> lowercase_english_alphabet = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u',
						   'v', 'w', 'x', 'y', 'z'}; 
	bool result_bool = false;
	for(auto & x : lowercase_english_alphabet){if(x == CHAR) result_bool = true;}
	
	return result_bool;	
}


bool isCharUppercase (char CHAR){
	
	vector<char> uppercase_english_alphabet = {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 
						   'V', 'W', 'X', 'Y', 'Z'}; 
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
    if(isCharUppercase(CHAR) && convertCharToInt(CHAR) - (D % 26) < 26){ cout << "entered" << endl; result_char = convertIntToChar (52 - (26 - abs (convertCharToInt(CHAR) - (D % 26))));}
    
    return result_char;
}

string & caesarDecypherString (string & STRING, int D){
    for(auto & s: STRING){s = caesarDecypherChar(s, D); cout << s << endl;}
    return STRING;
}

int main(){
        
    int d = 21;//20
    string test_string ("NelMezzoDelCamminDiNostraVita");
    cout << "caesarCypherString('NelMezzoDelCamminDiNostraVita', d)" << endl;
    string s2 = caesarCypherString(test_string, d);
    cout << s2 << endl;
    
    string s3 = caesarDecypherString(s2, d);
    cout << s3 << endl;    
}
