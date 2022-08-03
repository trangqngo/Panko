module ColorConverter

open System
open Parser

let singleHexToDec (str: string) : int = 
    //TODO: convert str's letters to all uppercase 
    match str with 
    | "A" -> 10
    | "B" -> 11
    | "C" -> 12
    | "D" -> 13
    | "E" -> 14
    | "F" -> 15 
    | "1" -> 1
    | "2" -> 2
    | "3" -> 3
    | "4" -> 4
    | "5" -> 5
    | "6" -> 6
    | "7" -> 7
    | "8" -> 8
    | "9" -> 9
    | "0" -> 0
    | _ -> failwith ("Invalid hex digit.")

// hex only has 2 chars 
let hexToDec (str: string) : int = 
    let hex1 = singleHexToDec (string str.[0])
    let hex2 = singleHexToDec (string str.[1])
    hex1 * 16 + hex2

let singleDecToHex (n: int) : string  = 
    match n with 
    | 10 -> "A"
    | 11 -> "B"
    | 12 -> "C"
    | 13 -> "D"
    | 14 -> "E"
    | 15 -> "F" 
    | 1 -> "1"
    | 2 -> "2"
    | 3 -> "3"
    | 4 -> "4"
    | 5 -> "5"
    | 6 -> "6"
    | 7 -> "7"
    | 8 -> "8"
    | 9 -> "9"
    | 0 -> "0"
    | _ -> failwith ("Invalid hex digit.")

// n is max 255 (corresponding to FF)
let decToHex (n: int) : string = 
    let a = singleDecToHex (n % 16)
    let b = singleDecToHex ((n / 16) % 16)
    b + a
    
// give a tuple (r,g,b), return a hex string of a color 
let RGBToHex (col: int*int*int) : string = 
    let r,g,b = col
    let hex1 = decToHex r
    let hex2 = decToHex g
    let hex3 = decToHex b
    hex1 + hex2 + hex3

// given a hex color, return a tuple of (r,g,b) values
let hexToRGB (col: string) : int*int*int= 
    let hex1 = stringify [col.[0]; col.[1]]
    let hex2 = stringify [col.[2]; col.[3]]
    let hex3 = stringify [col.[4]; col.[5]]
    (hexToDec hex1), (hexToDec hex2), (hexToDec hex3)