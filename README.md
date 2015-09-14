# GOLD level calculator

A command line programme to determine the GOLD level that users should be enrolled on.

## Technology

* [Stack](https://github.com/commercialhaskell/stack) is required to build the programme
* It is currently built against [LTS Haskell 3.5](https://www.stackage.org/lts-3.5)
* [In-depth guide](https://github.com/commercialhaskell/stack/blob/master/GUIDE.md) to Stack

## Build

    stack build --pedantic

## Test

    stack test --pedantic

## Run one calculation

    build/calc-one --help

### Example

    $ build/calc-one --file "data/GOLD levels.csv" --ielts 5.5 --listening 50 --reading 55 --writing B2+ --speaking B1
    L2

## Run many calculations

    build/calc-many --help

### Examples

#### Success

    $ build/calc-many --file "data/GOLD levels.csv" --users "data/GOLD users good.csv"
    Output file "GOLD users good_output.csv" has been written to the current working directory

    $ more GOLD\ users\ good_output.csv
    1231231230,McGowan,Mike,Brighton,N,5.5,50,60,B1,B2+,GM1L2
    4564564560,"van Tienhoven",Sacha,Brighton,Y,4.5,40,70,A1+,C2,X
    7897897890,Nockles,Joe,Brighton,N,6.5,80,80,C1,C2,"No GOLD"
    ,,,,N,4.5,35,35,A2,A2+,GM1L1
    ,,,,N,6.5,70,65,B1+,B2,GM1L3
    ,,,,Y,6.5,70,65,B1+,B2,GM2L3
    ,,,,N,6.5,100,70,B2,A1,Alert

#### Failure

    $ build/calc-many --file "data/GOLD levels.csv" --users "data/GOLD users bad.csv"
    Row 2 has error ""5" is not one of [4.5, 5.0, 5.5, 6.0, 6.5]"
    Row 4 has error ""101" is not an integer in the range [0..100] inclusive"
    Row 5 has error ""C2+" is not one of [A1, A1+, A2, A2+, B1, B1+, B2, B2+, C1, C1+, C2]"
    Row 7 has error ""32.5" is not an integer in the range [0..100] inclusive"
    Row 8 has error ""Yes" is not one of ['Y', 'N']"
    Something went wrong trying to load or parse the CSV users file
