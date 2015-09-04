# GOLD level calculator

A command line programme to determine the GOLD level that users should be enrolled on.

## Technology

* [Stack](https://github.com/commercialhaskell/stack) is required to build the programme
* It is currently built against [LTS Haskell 3.4](https://www.stackage.org/lts-3.4)
* [In-depth guide](https://github.com/commercialhaskell/stack/blob/master/GUIDE.md) to Stack

### Build

    stack build --pedantic

### Test

    stack test --pedantic

### Run one calculation

    build/calc-one --help

#### Example

    $ build/calc-one --file "data/GOLD levels.csv" --ielts 5.5 --listening 50 --reading 55 --writing B2+ --speaking B1
    L2

### Run many calculations

    build/calc-many --help

#### Example

    $ build/calc-many --file "data/GOLD levels.csv" --users "data/GOLD users.csv"
    CSVInput {studentID = "1231231230", lastName = "McGowan", firstName = "Mike", centre = "Brighton", prev = N, params = GOLDCalcParams 5.5 50 60 B1 B2+} ==> GM1L2
    CSVInput {studentID = "4564564560", lastName = "van Tienhoven", firstName = "Sacha", centre = "Brighton", prev = Y, params = GOLDCalcParams 4.5 40 70 A1+ C2} ==> X
    CSVInput {studentID = "7897897890", lastName = "Nockles", firstName = "Joe", centre = "Brighton", prev = N, params = GOLDCalcParams 6.5 80 80 C1 C2} ==> No GOLD
