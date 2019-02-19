# Mars Rover

Maintainer: Jan Schmitt

Id: lockejan

---

Implementation of the Mars Rover task.

## Installation

0. Leiningen needs to be installed on your system.
1. Just clone the Repository.
2. Navigate into the **"cruisen"** subdirectory.
3. Enter **"lein repl"** in your terminal and hit enter. ;)

## Usage

The API can be used via three functions in general:

1. **init!** - creates a board, positions obstacles and the rover on it and prints it to the console. It's possible to define a custom board size.
2. **hello-rover!** - is used to navigate the rover over the board.
3. **print-board!** - prints out the current board state to the terminal

Call docstrings for more informations via (doc foo)

## Examples

Using default values:
**(init!)**

Or custom ones:
**(init! 20 30)**

Navigate the rover:
**(hello-rover! "ffbbrrflf")**


## License

Copyright © 2019 Jan Schmitt

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
