# Mars Rover

Maintainer: Jan Schmitt

Id: jasch264

---

# cruisen

Implementation of the Mars Rover task.

## Installation

1. Just clone the private Repository https://git.hhu.de/jasch264/mars-rover.
2. Navigate into the **"cruisen"** subdirectory.
3. Enter **"lein repl"** in your terminal and hit enter. ;)

## Usage

The API can be used via three functions in general:

1. **init!** - creates a board, positions obstacles and the rover on it and prints it to the console. It's possible to define a custom board size.
2. **hello-rover!** - is used to navigate the rover over the board.
3. **print-board!** - prints out the current board state to the terminal

## Examples

Using default values:
**(init!)**

Or custom ones:
**(init! 20 30)**

Navigate the rover:
**(hello-rover! "ffbbrrflf")**

### Bugs

...nothing yet

## License

Copyright © 2019 Jan Schmitt

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
