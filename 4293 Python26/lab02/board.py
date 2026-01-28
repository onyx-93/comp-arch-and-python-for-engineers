_EMPTY = ' ' # Used to indicate empty spaces in the board

class InvalidMoveError(ValueError):
    pass

class ConnectFourBoard:

    """Represents a Connect 4 board. Handles board state and checks moves for validity."""

    def __init__(self, num_rows, num_cols): #constructor method
        """Initialize a new board"""
        self.num_rows = num_rows
        self.num_cols = num_cols
        self.clear()

    def clear(self): #instance method
        """Replace all pieces with empty spaces."""
        self.rows = list()
        for row in range(self.num_rows):
            self.rows.append([_EMPTY for col in range(self.num_cols)])

    def display(self): #instance method
        """Display the current board state"""
        for row in range(self.num_rows):
            print(f'\t|{"|".join(self.rows[row])}|')
        print('\t ' + ' '.join([str(col) for col in range(self.num_cols)]))

    def check_winner(self): #instance method
        """Check whether someone has won the game."""
        # TODO: Implement this function instead of just returning false
        for row in range(self.num_rows):
            for col in range(self.num_cols):
                symbol = self.rows[row][col]
                if symbol is _EMPTY:
                    continue

                # Check horizontal
                if col + 3 < self.num_cols:
                    if (self.rows[row][col + 1] == symbol and
                        self.rows[row][col + 2] == symbol and
                        self.rows[row][col + 3] == symbol):
                        return True

                # Check vertical
                if row + 3 < self.num_rows:
                    if (self.rows[row + 1][col] == symbol and
                        self.rows[row + 2][col] == symbol and
                        self.rows[row + 3][col] == symbol):
                        return True

                # Check diagonal down-right
                if row + 3 < self.num_rows and col + 3 < self.num_cols:
                    if (self.rows[row + 1][col + 1] == symbol and
                        self.rows[row + 2][col + 2] == symbol and
                        self.rows[row + 3][col + 3] == symbol):
                        return True

                # Check diagonal up-right
                if row - 3 >= 0 and col + 3 < self.num_cols:
                    if (self.rows[row - 1][col + 1] == symbol and
                        self.rows[row - 2][col + 2] == symbol and
                        self.rows[row - 3][col + 3] == symbol):
                        return True
       # return False

    def is_full(self): #instance method
        """Check whether the board is full."""
        # TODO: Implement this function instead of just returning false
        return False

    def add_piece(self, col, symbol): #instance method
        """Add a piece to the specified column."""
        # TODO: Add code to check if the move is valid (and raise InvalidMoveError if not)

        # Find the first empty row in col and replace it with symbol
        for row in reversed(range(self.num_rows)):
            if self.rows[row][col] is _EMPTY:
                self.rows[row][col] = symbol
                break

# board = ConnectFourBoard(6, 7) this is an example of how to create an object of the class ConnectFourBoard
# board.display() this is an example of how to call the display method on the object board 