from dataclasses import dataclass
import math

def round_away(x: float) -> int:
    if x >= 0:
        return math.ceil(x)
    else:
        return math.floor(x)

@dataclass
class Vector:
    x: int
    y: int

    def __add__(self, other):
        return Vector(self.x + other.x, self.y + other.y)

    def __sub__(self, other):
        return Vector(self.x - other.x, self.y - other.y)

    def __hash__(self) -> int:
        return hash(f"{self.x},{self.y}")

    def norm(self) -> float:
        return math.sqrt(self.x**2 + self.y**2)

    def round(self):
        return Vector(round_away(self.x / 2.0), round_away(self.y / 2.0))

def tail_movement(tail_pos: Vector, head_pos: Vector) -> Vector:
    dir = head_pos - tail_pos

    if dir.norm() <= math.sqrt(2):
        return Vector(0,0)
    else:
        return dir.round()

class Rope:
    def __init__(self, length: int) -> None:
        self.segments = [Vector(0,0) for _ in range(length)]
        self.tail_history = {self.segments[-1]}

    def move(self, dir_char: str):
        direction = LETTER_TO_DIR[dir_char]
        for i, segment in enumerate(self.segments):
            if i != 0:
                direction = tail_movement(segment, self.segments[i - 1])

            self.segments[i] += direction
        
        self.tail_history.add(self.segments[-1])

    def show_rope(self, width, height, offset=Vector(0, 0)):
        for y in range(height - 1, -1, -1):
            for x in range(width):
                cursor_pos = Vector(x, y) - offset
                for i, segment in enumerate(self.segments):
                    if segment == cursor_pos:
                        print(i % 10, end='')
                        break

                    if cursor_pos == Vector(0,0):
                        print('s', end='')
                        break
                else:
                    print('.', end="")
                
            print()

LETTER_TO_DIR = {
    "R":  Vector(1,0),
    "L":  Vector(-1,0),
    "U":  Vector(0,1),
    "D":  Vector(0,-1)
}


def track_rope(movements: list[str], length: int = 2) -> int:
    rope = Rope(length)
    for movement in movements:
        direction, count = movement[0], int(movement[2:])
        for _ in range(count):
            rope.move(direction)
        

    return len(rope.tail_history)

def read_input(filename):
    with open(filename) as f:
        return f.read().splitlines(keepends=False)

if __name__ == "__main__":
    print(track_rope(read_input('test.txt'), 10))

    input = read_input('input.txt')
    print("Part 1")
    print(track_rope(input))

    print("Part 2")
    print(track_rope(input, length=10))