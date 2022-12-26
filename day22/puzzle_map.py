from collections import defaultdict
from itertools import cycle
from copy import copy
from dataclasses import dataclass
from enum import Enum


class Tile(str, Enum):
    FLOOR = "."
    WALL = "#"
    EMPTY = " "


# Part 1
DIRS = [(1, 0), (0, 1), (-1, 0), (0, -1)]
DIR_CHAR = [">", "V", "<", "^"]
TURN_MAP = {"R": 1, "L": -1}


@dataclass()
class State:
    dir_idx: int = 0
    x: int = 0
    y: int = 0
    face: int = 1


def wrapped_addition(x: int, min: int, max: int, dx: int):
    return (x - min + dx) % (max + 1 - min) + min


class Map:
    def __init__(self, map_: list[str]) -> None:
        self._map: dict[tuple[int, int], Tile] = {}
        # max/min x at y=i
        self.x_maxes: dict[str, int] = defaultdict(lambda: 0)
        self.x_mins: dict[str, int] = defaultdict(lambda: 10000)
        # max/min y at x=i
        self.y_maxes: dict[str, int] = defaultdict(lambda: 0)
        self.y_mins: dict[str, int] = defaultdict(lambda: 10000)

        for y, row in enumerate(map_):
            for x, position in enumerate(row):
                if position == " ":
                    continue

                self._map[(x, y)] = Tile(position)

                self.x_maxes[y] = max(x, self.x_maxes[y])
                self.x_mins[y] = min(x, self.x_mins[y])
                self.y_maxes[x] = max(y, self.y_maxes[x])
                self.y_mins[x] = min(y, self.y_mins[x])

    def __getitem__(self, key: tuple[int, int]):
        return self._map[key]

    def print_with_history(self, history: list[State]):
        y_max = max(self.y_maxes.values())
        x_max = max(self.x_maxes.values())

        lines = []
        for y in range(y_max + 1):
            line = []
            for x in range(x_max + 1):
                line.append(self._map.get((x, y), Tile.EMPTY).value)

            lines.append(line)

        for state in history:
            lines[state.y][state.x] = DIR_CHAR[state.dir_idx]

        print("\n".join(["".join(line) for line in lines]))


def step_state(state: State, map_: Map, instruction: str | int):
    if isinstance(instruction, str):
        state.dir_idx = (state.dir_idx + TURN_MAP[instruction]) % 4
        return state, [copy(state)]

    history = []
    for _ in range(instruction):
        x, y = state.x, state.y
        dx, dy = DIRS[state.dir_idx]

        new_x = wrapped_addition(state.x, map_.x_mins[y], map_.x_maxes[y], dx)
        new_y = wrapped_addition(state.y, map_.y_mins[x], map_.y_maxes[x], dy)

        if map_[(new_x, new_y)] == "#":
            break

        state.x, state.y = new_x, new_y

        history.append(copy(state))

    return state, history


def part1(filename):
    map_, moves = read_input(filename)
    state = State()
    state.x = map_.x_mins[0]

    history = [copy(state)]
    # map_.print_with_history(history)
    for move in moves:
        # print('---')
        # print(move)
        state, step_history = step_state(state, map_, move)
        history.extend(step_history)
        # map_.print_with_history(history)

    print((state.y + 1) * 1000 + (state.x + 1) * 4 + state.dir_idx)


# Part 2

FACE_NEIGHBORS = {
    1: [6, 4, 3, 2],
    2: [3, 5, 6, 1],
    3: [4, 5, 2, 1],
    4: [6, 5, 3, 1],
    5: [6, 2, 3, 4],
    6: [1, 2, 5, 4],
}


def face_corner(size: int, face: int):
    corners = {
        1: (size * 2, 0),
        2: (0, size),
        3: (size, size),
        4: (size * 2, size * 1),
        5: (size * 2, size * 2),
        6: (size * 3, size * 2),
    }

    return corners[face]


def translate_face(
    x: int, y: int, from_face: int, to_face: int, size: int
) -> tuple[int, int, int]:
    max_val = size - 1
    flipped_x = size - x - 1
    flipped_y = size - y - 1
    match (from_face, to_face):
        case (1, 2):
            return flipped_x, 0, 1
        case (1, 3):
            return y, 0, 1
        case (1, 4):
            return x, 0, 1
        case (1, 6):
            return max_val, flipped_y, 2
        case (2, 1):
            return flipped_x, 0, 1
        case (2, 3):
            return 0, y, 0
        case (2, 5):
            return flipped_x, y, 3
        case (2, 6):
            return flipped_y, max_val, 3
        case (3, 1):
            return 0, x, 0
        case (3, 2):
            return max_val, y, 2
        case (3, 4):
            return 0, y, 0
        case (3, 5):
            return 0, flipped_x, 0
        case (4, 1):
            return x, max_val, 3
        case (4, 3):
            return max_val, y, 2
        case (4, 5):
            return x, 0, 1
        case (4, 6):
            return flipped_y, 0, 1
        case (5, 2):
            return flipped_x, max_val, 3
        case (5, 3):
            return flipped_y, max_val, 3
        case (5, 4):
            return x, max_val, 3
        case (5, 6):
            return 0, y, 0
        case (6, 1):
            return max_val, flipped_y, 2
        case (6, 2):
            return 0, x, 0
        case (6, 4):
            return max_val, flipped_x, 2
        case (6, 5):
            return max_val, y, 2


def cube_to_plane(cube_state: State, size: int) -> tuple[int, int]:
    fx, fy = face_corner(size, cube_state.face)
    return fx + cube_state.x, fy + cube_state.y


def step_state_cube(state: State, map_: list[str], instruction: str | int):
    cube_size = len(map_) // 3

    if isinstance(instruction, str):
        state.dir_idx = (state.dir_idx + TURN_MAP[instruction]) % 4
        return state, [copy(state)]

    history = []
    for _ in range(instruction):
        x, y = state.x, state.y
        dx, dy = DIRS[state.dir_idx]
        new_state = copy(state)
        new_state.x, new_state.y = x + dx, y + dy

        # If we stepped off the current face, wrap around
        if not ((0 <= new_state.x < cube_size) and (0 <= new_state.y < cube_size)):
            to_face = FACE_NEIGHBORS[state.face][state.dir_idx]
            new_state.x, new_state.y, new_state.dir_idx = translate_face(
                x, y, state.face, to_face, cube_size
            )
            new_state.face = to_face

        fx, fy = cube_to_plane(new_state, cube_size)
        if map_[fy][fx] == "#":
            break

        state = new_state

        history.append(copy(state))

    return state, history


def print_map(map_: list[str], history: list[State]):
    list_map = [list(line) for line in map_]
    cube_size = len(list_map) // 3

    for state in history:
        fx, fy = cube_to_plane(state, cube_size)
        list_map[fy][fx] = DIR_CHAR[state.dir_idx]
    print("\n".join(["".join(line) for line in list_map]))


def part2(filename):
    with open(filename) as f:
        map_, moves = f.read().split("\n\n")
        map_ = map_.split("\n")

    state = State()
    history = [copy(state)]
    for move in (partition(moves)):
        state, step_history = step_state_cube(state, map_, move)
        history.extend(step_history)
        print("---")
        print(move)
        print_map(map_, history[-15:])

    print("---")
    print_map(map_, history)

    fx, fy = cube_to_plane(state, len(map_) // 3)
    print((fy + 1) * 1000 + (fx + 1) * 4 + state.dir_idx)


# Utils
def partition(xs: str) -> list[int, str]:
    result = []
    int_buff = []
    for c in xs:
        if c.isdigit():
            int_buff += c
        else:
            result.append(int("".join(int_buff)))
            result.append(c)
            int_buff = []
    if int_buff:
        result.append(int("".join(int_buff)))

    return result


def read_input(filename):
    with open(filename) as f:
        map_, moves = f.read().split("\n\n")

        return Map(map_.split("\n")), partition(moves)


if __name__ == "__main__":
    # part1("test.txt")
    # part2('test.txt')
    # part1("input.txt")
    part2("input.txt")
