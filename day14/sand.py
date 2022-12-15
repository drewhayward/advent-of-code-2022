from enum import Enum, auto

class Tile(Enum):
    WALL = auto()
    SAND = auto()

def get_dir(x1, x2) -> int:
    if x1 == x2:
        return 0
    if x1 > x2:
        return -1
    return 1

def sand_dirs():
    yield (0, 1)
    yield (-1, 1)
    yield (1, 1)

class Map:
    def __init__(self) -> None:
        self.occupancy_map = dict()
        self.min_x = 100000
        self.min_y = 0
        self.max_x = -1
        self.max_y = -1
        self.floor = 100000

    def __contains__(self, item):
        if item[1] >= self.floor:
            return True
        return item in self.occupancy_map

    def __getitem__(self, key: tuple[int, int]):
        if key[1] >= self.floor:
            return Tile.WALL
        return self.occupancy_map

    def init_map(self, lines: list[list[tuple[int, int]]]):
        for line in lines:
            for i in range(1, len(line)):
                pair1, pair2 = line[i - 1], line[i]
                dx = get_dir(pair1[0], pair2[0])
                dy = get_dir(pair1[1], pair2[1])
                cx, cy = pair1
                tx, ty = pair2
                while cx != tx or cy != ty:
                    self.occupancy_map[(cx,cy)] = Tile.WALL
                    cx += dx
                    cy += dy

                self.occupancy_map[(cx,cy)] = Tile.WALL

        for x, y in self.occupancy_map.keys():
            self.max_x = max(x, self.max_x)
            self.max_y = max(y, self.max_y)
            self.min_x = min(x, self.min_x)

    def print_map(self):
        xs = [x for x,y in self.occupancy_map.keys()]
        for y in range(self.min_y, self.max_y + 1):
            for x in range(min(xs), max(xs) + 1):
                if (x, y) not in self.occupancy_map:
                    print('.', end='')
                elif x == self.floor or self.occupancy_map[(x, y)] == Tile.WALL:
                    print('#', end='')
                elif self.occupancy_map[(x, y)] == Tile.SAND:
                    print('o', end='')
            print()

    def settle_sand(self) -> bool:
        sand_pos = (500,0)
        # while the sand could feasibly still hit something
        sand_placed = False
        while self.min_y <= sand_pos[1] <= self.max_y:
            sx, sy = sand_pos
            for dx, dy in sand_dirs():
                if (sx + dx, sy + dy) not in self:
                    sand_pos = (sx + dx, sy + dy)
                    break
            else: # the sand settles
                self.occupancy_map[sand_pos] = Tile.SAND
                sand_placed = True
                break # No options, sand remains

        return sand_placed

def read_input(filename):
    with open(filename) as f:
        lines = []
        for line in f.read().splitlines(keepends=False):
            lines.append([tuple(map(int, pair.split(',')) ) for pair in line.split(' -> ')])

    return lines

def part1(inp) -> int:
    map = Map()
    map.init_map(inp)
    sand_count = 0
    while map.settle_sand():
        sand_count += 1

    map.print_map()
    print(sand_count)

def part2(inp) -> int:
    map = Map()
    map.init_map(inp)
    map.floor = map.max_y + 2
    map.max_y += 2
    sand_count = 0
    while (500,0) not in map.occupancy_map:
        map.settle_sand()
        sand_count += 1
    print(sand_count)

if __name__ == "__main__":
    from pprint import pprint
    print("I hate sand...")

    test_input = read_input("test.txt")
    part1(test_input)
    part2(test_input)

    input = read_input("input.txt")
    print('Part 1')
    part1(input)

    print('Part 2')
    part2(input)