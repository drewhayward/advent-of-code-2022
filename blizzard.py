from pprint import pprint
from collections import deque, defaultdict
from functools import lru_cache

WIND_TO_DIR = {
    ">": (1,0),
    "v": (0, 1),
    "<": (-1, 0),
    "^": (0, -1)
}

def read_input(filename) -> tuple[list[tuple[int,int,str]], int, int]:
    with open(filename) as f:
        winds = []
        lines = f.readlines()
        height = len(lines) - 2
        width = len(lines[0]) - 3
        for y, line in enumerate(lines[1:-1]):
            for x, c in enumerate(line[1:-2]):
                if c != ".":
                    winds.append((x, y, c))

        return winds, width, height



def walkable_neighbors(x: int, y: int, width: int, height: int, occupied: set[tuple[int,int]]):
    if x == 0 and y == -1:
        yield (0, 0)
        yield (0, -1)
        return
    
    if x == 0 and y == 0:
        yield (0, -1)

    for dx, dy in WIND_TO_DIR.values():
        nx = (x + dx)
        ny = (y + dy)

        in_bounds = 0 <= nx < width and 0 <= ny < height

        if (nx, ny) not in occupied and in_bounds:
            yield (nx, ny)
    
    if (x, y) not in occupied:
        yield (x, y)
        



def navigate_blizzard(winds: list[tuple[int, int, str]], width: int, height: int):
    @lru_cache
    def winds_at_minute(minute: int) -> set[tuple[int, int]]:
        # get wind postions for the next minute
        # occupied = set()
        occupied = defaultdict(list)
        for (x, y, dir) in winds:
            dx, dy = WIND_TO_DIR[dir]
            nx = (x + minute * dx) % width
            ny = (y + minute * dy) % height
            occupied[(nx, ny)].append(dir)

        for y in range(height):
            for x in range(width):
                if (x, y) not in occupied:
                    print(".", end='')
                elif len(occupied[(x,y)]) == 1:
                    print(occupied[(x,y)][0], end='')
                else:
                    print(len(occupied[(x,y)]), end='')
            print()
                

        return occupied

    # BFS search though blizzard spacetime
    frontier = deque([(0, -1, 0, None)])
    visited = set()
    best = 0
    parents = {}
    while frontier:
        if (node := frontier.popleft()) in visited:
            continue
        else:
            visited.add(node)

        x, y, minutes, parent = node
        parents[(x, y, minutes)] = parent

        if (x, y) == (width - 1, height - 1):
            best = minutes + 1
            break

        for nx, ny in walkable_neighbors(x, y, width, height, winds_at_minute(minutes + 1)):
            # print(f"\tGoing to {(nx, ny)} in the next minute")
            frontier.append((nx, ny, minutes + 1, (x, y, minutes)))

    # Reconstruct path
    path = []
    curr = (x, y, minutes)
    while curr is not None:
        path.append(curr)
        curr = parents[curr]

    path.reverse()
    pprint(path)
    
    return best
        
def part1(filename):
    winds, width, height = read_input(filename)
    mins  = navigate_blizzard(winds, width, height)
    print(mins)


if __name__ == "__main__":
    part1('test.txt')
    part1('input.txt')