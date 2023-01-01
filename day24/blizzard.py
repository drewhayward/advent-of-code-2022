from pprint import pprint
from collections import deque, defaultdict
from functools import lru_cache

WIND_TO_DIR = {">": (1, 0), "v": (0, 1), "<": (-1, 0), "^": (0, -1)}


def read_input(filename) -> tuple[list[tuple[int, int, str]], int, int]:
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


def walkable_neighbors(
    x: int, y: int, width: int, height: int, occupied: set[tuple[int, int]]
):
    if x == 0 and y == -1:
        if (0, 0) not in occupied:
            yield (0, 0)
        yield (0, -1)
        return

    if x == width - 1 and y == height - 1:
        yield (width - 1, height)

    if x == 0 and y == 0:
        yield (0, -1)

    for dx, dy in WIND_TO_DIR.values():
        nx = x + dx
        ny = y + dy

        in_bounds = 0 <= nx < width and 0 <= ny < height

        if (nx, ny) not in occupied and in_bounds:
            yield (nx, ny)

    if (x, y) not in occupied:
        yield (x, y)


def navigate_blizzard(
    winds: list[tuple[int, int, str]],
    width: int,
    height: int,
    goal: tuple[int, int],
    start: tuple[int, int],
    start_minute: int = 0,
):
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

        return occupied

    # BFS search though blizzard spacetime
    frontier = deque([(start[0], start[1], start_minute, None)])
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

        assert (x, y) not in winds_at_minute(minutes)

        if (x, y) == goal:
            best = minutes
            break

        for nx, ny in walkable_neighbors(
            x, y, width, height, winds_at_minute(minutes + 1)
        ):
            # print(f"\tGoing to {(nx, ny)} in the next minute")
            frontier.append((nx, ny, minutes + 1, (x, y, minutes)))

    return best


def part1(filename):
    winds, width, height = read_input(filename)
    mins = navigate_blizzard(
        winds, width, height, goal=(width - 1, height), start=(0, -1)
    )
    print(mins)


def part2(filename):
    winds, width, height = read_input(filename)
    start, end = (0, -1), (width - 1, height)
    min1 = navigate_blizzard(winds, width, height, goal=end, start=start)
    min2 = navigate_blizzard(
        winds, width, height, goal=start, start=end, start_minute=min1
    )
    min3 = navigate_blizzard(
        winds, width, height, goal=end, start=start, start_minute=min2
    )
    print(min1, min2, min3)


if __name__ == "__main__":
    part1("test.txt")
    part2("test.txt")
    part1("input.txt")
    part2("input.txt")
