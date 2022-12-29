from typing import Optional
from itertools import product
from collections import defaultdict

CARDINAL_DIRS = [
    (0, -1),
    (0, 1),
    (-1, 0),
    (1, 0),
]


def read_input(filename) -> set[tuple[int, int]]:
    with open(filename) as f:
        results = set()
        for y, line in enumerate(f.readlines()):
            for x, char in enumerate(line):
                if char == "#":
                    results.add((x, y))
        return results


def neighbors(elf: tuple[int, int]) -> tuple[int, int]:
    x, y = elf
    for dx, dy in product([-1, 0, 1], repeat=2):
        if dx == 0 and dy == 0:
            continue
        yield x + dx, y + dy


def dir_neighbors(elf: tuple[int, int], dir: tuple[int, int]) -> tuple[int, int]:
    elf = list(elf)
    zindex = dir.index(0)
    other_idx = (zindex + 1) % 2
    for d in [-1, 0, 1]:
        new_pos = [0, 0]
        new_pos[other_idx] = elf[other_idx] + dir[other_idx]
        new_pos[zindex] = elf[zindex] + d
        yield tuple(new_pos)


def diffuse_elves(elves: set[tuple[int, int]], max_rounds: Optional[int] = None):
    elves = set(elves)
    round = 1
    while True:
        potential_moves = defaultdict(list)
        for elf in elves:
            if not any(map(elves.__contains__, neighbors(elf))):
                continue

            for i in range(4):
                dir = CARDINAL_DIRS[(round + i - 1) % 4]
                if not any(map(elves.__contains__, dir_neighbors(elf, dir))):
                    new_pos = (dir[0] + elf[0], dir[1] + elf[1])
                    potential_moves[new_pos].append(elf)
                    break

        moved = False
        for new_pos, eagar_elves in potential_moves.items():
            if len(eagar_elves) != 1:
                continue
            moving_elf = eagar_elves[0]
            elves.remove(moving_elf)
            elves.add(new_pos)

            moved = True

        if not moved:
            print("stagnation round", round)
            break
            
        if round == max_rounds:
            break

        round += 1

    if max_rounds is not None:
        min_x = min(map(lambda x: x[0], elves))
        max_x = max(map(lambda x: x[0], elves))
        min_y = min(map(lambda x: x[1], elves))
        max_y = max(map(lambda x: x[1], elves))
        area = (max_x - min_x + 1) * (max_y - min_y + 1)
        print("area", area - len(elves))


if __name__ == "__main__":
    test_elves = read_input("test.txt")
    elves = read_input("input.txt")

    diffuse_elves(test_elves, max_rounds=10)
    diffuse_elves(test_elves)
    diffuse_elves(elves, max_rounds=10)
    diffuse_elves(elves)
