from itertools import chain, product
from functools import partial
from collections import Counter

def gen_dirs():
    for dir in product([-1,0,1], repeat=3):
        total = sum(map(abs, dir))
        if total == 3 or total == 0:
            continue

        yield dir


def read_input(filename):
    with open(filename) as f:
        contents = f.read()
        return [tuple(map(int, line.split(","))) for line in contents.split("\n")]


def bordering(block, full=False):
    bx, by, bz = block
    for dir in product([-1,0,1], repeat=3):
        dx, dy, dz = dir
        if sum(map(abs,dir)) == 1 or full:
            yield (bx + dx, by + dy, bz + dz)


def part1(blocks):
    area = Counter(chain.from_iterable(map(bordering, blocks)))
    for block in blocks:
        if block in area:
            del area[block]

    return sum(area.values())


def part2(blocks):
    blocks = set(blocks)
    area = Counter(chain.from_iterable(map(bordering, blocks)))
    for block in blocks:
        if block in area:
            del area[block]

    # all diagonally and adjacent blocks. Subtracting the original blocks
    # disconnects the inner and outer surfaces
    full_shell = set(chain.from_iterable(map(partial(bordering, full=True), blocks))) - blocks
    shell_sets: list[set(tuple(int,int,int))] = []
    while full_shell:
        visited = set()
        frontier = [full_shell.pop()]
        while frontier:
            block = frontier.pop()

            if block not in visited:
                visited.add(block)
            else:
                continue
                
            for neighbor in bordering(block):
                if neighbor in full_shell:
                    frontier.append(neighbor)
        
        full_shell.difference_update(visited)
        shell_sets.append(visited)

    best = 0
    for shell in shell_sets:
        total = 0
        for block in shell:
            total += area.get(block, 0)
        best = max(total, best)

    return best

if __name__ == "__main__":
    test_input = read_input("test.txt")
    print(part1(test_input))
    print(part2(test_input))

    input = read_input("input.txt")
    print("Part 1")
    print(part1(input))
    print("Part 2")
    print(part2(input))
