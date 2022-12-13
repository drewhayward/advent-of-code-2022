from itertools import chain, repeat
from functools import cmp_to_key
from typing import Any
from ast import literal_eval
from enum import Enum, auto


def read_input(filename):
    with open(filename) as f:
        contents = f.read()

        blocks = contents.split("\n\n")

        return [tuple(map(literal_eval, block.split("\n"))) for block in blocks]


class OrderedStatus(int, Enum):
    IN_ORDER = -1
    INCONCLUSIVE = 0
    OUT_OF_ORDER = 1


def ordering(
    packet1: list[list[int] | int] | int, packet2: list[list[int] | int] | int
) -> OrderedStatus:
    match (packet1, packet2):
        case int(p1), list(p2):
            return ordering([p1], p2)
        case list(p1), int(p2):
            return ordering(p1, [p2])
        case int(p1), int(p2):
            if p1 < p2:
                return OrderedStatus.IN_ORDER
            if p1 == p2:
                return OrderedStatus.INCONCLUSIVE
            return OrderedStatus.OUT_OF_ORDER
        case list(p1), list(p2):
            left_iter = chain(p1, repeat(None))
            right_iter = chain(p2, repeat(None))
            for left, right in zip(left_iter, right_iter):
                if left is not None and right is not None:
                    compared = ordering(left, right)
                    if compared != OrderedStatus.INCONCLUSIVE:
                        return compared
                    continue

                if left is None and right is not None:
                    return OrderedStatus.IN_ORDER
                if left is not None and right is None:
                    return OrderedStatus.OUT_OF_ORDER

                return OrderedStatus.INCONCLUSIVE
            

def part1(pairs):
    total = 0
    for i, (left, right) in enumerate(pairs):
        if ordering(left, right) == OrderedStatus.IN_ORDER:
            total += i + 1

    return total

def part2(pairs):
    flattened_input = list(chain.from_iterable(pairs))
    flattened_input.append([[6]])
    flattened_input.append([[2]])
    flattened_input = sorted(flattened_input, key=cmp_to_key(ordering))

    divider_index1 = flattened_input.index([[6]]) + 1
    divider_index2 = flattened_input.index([[2]]) + 1

    return divider_index1 * divider_index2



if __name__ == "__main__":
    test_input = read_input("test.txt")
    print(part1(test_input))
    print(part2(test_input))

    input = read_input("input.txt")
    print('Part 1')
    print(part1(input))

    print('Part 2')
    print(part2(input))
