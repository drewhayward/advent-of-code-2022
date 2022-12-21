from functools import partial

class Node:
    def __init__(self, value: int) -> None:
        self.prev = None
        self.next = None
        self.value = value

    def extract(self) -> None:
        self.prev.next = self.next
        self.next.prev = self.prev
        self.prev, self.next = None, None

    def insert_after(self, before):
        after = before.next
        before.next = self
        after.prev = self
        self.next = after
        self.prev = before

    def move(self, total, key=1):
        curr = self.prev
        self.extract()

        to_move = (self.value * key) % (total - 1)
        while to_move != 0:
            if to_move > 0:
                curr = curr.next
                to_move -= 1
            elif to_move < 0:
                curr = curr.prev
                to_move += 1

        self.insert_after(curr)


def setup_nodes(nums: list[int]) -> list[Node]:
    nodes = list(map(Node, nums))
    for i in range(len(nodes) - 1):
        before, after = nodes[i], nodes[i + 1]
        before.next = after
        after.prev = before
    nodes[0].prev = nodes[-1]
    nodes[-1].next = nodes[0]

    return nodes

def mix(nums: list[int], key=1, mixes=1):
    nodes = setup_nodes(nums)
    zero_node = None
    for _ in range(mixes):
        for node in nodes:
            if node.value == 0:
                zero_node = node
            node.move(len(nodes), key=key)

    total = 0
    curr = zero_node
    for i in range(1, 3001):
        curr = curr.next
        if i % 1000 == 0:
            total += curr.value * key

    print(total)


part1 = mix
part2 = partial(mix, key=811589153, mixes=10)


def read_input(filename) -> list[int]:
    with open(filename) as f:
        return list(map(int, f.read().splitlines(keepends=False)))


if __name__ == "__main__":
    test_input = read_input("test.txt")
    part1(test_input)
    part2(test_input)

    input = read_input("input.txt")
    part1(input)
    part2(input)
