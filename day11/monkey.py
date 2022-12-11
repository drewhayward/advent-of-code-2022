from copy import deepcopy
from functools import reduce
import operator
import math
from collections import deque
from tqdm import trange

class Monkey:
    def __init__(
        self,
        num: int,
        items: list[int],
        operation_expr: str,
        div_test: int,
        true_monkey: int,
        false_monkey: int,
    ) -> None:
        self.num = num
        self.items = deque(items)
        self.operation_expr = operation_expr
        self.div_test = div_test
        self.true_monkey = true_monkey
        self.false_monkey = false_monkey

        self.counter = 0

    def __str__(self) -> str:
        return f"Monkey {self.num}: {', '.join(map(str, self.items))}"

def parse_monkey(contents: str) -> Monkey:
    lines = iter(contents.split('\n'))

    return Monkey(
        num=int(next(lines)[7:-1]),
        items = list(map(int, next(lines).split(': ')[-1].split(', '))),
        operation_expr = next(lines).split(' = ')[-1],
        div_test = int(next(lines).split('by ')[-1]),
        true_monkey = int(next(lines).split('monkey')[-1]),
        false_monkey = int(next(lines).split('monkey')[-1]),
    )

def run_round(monkeys: list[Monkey], reduce_nums=True):
    magic_monkey_num = reduce(operator.mul, [m.div_test for m in monkeys])
    for monkey in monkeys:
        while monkey.items:
            item = monkey.items.popleft()
            monkey.counter += 1
            new_value = eval(monkey.operation_expr, {'old': item})
            if reduce_nums:
                new_value = math.floor(new_value / 3)
            else:
                new_value = new_value % magic_monkey_num

            if new_value % monkey.div_test == 0:
                monkeys[monkey.true_monkey].items.append(new_value)
            else:
                monkeys[monkey.false_monkey].items.append(new_value)


def run_monkeys(monkeys, rounds=20, reduce=True) -> int:
    monkeys = deepcopy(monkeys)
    for round in range(1, rounds + 1, 1):
        run_round(monkeys, reduce_nums=reduce)


    monkeys.sort(key=lambda m: m.counter)

    return monkeys[-1].counter * monkeys[-2].counter


def read_file(filename) -> list[Monkey]:
    with open(filename) as f:
        return list(map(parse_monkey, f.read().split('\n\n')))

if __name__ == "__main__":

    test_monkeys = read_file('test.txt')
    print(run_monkeys(test_monkeys))
    print(run_monkeys(test_monkeys, rounds=10000, reduce=False))

    monkeys = read_file('input.txt')
    print("Part 1")
    print(run_monkeys(monkeys))

    print("Part 2")
    print(run_monkeys(monkeys, rounds=10000, reduce=False))
