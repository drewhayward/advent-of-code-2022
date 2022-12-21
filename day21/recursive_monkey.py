import operator
from functools import lru_cache
from typing import Union

OP_TO_FUNC = {
    "+": operator.add,
    "*": operator.mul,
    "/": operator.floordiv,
    "-": operator.sub,
}

def read_input(filename):
    with open(filename) as f:
        result = {}
        for line in f.read().split("\n"):
            name, lhs = line.split(": ")
            match lhs.split(" "):
                case [num]:
                    result[name] = int(num)
                case list(expr):
                    result[name] = expr
        return result

def make_monkey_say(monkey_map):
    @lru_cache()
    def monkey_say(monkey: str):
        say = monkey_map[monkey]
        if isinstance(say, int):
            return say

        arg1, op, arg2 = say
        return OP_TO_FUNC[op](monkey_say(arg1), monkey_say(arg2))
    return monkey_say

def part1(monkey_map: dict[str, Union[list[str], int]]):
    monkey_say = make_monkey_say(monkey_map)
    return monkey_say("root")


def part2(monkey_map: dict[str, Union[list[str], int]]):
    monkey_say = make_monkey_say(monkey_map)
    monkey_say("root")

    def force_value(monkey, value=None):
        if monkey == "humn":
            return value

        if isinstance(monkey_map[monkey], int):
            return None

        lhs, op, rhs = monkey_map[monkey]
        if monkey == "root":
            op = "="

        # adjust left side, fix right
        right_value = monkey_say(rhs)
        match op:
            case "/":
                child_target = value * right_value
            case "*":
                child_target = value // right_value
            case "-":
                child_target = value + right_value
            case "+":
                child_target = value - right_value
            case "=":
                child_target = right_value
        left_result = force_value(lhs, child_target)

        # adjust right side, fix left
        left_value = monkey_say(lhs)
        match op:
            case "/":
                child_target = left_value // value
            case "*":
                child_target = value // left_value
            case "-":
                child_target = left_value - value
            case "+":
                child_target = value - left_value
            case "=":
                child_target = left_value
        right_result = force_value(rhs, child_target)

        return left_result or right_result

    return force_value("root")


if __name__ == "__main__":
    test_input = read_input("test.txt")
    print(part1(test_input))
    print(part2(test_input))

    input = read_input("input.txt")
    print(part1(input))
    print(part2(input))
