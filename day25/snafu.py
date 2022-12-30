from typing import Optional
import math

SNAFU_DIGIT_TO_VALUE = {"2": 2, "1": 1, "0": 0, "-": -1, "=": -2}

DIGIT_TO_SNAFU = {
    0: ("0", 0),
    1: ("1", 0),
    2: ("2", 0),
    3: ("=", 1),
    4: ("-", 1),
    5: ("0", 1)
}


def snafu_to_int(snafu: str) -> int:
    total = 0
    power = len(snafu) - 1
    for c in snafu:
        total += SNAFU_DIGIT_TO_VALUE[c] * (5**power)
        power -= 1

    return total


def int_to_snafu(num: int) -> str:
    def _to_snafu(num: int, pow: int) -> tuple[str, int]:
        if pow == -1:
            return "", 0

        level_digit = num // (5 ** pow)
        sub_snaf, sub_carry = _to_snafu(num % (5 ** pow), pow - 1)
        snaf_char, carry = DIGIT_TO_SNAFU[level_digit + sub_carry]
        return snaf_char + sub_snaf, carry

    return _to_snafu(num, math.ceil(math.log(num, 5)))[0].lstrip('0')

def part1(filename):
    with open(filename) as f:
        snafu_nums = f.read().split('\n')

    ans = int_to_snafu(sum(map(snafu_to_int, snafu_nums)))
    print(ans)

if __name__ == "__main__":
    part1('test.txt')
    part1('input.txt')
