from typing import Optional

def plan_cycles(instructions: list[str]) -> list[Optional[int]]:
    cycles = []
    for instr in instructions:
        match instr.split():
            case ["addx", num]:
                cycles.extend([None, int(num)])
            case ["noop"]:
                cycles.append(None)

    return cycles

def sim_cpu(cycles):
    x = 1
    signal_sum = 0
    cycle_check = 20
    for i, value in enumerate(cycles):
        cycle = i + 1

        if cycle == cycle_check:
            signal_sum += x * cycle
            cycle_check += 40

        if value is not None:
            x += value

    return signal_sum

def sim_pixels(cycles):
    pixels = []
    x = 1
    for i, value in enumerate(cycles):
        cycle = i + 1

        if x <= cycle % 40 < x + 3:
            pixels.append('#')
        else:
            pixels.append('.')

        if value is not None:
            x += value

    return pixels
    

def read_input(filename):
    with open(filename) as f:
        return f.read().splitlines(keepends=False)

def part1(instructions):
    return sim_cpu(plan_cycles(instructions))

def part2(instructions):
    cycles = plan_cycles(instructions)
    pixels = sim_pixels(cycles)
    print_image(pixels)

def print_image(pixels, width=40):
    for i in range(0, len(pixels), width):
        print(''.join(pixels[i : i + width]))

if __name__ == "__main__":
    test = read_input('test.txt')
    print(part1(test))
    print(part2(test))

    input = read_input('input.txt')
    print("Part 1")
    print(part1(input))

    print("Part 2")
    print(part2(input))
