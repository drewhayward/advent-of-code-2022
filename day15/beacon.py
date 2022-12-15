from tqdm import trange


def read_input(filename):
    with open(filename) as f:
        contents = f.read()
        pairs = []
        for line in contents.split("\n"):
            # Sensor at x=14, y=17: closest beacon is at x=10, y=16
            words = line.split()
            sensor = int(words[2][2:-1]), int(words[3][2:-1])
            beacon = int(words[-2][2:-1]), int(words[-1][2:])
            pairs.append((sensor, beacon))

        return pairs


def part1(test_input: list[tuple[tuple[int, int], tuple[int, int]]], y_value: int):
    # want the number of points that are readable by a sensor
    sensible = set()
    for sensor, beacon in test_input:
        total_dist = abs(sensor[0] - beacon[0]) + abs(sensor[1] - beacon[1])

        # skip sensors that can't even detect this line
        slack = total_dist - abs(sensor[1] - y_value)
        if slack < 0:
            continue

        sensible.update(range(sensor[0] - slack, sensor[0] + slack + 1))

    # subtract beacons which are in that range
    for _, (bx, by) in test_input:
        if bx in sensible and by == y_value:
            sensible.remove(bx)

    print(len(sensible))

def merge_interval_size(intervals: list[tuple[int, int]]) -> int:
    intervals.sort(key=lambda x: x[0])
    merged = []
    c_start, c_end = intervals[0]
    for start, end in intervals[1:]:
        if start <= c_end:
            c_end = max(end, c_end)
        else:
            merged.append((c_start, c_end))
            c_start, c_end = start, end
    merged.append((c_start, c_end))

    total = 0
    for start, end in merged:
        total += end - start

    return total, merged

def part2(puz_input, max_val=4000000):
    tx, ty = None, None
    for y_value in range(0, max_val + 1):
        intervals = []
        for sensor, beacon in puz_input:
            total_dist = abs(sensor[0] - beacon[0]) + abs(sensor[1] - beacon[1])

            # skip sensors that can't even detect this line
            slack = total_dist - abs(sensor[1] - y_value)
            if slack < 0:
                continue

            start_range = max(sensor[0] - slack, 0)
            end_range = min(sensor[0] + slack, max_val)
            intervals.append((start_range, end_range))

        used_space, merged_intervals = merge_interval_size(intervals)
        if used_space != max_val:
            print(merged_intervals)
            break

    print(y_value + (merged_intervals[0][1] + 1) * max_val)

if __name__ == "__main__":
    test_input = read_input("test.txt")
    # part1(test_input, 10)

    input = read_input("input.txt")
    part1(input, 2000000)
    part2(input)
