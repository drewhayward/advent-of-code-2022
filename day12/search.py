import heapq as hq
from copy import deepcopy

def read_input(filename) -> list[list[str]]:
    with open(filename) as f:
        lines = f.read().splitlines(keepends=False)
        return list(map(list, lines))

def print_state(map, visited):
    for y in range(len(map)):
        for x, c in enumerate(map[y]):
            if (x, y) in visited:
                print('*', end='')
            else:
                print(c, end='')
        print()

def climbing_edge(from_edge, to_edge, map):
    from_elevaption = ord(map[from_edge[1]][from_edge[0]])
    to_elevation = ord(map[to_edge[1]][to_edge[0]])
    return to_elevation - 1 <= from_elevaption

def desc_edge(from_edge, to_edge, map):
    return climbing_edge(to_edge, from_edge, map)

def shortest_path(map: list[list[int]], start, edge_fn, goal=None) -> int:
    height = len(map)
    width = len(map[0])

    def neighbors(x, y):
        for dx, dy in [(0,1), (1,0), (-1, 0), (0,-1)]:
            if 0 <= x + dx < width and 0 <= y + dy < height:
                yield x + dx, y + dy

    visited = set()
    frontier = [(0, start)]
    while frontier:
        steps, pos = hq.heappop(frontier)
        x, y = pos

        if goal and pos == goal:
            return steps
        elif not goal and map[y][x] == 'a':
            return steps

        if pos in visited:
            continue
        visited.add(pos)

        # Add the neighbors
        current_elevation = ord(map[y][x])
        for nx, ny in neighbors(*pos):
            if edge_fn(pos, (nx, ny), map):
                hq.heappush(frontier, (steps + 1, (nx, ny)))


def prep_map(map: list[list[str]]):
    start_pos = None
    goal_pos = None
    for y in range(len(map)):
        for x, map_level in enumerate(map[y]):
            if map_level == "S":
                map[y][x] = 'a'
                start_pos = (x, y)
            
            if map_level == "E":
                map[y][x] = "z"
                goal_pos = (x, y)

    return start_pos, goal_pos

def part1(map):
    map = deepcopy(map)
    start, goal = prep_map(map)
    return shortest_path(map, start, edge_fn=climbing_edge, goal=goal)

def part2(map):
    map = deepcopy(map)
    _, start = prep_map(map)
    return shortest_path(map, start, edge_fn=desc_edge, goal=None)

if __name__ == "__main__":
    test_input = read_input('test.txt')
    print(part1(test_input))
    print(part2(test_input))

    input = read_input('input.txt')
    print("Part 1")
    print(part1(input))
    
    print("Part 2")
    print(part2(input))