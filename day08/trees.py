from itertools import repeat


def parseInput(file):
    with open(file) as f:
        contents = f.read().splitlines(keepends=False)

        return [list(map(int, line)) for line in contents]


def visible(trees: list[int]) -> set[int]:
    trees = list(trees)
    tallest_height = trees[0]
    visible = {0, len(trees) - 1}
    for i, tree_height in enumerate(trees):
        if tree_height > tallest_height:
            visible.add(i)

        tallest_height = max(tallest_height, tree_height)

    tallest_height = trees[-1]
    for i in range(len(trees) - 1, -1, -1):
        tree_height = trees[i]
        if tree_height > tallest_height:
            visible.add(i)

        tallest_height = max(tallest_height, tree_height)

    return visible


def count_visible(trees: list[list[int]]) -> int:
    visible_trees = set()
    # Left/Right
    for y in range(len(trees)):
        visible_trees |= set(zip(visible(trees[y]), repeat(y)))

    # Up/Down
    for x in range(len(trees[0])):
        vertical = [trees[y][x] for y in range(len(trees))]
        visible_trees |= set(zip(repeat(x), visible(vertical)))

    return len(visible_trees)


def in_bounds(trees, x, y):
    return (0 <= x < len(trees[0])) and (0 <= y < len(trees))

def scenic_score(trees: list[list[int]], start_x, start_y) -> int:
    score = 1
    for dx, dy in [(0,1), (1,0), (0, -1), (-1, 0)]:
        x, y = start_x, start_y
        dir_score = 0
        while in_bounds(trees, x + dx, y + dy):
            dir_score += 1

            if trees[start_y][start_x] <= trees[y + dy][x + dx]:
                break

            x += dx
            y += dy

        score *= dir_score
        
    return score
    
def best_score(trees):
    best = 0
    for y in range(len(trees)):
        for x in range(len(trees[0])):
            best = max(scenic_score(trees, x, y), best)

    return best


if __name__ == "__main__":
    print("Test")
    test_input = parseInput("test.txt")
    print(count_visible(test_input))
    print(best_score(test_input))

    print('Part 1')
    input = parseInput("input.txt")
    print(count_visible(input))

    print("Part 2")
    print(best_score(input))