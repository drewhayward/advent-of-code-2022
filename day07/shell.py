from functools import lru_cache

TOTAL_SIZE = 70000000
UPDATE_SIZE = 30000000

class Directory:
    def __init__(self, name: str, parent) -> None:
        super().__init__()
        self.name = name
        self.parent = parent
        self.children = {}

    def list_all_dirs(self):
        yield self
        for child in self.children.values():
            if isinstance(child, Directory):
                yield from child.list_all_dirs()


    @lru_cache()
    def size(self) -> int:
        total = 0
        for child in self.children.values():
            total += child.size()

        return total

    def sizeceil(self, ceil: int) -> int:
        current_size = self.size()

        total = 0
        if current_size < ceil:
            total += current_size

        for child in self.children.values():
            if isinstance(child, Directory):
                total += child.sizeceil(ceil)

        return total

    def __repr__(self) -> str:
        return f"{self.name} (dir size={self.size()})"

    def __str__(self) -> str:
        child_reprs = '\n'.join(map(str, self.children.values()))
        childlines = child_reprs.splitlines(keepends=False)
        return f"- {self.name} (dir)" + "\n  " + "\n  ".join(childlines)

class File:
    def __init__(self, name: str, size: int) -> None:
        super().__init__()
        self._size: int = size
        self.name: str = name

    def size(self) -> int:
        return self._size

    def __str__(self) -> str:
        return f"- {self.name} (file, size={self._size})"

def build_file_tree(contents) -> Directory:
    root_directory = Directory(name='/', parent=None)
    current_directory = root_directory 
    for line in contents.split('\n')[1:]:
        match line.split():
            case ["$", "ls"]:
                continue
            case ["$", "cd", ".."]:
                current_directory = current_directory.parent
            case ["$", "cd", dirname]:
                current_directory = current_directory.children[dirname]
            case ["dir", dirname]:
                new_directory = Directory(dirname, parent=current_directory)
                assert dirname not in current_directory.children
                current_directory.children[dirname] = new_directory
            case [filesize, filename]:
                new_file = File(filename, int(filesize))
                assert filename not in current_directory.children
                current_directory.children[filename] = new_file

    return root_directory

def part1(filetree: Directory) -> int:
    return filetree.sizeceil(100000)

def part2(filetree: Directory) -> int:
    space_needed = UPDATE_SIZE - (TOTAL_SIZE - filetree.size())
    dirs = sorted(
        filter(
            lambda dir: dir.size() >= space_needed,
            filetree.list_all_dirs()
        ),
        key=Directory.size
    )

    return dirs[0].size()

if __name__ == "__main__":

    with open('test.txt') as f:
        contents = f.read()

    filetree = build_file_tree(contents)

    print('Test')
    print(str(filetree))
    print(part1(filetree))
    print(part2(filetree))


    print('Part 1')
    with open('input.txt') as f:
        contents = f.read()
    filetree = build_file_tree(contents)
    print(part1(filetree))

    print("Part 2")
    print(part2(filetree))


