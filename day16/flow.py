from collections import defaultdict
import heapq as hq
from copy import copy

class State:
    def __init__(self) -> None:
        self.position = "AA"
        self.elephant = "AA"
        self.minutes = 30
        self.open = ""
        self.total_flow = 0

    def __lt__(self, other):
        return self.total_flow < other.total_flow

    def max_possible_flow(self, distances, valve_rates) -> int:
        future_potential = 0
        for node, dist in distances[self.position].items():
            if node not in self.open:
                future_potential += (self.minutes - 1) * valve_rates[node]

        return self.total_flow + future_potential

    def __repr__(self) -> str:
        return f"<{self.position}, {self.minutes}, {self.open}, {self.total_flow}>"


def read_input(filename):
    with open(filename) as f:
        contents = f.read()

        valve_to_rate = {}
        neighbors = defaultdict(list)
        for line in contents.split("\n"):
            words = line.split()
            valve = words[1]
            rate = int(words[4][5:-1])
            connects_to = list(map(lambda x: x.strip(","), words[9:]))

            valve_to_rate[valve] = rate
            for other_valve in connects_to:
                neighbors[valve].append(other_valve)

        return valve_to_rate, neighbors


def transitions(state, distances, valve_rates):
    for node, dist in distances[state.position].items():
        if dist + 1 >= state.minutes or valve_rates[node] == 0:
            continue
        if node in state.open:
            continue

        new_state = copy(state)
        new_state.minutes -= dist + 1 # move to valve and open
        new_state.open += "," + node
        new_state.total_flow += new_state.minutes * valve_rates[node]
        new_state.position = node

        yield new_state


def all_pairs_shortest(neighbors) -> dict[str, dict[str, int]]:
    dist = {
        k: {_k:float('inf') for _k in neighbors.keys()}
        for k in neighbors.keys()
    }

    for u in neighbors:
        dist[u][u] = 0
        for v in neighbors[u]:
            dist[u][v] = 1

    for k in neighbors:
        for i in neighbors:
            for j in neighbors:
                if dist[i][j] > dist[i][k] + dist[k][j]:
                    dist[i][j] = dist[i][k] + dist[k][j]

    return dist

def part1(filename, helper=False):
    valve_rates, neighbors = read_input(filename)

    state = State()
    distances = all_pairs_shortest(neighbors)

    heap = [(-state.max_possible_flow(distances, valve_rates), state)]
    best = state
    num_states = 0
    while heap:
        num_states += 1
        state_max_flow, state = hq.heappop(heap)
        if state.total_flow > best.total_flow:
            best = state
        elif (-state_max_flow) < best.total_flow:
            break

        for new_state in transitions(state, distances, valve_rates):
            hq.heappush(heap, (-new_state.max_possible_flow(distances, valve_rates), new_state))

    print(best.total_flow, num_states)


if __name__ == "__main__":
    part1("test.txt")
    part1('input.txt')
