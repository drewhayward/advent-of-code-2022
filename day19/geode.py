from copy import copy
from typing import Generator
from pprint import pprint
from dataclasses import dataclass
from functools import lru_cache


@dataclass
class Blueprint:
    ore_robot_cost_ore: int
    clay_robot_cost_ore: int
    obsidian_robot_cost_ore: int
    obsidian_robot_cost_clay: int
    geode_robot_cost_ore: int
    geode_robot_cost_obsidian: int

@dataclass(unsafe_hash=True)
class State:
    ore: int = 0
    clay: int = 0
    obsidian: int = 0
    geodes: int = 0
    ore_robots: int = 1
    clay_robots: int = 0
    obsidian_robots: int = 0
    geode_robots: int = 0
    minutes = 0

def transition(state: State, blueprint: Blueprint) -> Generator[State, None, None]:
    base_state = copy(state)
    base_state.ore += base_state.ore_robots
    base_state.clay += base_state.clay_robots
    base_state.obsidian += base_state.obsidian_robots
    base_state.geodes += base_state.geode_robots
    base_state.minutes += 1
    yield base_state

    # Yield options to build robots
    if state.ore >= blueprint.ore_robot_cost_ore:
        robot_state = copy(base_state)
        robot_state.ore -= blueprint.ore_robot_cost_ore
        robot_state.ore_robots += 1
        yield robot_state
        
    if state.ore >= blueprint.clay_robot_cost_ore:
        robot_state = copy(base_state)
        robot_state.ore -= blueprint.clay_robot_cost_ore
        robot_state.clay_robots += 1
        yield robot_state

    if state.ore >= blueprint.obsidian_robot_cost_ore and state.clay >= blueprint.obsidian_robot_cost_clay:
        robot_state = copy(base_state)
        robot_state.ore -= blueprint.obsidian_robot_cost_ore
        robot_state.clay -= blueprint.obsidian_robot_cost_clay
        robot_state.obsidian_robots += 1
        yield robot_state

    if state.ore >= blueprint.geode_robot_cost_ore and state.obsidian >= blueprint.geode_robot_cost_obsidian:
        robot_state = copy(base_state)
        robot_state.ore -= blueprint.geode_robot_cost_ore
        robot_state.obsidian -= blueprint.geode_robot_cost_obsidian
        robot_state.geode_robots += 1
        yield robot_state


def read_input(filename) -> list[Blueprint]:
    with open(filename) as f:
        blueprints = []
        for line in f.read().splitlines(keepends=False):
            words = line.split()
            blueprints.append(
                Blueprint(
                    int(words[6]),
                    int(words[12]),
                    int(words[18]),
                    int(words[21]),
                    int(words[27]),
                    int(words[30]),
                )
            )

        return blueprints


def max_geodes(blueprint: Blueprint, max_minutes: int = 24):

    frontier: list[State] = [State()]
    visited = set()
    most_geodes = 0
    while frontier:
        if (node := frontier.pop()) in visited:
            continue
        else:
            visited.add(node)

        if node.minutes >= max_minutes:
            most_geodes = max(most_geodes, node.geodes)
        else:
            frontier.extend(transition(node, blueprint))
        
    print(most_geodes)

def part1(filename):
    blueprints = read_input(filename)

    print(max_geodes(blueprints[0]))


if __name__ == "__main__":
    part1("test.txt")
