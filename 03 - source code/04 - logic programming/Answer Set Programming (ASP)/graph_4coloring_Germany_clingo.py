# -*- coding: utf-8 -*-
"""
graph_4coloring_Germany_clingo.py

2026-06-25

sources:
  https://github.com/aluriak/clyngor -- Handy python wrapper around Potassco's Clingo ASP solver.
  Google AI


run on Ubuntu 24 LTS: $ python3 -m venv ./clingo_ASP
                      $ source ./clingo_ASP/bin/activate
                      $ cd ./clingo_ASP
                      $ python --version  # check version, clingo needs Python version >=3.6
                      Python 3.14.5
                      $ pip3 install clingo
                      $ pip3 install clyngor
                      $ python3 ./graph_4coloring_Germany_clingo.py  # => real	0m5.888s

"""

from clyngor import solve

# 1. Stream solutions from the ASP file
answers = solve('graph_4coloring_Germany_clingo.lp')

# Helper function to print a single answer cleanly
def print_clean_solution(title, raw_frozenset):
    print(f"\n🎨 --- {title} ---")
    print("-" * 30)

    # Extract tuples from the frozenset and sort alphabetically by region name
    # Element format: ('assign', ('region', 'color'))
    assignments = sorted(list(raw_frozenset), key=lambda x: x[1][0])

    for atom, (region, color) in assignments:
        if atom == 'assign':
            print(f"  📍 Region {region:<2} ➔ {color}")
    print("-" * 30)

# Track counts and hold onto the last seen element
total_solutions = 0
last_solution = None

print("⏳ Streaming and counting solutions...")

# 2. Iterate through the generator (instantaneous, memory-efficient)
for answer in answers:
    total_solutions += 1

    # Handle the first solution immediately
    if total_solutions == 1:
        print_clean_solution("FIRST SOLUTION", answer)

    # Continuously overwrite to keep track of the final solution
    last_solution = answer

# 3. Handle the last solution and summary after the stream finishes
if total_solutions > 1:
    print_clean_solution("LAST SOLUTION", last_solution)

print(f"\n📊 Summary: Processed {total_solutions:,} total valid colorings.")

# end of graph_4coloring_Germany_clingo.py
