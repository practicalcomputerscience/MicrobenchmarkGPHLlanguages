# -*- coding: utf-8 -*-
"""
graph_4coloring_Australia_clingo.py

2026-06-25

source: Google AI

run on Ubuntu 24 LTS: $ python3 -m venv ./clingo_ASP
                      $ source ./clingo_ASP/bin/activate
                      $ cd ./clingo_ASP
                      $ python --version  # check version, clingo needs Python version >=3.6
                      Python 3.14.5
                      $ pip3 install clingo
                      $ python3 ./graph_4coloring_Australia_clingo.py
                      📊 Summary: Found 576 total valid colorings.

                      🎨 --- FIRST SOLUTION ---
                        📍 Region nsw ➔ blue
                        📍 Region nt ➔ green
                        📍 Region ql ➔ red
                        📍 Region sa ➔ yellow
                        📍 Region tas ➔ blue
                        📍 Region vic ➔ red
                        📍 Region wa ➔ blue
                      --------------------------

                      🎨 --- LAST SOLUTION ---
                        📍 Region nsw ➔ yellow
                        📍 Region nt ➔ green
                        📍 Region ql ➔ blue
                        📍 Region sa ➔ red
                        📍 Region tas ➔ green
                        📍 Region vic ➔ blue
                        📍 Region wa ➔ yellow
                      -------------------------

                      $

"""

import clingo

# 1. Initialize control with "0" to find ALL solutions
ctl = clingo.Control(["0"])
ctl.load("graph_4coloring_Australia_clingo.lp")
ctl.ground([("base", [])])

# List to store all formatted solutions
all_solutions = []

# 2. Callback function to format and collect models
def on_model(model):
    # Sort symbols alphabetically by region name (e.g., a, b, c...)
    symbols = sorted(model.symbols(shown=True), key=lambda s: str(s.arguments[0]))

    # Format each assignment inside this specific solution
    formatted_solution = []
    for symbol in symbols:
        if symbol.name == "assign":
            region = symbol.arguments[0]
            color = symbol.arguments[1]
            formatted_solution.append(f"  📍 Region {region} ➔ {color}")

    # Combine the lines into a single text block and save it
    all_solutions.append("\n".join(formatted_solution))

# 3. Solve the problem
ctl.solve(on_model=on_model)

# 4. Display the results cleanly
total_solutions = len(all_solutions)

if total_solutions > 0:
    print(f"📊 Summary: Found {total_solutions} total valid colorings.\n")

    print("🎨 --- FIRST SOLUTION ---")
    print(all_solutions[0])
    print("-" * 26 + "\n")

    if total_solutions > 1:
        print("🎨 --- LAST SOLUTION ---")
        print(all_solutions[-1])
        print("-" * 25 + "\n")
else:
    print("❌ No valid map colorings found.")

# end of graph_4coloring_Australia_clingo.py
