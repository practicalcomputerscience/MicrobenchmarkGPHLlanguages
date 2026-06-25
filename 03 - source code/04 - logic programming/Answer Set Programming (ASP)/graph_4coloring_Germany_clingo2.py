# -*- coding: utf-8 -*-
"""
graph_4coloring_Germany_clingo2.py

2026-06-25

source: Google AI


run on Ubuntu 24 LTS: $ python3 -m venv ./clingo_ASP
                      $ source ./clingo_ASP/bin/activate
                      $ cd ./clingo_ASP
                      $ python3 ./graph_4coloring_Germany_clingo2.py  # => real	0m1.225s

"""

import subprocess
import json

# 1. Run clingo directly as a system command (Fastest possible way)
# Using "--outf=2" tells clingo to output clean JSON
result = subprocess.run(
    ["clingo", "graph_4coloring_Germany_clingo.lp", "0", "--outf=2"],
    capture_output=True,
    text=True
)

# 2. Parse the output instantly
data = json.loads(result.stdout)

# Extract solutions and count
witnesses = data.get("Call", [])[0].get("Witnesses", [])
total_solutions = len(witnesses)

def print_clean_witness(title, witness_dict):
    print(f"\n🎨 --- {title} ---")
    print("-" * 30)
    
    # Extract the string atoms like "assign(sh,blue)"
    atoms = witness_dict.get("Value", [])
    
    # Sort them by region name
    # Looks at the string after the parenthesis: "assign(sh..." -> sorts by "sh"
    atoms.sort(key=lambda x: x.split("(")[1])
    
    for atom in atoms:
        # Quick string cleanup to extract region and color
        # "assign(sh,yellow)" -> "sh,yellow" -> ["sh", "yellow"]
        parts = atom.replace("assign(", "").replace(")", "").split(",")
        if len(parts) == 2:
            print(f"  📍 Region {parts[0]:<2} ➔ {parts[1]}")
    print("-" * 30)

# 3. Print the results
if total_solutions > 0:
    print(f"📊 Summary: Found {total_solutions:,} total valid colorings.")
    
    # First solution
    print_clean_witness("FIRST SOLUTION", witnesses[0])
    
    # Last solution
    if total_solutions > 1:
        print_clean_witness("LAST SOLUTION", witnesses[-1])
else:
    print("❌ No valid map colorings found.")

# end of graph_4coloring_Germany_clingo2.py
