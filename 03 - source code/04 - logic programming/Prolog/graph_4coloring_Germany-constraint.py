# -*- coding: utf-8 -*-
"""
e:/zzz_Scripts/Python/python-constraint/graph_4coloring_Germany-constraint.py

2025-11-22

test on Ubuntu 24 LTS: OK!

install package in your virtual Python environment: $ pip3 install python-constraint2
                                                    $ pip3 list python-constraint2 => 2.4.0
                                                    $ python3 --version => 3.14.0

run in your virtual Python environment:
  $ python3 ./graph_4coloring_Germany-constraint.py
  number N of different solutions = 191808

  1st solution = {'NI': 'yellow', 'HE': 'blue', 'BB': 'blue', 'TH': 'green', 'ST': 'red', 'SN': 'yellow', 'RP': 'yellow', 'BY': 'red', 'SH': 'blue', 'MV': 'green', 'NW': 'green', 'BW': 'green', 'HH': 'green', 'HB': 'blue', 'BE': 'yellow', 'SL': 'blue'}
  ...
  Last solution = {'NI': 'red', 'HE': 'yellow', 'BB': 'yellow', 'TH': 'green', 'ST': 'blue', 'SN': 'red', 'RP': 'red', 'BY': 'blue', 'SH': 'yellow', 'MV': 'green', 'NW': 'blue', 'BW': 'green', 'HH': 'green', 'HB': 'yellow', 'BE': 'red', 'SL': 'yellow'}
  $ 
  
  $ multitime -n 20 python3 ./graph_4coloring_Germany-constraint.py
              Mean        Std.Dev.    Min         Median      Max
  real        0.380       0.007       0.371       0.380       0.408
  user        0.345       0.007       0.332       0.345       0.366
  sys         0.035       0.005       0.024       0.036       0.047

sources:
  https://python-constraint.github.io/python-constraint/index.html
  https://github.com/python-constraint/python-constraint/tree/main
  code based on Google AI answer to prompt: "map coloring problem with python-constraint2"

"""

from constraint import Problem


def main():

    colors = ['red', 'green', 'blue', 'yellow']

    def notEqual(a, b):
        """A helper function to ensure two variables are not equal."""
        return a != b

    regions = ["SH", "MV", "HH", "HB", "NI", "ST", "BE", "BB", "SN", "NW", "HE", "TH", "RP", "SL", "BW", "BY"]

    problem = Problem()

    problem.addVariables(regions, colors)

    problem.addConstraint(notEqual, ("SH", "NI"))
    problem.addConstraint(notEqual, ("SH", "HH"))
    problem.addConstraint(notEqual, ("SH", "MV"))
    problem.addConstraint(notEqual, ("HH", "NI"))
    problem.addConstraint(notEqual, ("MV", "NI"))
    problem.addConstraint(notEqual, ("MV", "BB"))
    problem.addConstraint(notEqual, ("NI", "HB"))
    problem.addConstraint(notEqual, ("NI", "BB"))
    problem.addConstraint(notEqual, ("NI", "ST"))
    problem.addConstraint(notEqual, ("NI", "TH"))
    problem.addConstraint(notEqual, ("NI", "HE"))
    problem.addConstraint(notEqual, ("NI", "NW"))
    problem.addConstraint(notEqual, ("ST", "BB"))
    problem.addConstraint(notEqual, ("ST", "SN"))
    problem.addConstraint(notEqual, ("ST", "TH"))
    problem.addConstraint(notEqual, ("BB", "BE"))
    problem.addConstraint(notEqual, ("BB", "SN"))
    problem.addConstraint(notEqual, ("NW", "HE"))
    problem.addConstraint(notEqual, ("NW", "RP"))
    problem.addConstraint(notEqual, ("SN", "TH"))
    problem.addConstraint(notEqual, ("SN", "BY"))
    problem.addConstraint(notEqual, ("RP", "SL"))
    problem.addConstraint(notEqual, ("RP", "HE"))
    problem.addConstraint(notEqual, ("RP", "BW"))
    problem.addConstraint(notEqual, ("HE", "BW"))
    problem.addConstraint(notEqual, ("HE", "TH"))
    problem.addConstraint(notEqual, ("HE", "BY"))
    problem.addConstraint(notEqual, ("TH", "BY"))
    problem.addConstraint(notEqual, ("BW", "BY"))

    all_solutions = problem.getSolutions()

    print(f"number N of different solutions = {len(all_solutions)}")
    print(f"\n1st solution = {all_solutions[0]}")
    print(f"...")
    print(f"Last solution = {all_solutions[-1]}")


if __name__ == "__main__":
    main()

# end of graph_4coloring_Germany-constraint.py

