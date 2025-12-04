'''
MapColoring_Australia.py

To solve the map coloring problem, which is a constraint satisfaction problem, I have employed backtracking
algorithm. The colors should be assigned to the states considering the constraints (the borders).

2025-11-14 + 2025-12-04 (Windows 11)

source: https://github.com/parisasl/MapColoring/blob/main/MapColoring.py

'''

# Class of states and their possible colors(domain).
class Variable:
    def __init__(self, name, domain):
        self.name = name
        self.domain = domain

# Class of the constraints, which are going to be defined.
class Constraint:
    def __init__(self, variables):
        self.variables = variables

    def check(self, values):
        return True

# Checking whether the domain(color) of the constraints (states with border) are the same.
# If they are same: return False
class DifferentConstraint(Constraint):
    def check(self, values):
        if len(values) == 0:
            return True
        v = None
        for val in values:
            if v is None:
                v = val
            elif val == v:
                return False
        return True

# class EqualConstraint(Constraint):
#     def check(self, values):
#         if len(values) == 0:
#             return True
#         v = values[0]
#         for val in values:
#             if v != val:
#                 return False
#         return True

#selecting the keys(and values) that we are looking for from a dict. (sub_dic)
def filter_dictionary(d, keys):
    return {k: v for (k, v) in d.items() if k in keys}


def dictionary_to_array(d):
    return [v for (k, v) in d.items()]

# Concatenate two dictionaries
def union(d1, d2):
    d = d1.copy()
    d.update(d2)
    return d

# Adding arrays without the duplicates
def union_arr(a, b):
    return list(set(a) | set(b))

colors = ["red","green","blue","yellow"]
states = ["WA","NT","Q","NSW","V","SA","T"]

class Problem:
    def __init__(self):
        self.variables = []
        self.constraints = []

    def add_variable(self, variable):
        self.variables.append(variable)

    def add_constraint(self, constraint):
        self.constraints.append(constraint)

    def check_consistency(self, assignment):
        for constraint in self.constraints:
            relevantValues = filter_dictionary(assignment, constraint.variables)
            if not constraint.check(dictionary_to_array(relevantValues)):
                return False
        return True

    # Finding the answers
    def find(self, assignment, v):
        vars = v.copy()
        if len(vars) == 0:
            return [assignment]

        var = vars.pop()
        results = []
        # For each color in a particular state, we check if it is consistent with previous constraints.
        for option in var.domain:
            new_assignment = union(assignment, {var.name: option})
            if self.check_consistency(new_assignment):
                # The backtracking algorithm
                res = self.find(new_assignment, vars)
                results += res
        return results

    def get_solutions(self):
        return self.find({}, self.variables.copy())

problem = Problem()
# At first we define the states as variables and consider that all the colors(domains) are possible for each state.
for state in states:
    problem.add_variable(Variable(state, colors))

# defining the constraints (the states which have a common border).
problem.add_constraint(DifferentConstraint(["WA", "NT"]))
problem.add_constraint(DifferentConstraint(["WA", "SA"]))
problem.add_constraint(DifferentConstraint(["NT", "SA"]))
problem.add_constraint(DifferentConstraint(["NT", "Q"]))
problem.add_constraint(DifferentConstraint(["SA", "Q"]))
problem.add_constraint(DifferentConstraint(["SA", "NSW"]))
problem.add_constraint(DifferentConstraint(["SA", "V"]))
problem.add_constraint(DifferentConstraint(["Q", "NSW"]))
problem.add_constraint(DifferentConstraint(["V", "NSW"]))
problem.add_constraint(DifferentConstraint(["T", "V"]))  # 2025-11-14 my addition


#Finding the solutions
#print(problem.get_solutions())
#print("possible solutions: " + str(len(problem.get_solutions())))

def main():
    print("number N of different solutions = " + str(len(problem.get_solutions())))

    first_solution = problem.get_solutions()[0]
    last_solution  = problem.get_solutions()[-1]
    print("\n1st solution = "  + str(first_solution))
    print("...")
    print("last solution = " + str(last_solution))

main()

'''
output:
>python MapColoring_Australia.py
number N of different solutions = 576

1st solution = {'T': 'red', 'SA': 'red', 'V': 'green', 'NSW': 'blue', 'Q': 'green', 'NT': 'blue', 'WA': 'green'}
...
last solution = {'T': 'yellow', 'SA': 'yellow', 'V': 'blue', 'NSW': 'green', 'Q': 'blue', 'NT': 'green', 'WA': 'blue'}

>
'''
