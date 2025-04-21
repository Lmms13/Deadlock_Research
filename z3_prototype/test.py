import os
import json
from z3 import *

Levels = DeclareSort('Levels')
bot = Int('bot')
top = Int('top')
value = Function('value', Levels, IntSort())

def add_level_constraint(solver, z3_consts, l1, l2, name):
    if l1 == "top":
        if l2 not in z3_consts:
            z3_consts[l2] = Const(l2, Levels)
        constraint = top < value(z3_consts[l2])
    elif l2 == "top":
        if l1 not in z3_consts:
            z3_consts[l1] = Const(l1, Levels)
        constraint = value(z3_consts[l1]) < top
    elif l1 == "bot":
        if l2 not in z3_consts:
            z3_consts[l2] = Const(l2, Levels)
        constraint = bot < value(z3_consts[l2])
    elif l2 == "bot":
        if l1 not in z3_consts:
            z3_consts[l1] = Const(l1, Levels)
        constraint = value(z3_consts[l1]) < bot
    else:
        if l1 not in z3_consts:
            z3_consts[l1] = Const(l1, Levels)
        if l2 not in z3_consts:
            z3_consts[l2] = Const(l2, Levels)
        constraint = value(z3_consts[l1]) < value(z3_consts[l2])
    
    solver.assert_and_track(constraint, name)  # Track the constraint with a unique name

def check_inequalities(inequalities, file_path):
    solver = Solver()

    # Ensure bot is less than all elements in Levels and top is greater
    solver.add(ForAll([Const('x', Levels)], bot < value(Const('x', Levels))))
    solver.add(ForAll([Const('x', Levels)], top > value(Const('x', Levels))))

    z3_consts = {}
    constraint_map = {}  # Map constraint names to their details

    # Add constraints and track them
    for i, ineq in enumerate(inequalities):
        span = ineq["span"]
        l1 = ineq["l1"]
        l2 = ineq["l2"]
        name = f"constraint_{i}"
        constraint_map[name] = {"span": span, "l1": l1, "l2": l2}
        add_level_constraint(solver, z3_consts, l1, l2, name)

    # Check satisfiability
    if solver.check() == sat:
        return []
    else:
        unsat_core = solver.unsat_core()
        # Map the unsat core back to the original constraints
        return [
            {
                "span": constraint_map[str(c)]["span"],
                "l1": constraint_map[str(c)]["l1"],
                "l2": constraint_map[str(c)]["l2"],
                "file_path": file_path,
            }
            for c in unsat_core
        ]

if __name__ == "__main__":
    script_dir = os.path.dirname(os.path.abspath(__file__))
    json_files = [f for f in os.listdir(script_dir) if f.endswith(".json")]

    for json_file in json_files:
        file_path = os.path.join(script_dir, json_file)

        with open(file_path, "r") as file:
            input_data = file.read()

        inequalities = json.loads(input_data)
        unsat_constraints = check_inequalities(inequalities, file_path)

        # Print unsat constraints in the desired format
        if unsat_constraints:
            for constraint in unsat_constraints:
                span = constraint["span"]
                l1 = constraint["l1"]
                l2 = constraint["l2"]
                file_path = constraint["file_path"]
                # Print only the level values in red
                print(f"Level \033[31m{l1}\033[0m does not precede level \033[31m{l2}\033[0m at {file_path}:{span}")