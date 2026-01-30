"""Test what order Tarjan's algorithm returns SCCs."""

from aifpl.aifpl_dependency_analyzer import AIFPLDependencyAnalyzer

analyzer = AIFPLDependencyAnalyzer()

# Create a simple dependency chain: a -> b -> c
# Expected topological order: c, b, a (or a, b, c depending on direction)
graph = {
    'a': {'b'},  # a depends on b
    'b': {'c'},  # b depends on c
    'c': set(),  # c has no dependencies
}

sccs = analyzer._find_strongly_connected_components(graph)

print("Graph: a -> b -> c")
print(f"SCCs returned: {[list(scc) for scc in sccs]}")
print()

# The correct topological order for compilation is: c, b, a
# (compile dependencies before dependents)
print("For compilation, we need: c before b, b before a")
print()

# Check if sorting is needed
needs_sort = False
for i, scc in enumerate(sccs):
    for name in scc:
        deps = graph.get(name, set())
        # Check if any dependency comes AFTER this SCC
        for j in range(i + 1, len(sccs)):
            later_scc = sccs[j]
            if deps & later_scc:  # If any dependency is in a later SCC
                print(f"ERROR: {name} in SCC {i} depends on {deps & later_scc} in SCC {j}")
                needs_sort = True

if needs_sort:
    print("\n❌ Sorting IS needed - SCCs are not in topological order")
else:
    print("\n✅ Sorting NOT needed - SCCs are already in topological order")
