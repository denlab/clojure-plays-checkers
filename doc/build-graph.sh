#!/bin/bash -xe

# ---------------------------------------------------------------------------
# Build the graph image from the .dot file
# ---------------------------------------------------------------------------

dot -Tpng function-graph.dot -o function-graph.png
