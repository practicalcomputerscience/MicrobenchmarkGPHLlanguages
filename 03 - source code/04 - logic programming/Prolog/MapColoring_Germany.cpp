/*
MapColoring_Germany.cpp

2025-11-16

test on Ubuntu 24 LTS, gcc v.13.3.0: OK!

build like this: $ g++ ./MapColoring_Germany.cpp  -o MapColoring_Germany_cpp
                 $ g++ -O3 ./MapColoring_Germany.cpp  -o MapColoring_Germany_cpp

run like this:   $ ./MapColoring_Germany_cpp

                 without -O3:
                 $ sudo perf stat -r 20 ./MapColoring_Germany_cpp
                 ...
                 0,131745 +- 0,000508 seconds time elapsed  ( +-  0,39% )

                 with -O3:
                 $ sudo perf stat -r 20 ./MapColoring_Germany_cpp
                 ...
                 0,033907 +- 0,000422 seconds time elapsed  ( +-  1,24% )


This program is based on this MS Bing AI prompt:
"C/C++ program for the map coloring problem with backtracking solution, counting all solutions"


$ gcc --version
gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0
...
$

*/

#include <iostream>
#include <vector>
#include <string>
#include <stdexcept>

using namespace std;

// Function to check if the current color assignment is safe
bool isSafe(int node, const vector<vector<int>>& graph, const vector<int>& colors, int color) {
    for (size_t i = 0; i < graph.size(); i++) {
        if (graph[node][i] && colors[i] == color) {
            return false; // Adjacent node has the same color
        }
    }
    return true;
}

// Recursive backtracking function to color the map
void solveMapColoring(int node, const vector<vector<int>>& graph,
                      int m, vector<int>& colors,
                      int& solutionCount,
                      vector<vector<int>>& total_sol_vec) {
    int n = graph.size();

    // If all nodes are assigned a color, we found a valid solution
    if (node == n) {
        solutionCount++;

        // store solution dynamically:
        total_sol_vec.push_back(colors);

        // cout << "Solution #" << solutionCount << ": ";
        // for (int c : colors) cout << c << ",";
        // cout << endl;

        return;
    }

    // Try assigning each color from 1 to m
    for (int col = 1; col <= m; col++) {
        if (isSafe(node, graph, colors, col)) {
            colors[node] = col; // Assign color
            solveMapColoring(node + 1, graph, m, colors, solutionCount, total_sol_vec);
            colors[node] = 0; // Backtrack
        }
    }
}


// State colors: function to translate 1,2,3,4 into "red","blue","green","yellow":
string color_names(int color) {
    switch (color) {
        case 1: return "red";
        case 2: return "blue";
        case 3: return "green";
        case 4: return "yellow";
        default: return "invalid"; // Handle unexpected numbers
    }
}



int main() {
    try {
        int n = 16;  // number of regions (nodes)
        int m = 4;   // number of colors

        // vector<vector<int>> graph(n, vector<int>(n, 0));
        // cout << "Enter adjacency matrix (" << n << "x" << n << "):\n";
        // for (int i = 0; i < n; i++) {
        //     for (int j = 0; j < n; j++) {
        //         if (!(cin >> graph[i][j]) || (graph[i][j] != 0 && graph[i][j] != 1))
        //             throw invalid_argument("Adjacency matrix must contain only 0 or 1.");
        //     }
        // }
        // German states:
        // "SH", "MV", "HH", "HB", "NI", "ST", "BE", "BB", "SN", "NW", "HE", "TH", "RP", "SL", "BW", "BY"
        // 0     1     2     3     4     5     6     7     8     9     10    11    12    13    14    15
        // adjacency matrix (16x16):
        //   0 = S1 and S1 are no neighbors
        //   1 = S1 and S1 are neighbors
        vector<vector<int>> graph =
            {{0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0},  // SH ok 3
             {1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0},  // MV ok 3
             {1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0},  // HH ok 2
             {0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0},  // HB ok 1
             {1,1,1,1,0,1,0,1,0,1,1,1,0,0,0,0},  // NI ok 9
             {0,0,0,0,1,0,0,1,1,0,0,1,0,0,0,0},  // ST ok 4
             {0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0},  // BE ok 1
             {0,1,0,0,1,1,1,0,1,0,0,0,0,0,0,0},  // BB ok 5
             {0,0,0,0,0,1,0,1,0,0,0,1,0,0,0,1},  // SN ok 4
             {0,0,0,0,1,0,0,0,0,0,1,0,1,0,0,0},  // NW ok 3
             {0,0,0,0,1,0,0,0,0,1,0,1,1,0,1,1},  // HE ok 6
             {0,0,0,0,1,1,0,0,1,0,1,0,0,0,0,1},  // TH ok 5
             {0,0,0,0,0,0,0,0,0,1,1,0,0,1,1,0},  // RP ok 4
             {0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0},  // SL ok 1
             {0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,1},  // BW ok 3
             {0,0,0,0,0,0,0,0,1,0,1,1,0,0,1,0}}; // BY ok 4
             // 29 x 2 = 58 x "1"

        vector<int> colors(n, 0);
        int solutionCount = 0;

        vector<vector<int>> total_sol_vec;  // keep track of all solutions like in Prolog with findall()

        solveMapColoring(0, graph, m, colors, solutionCount, total_sol_vec);

        cout << "Total number of valid solutions: " << solutionCount << endl;
        if (solutionCount == 0) {
            cout << "No valid coloring possible with " << m << " colors.\n";
            return 0;
        }

        /////////////////////////////////////////////////
        // like in Prolog, print first and last solution:
        //
        try {
          // Check if the matrix is not empty
          if (total_sol_vec.empty()) {
            cout << "Solution matrix is empty. No rows to print.\n";
            return 1;
          }
        } catch (const exception& e) {
            cerr << "Error: " << e.what() << endl;
            return 1;
        }

        cout << "\n               SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY" << endl;

        const vector<int>& firstRow = total_sol_vec.at(0);
        cout << "1st solution = ";
        for (int c : firstRow) {  // don't print last comma like in Prolog
            if (c != firstRow.back()) cout << color_names(c) << ",";
        }
        cout << color_names(firstRow.back());

        // is there at least a second solution?
        if (total_sol_vec.size() > 1) {
           const vector<int>& lastRow = total_sol_vec.back();
           cout << "\n...\nLast solution = ";
           for (int c : lastRow) {
               if (c != lastRow.back()) cout << color_names(c) << ",";
           }
           cout << color_names(lastRow.back());
        }

        cout << endl;

    } catch (const exception& e) {
        cerr << "Error: " << e.what() << endl;
        return 1;
    }

    return 0;
}

// end of MapColoring_Germany.cpp
