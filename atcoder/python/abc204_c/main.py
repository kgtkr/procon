import sys
sys.setrecursionlimit(20000)

N, M = map(int, input().split())
AB = [map(int, input().split()) for _ in range(M)]

edges = [[] for _ in range(N)]
for A, B in AB:
    edges[A-1].append(B-1)

def visit(node, visited):
    visited[node] = True
    for next_node in edges[node]:
        if not visited[next_node]:
            visit(next_node, visited)

ans = 0

for start_node in range(N):
    visited = [False] * N
    visit(start_node, visited)
    ans += visited.count(True)

print(ans)
