import math
n = int(input())
print(math.ceil((n-4)/4+1) if n % 2 == 0 else (n-1)//2)