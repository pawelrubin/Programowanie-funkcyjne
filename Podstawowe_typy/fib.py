import time

def LinearFibonacci(n):
  a, b = 0, 1
  for _ in range(n):
    a, b = b, a + b
  return a

start = time.time()
print(LinearFibonacci(213769))
end = time.time()
print(end-start)