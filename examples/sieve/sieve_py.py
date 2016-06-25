import numpy as np
import sys
from numba import jit

@jit
def count_primes(n):
  v = np.full(n,True,np.bool)
  v[0] = v[1] = False
  for p in range(n):
    if v[p]:
      for k in range(p*p,n,p):
        v[k] = False
  return np.count_nonzero(v)

if __name__=="__main__":
  if len(sys.argv)!=2:
    print("Usage: erasto [n]",file=sys.stderr)
    sys.exit(1)
  n = int(sys.argv[1])
  print("Count = {}".format(count_primes(n)))

