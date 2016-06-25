import sys
import PIL.Image as Image
import numpy as np
from math import *
from numba import jit

def main():
  n = float(sys.argv[1])
  r = float(sys.argv[2])
  f1 = sys.argv[3]
  f2 = sys.argv[4]
  im = Image.open(f1)
  arr = jrotate(n,np.asarray(im))
  redden(r,arr)
  im = Image.fromarray(arr)
  im.save(f2)

@jit
def redden(r,im):
  m = 1+r/100
  x,y,_ = im.shape
  for i in range(x):
    for j in range(y):
      im[i,j,0] = round(min(im[i,j,0]*m,255))

@jit
def jrotate(deg,im):
  x,y,c = im.shape
  ans = np.empty_like(im)

  cx = float(x)/2
  cy = float(y)/2
  theta = deg*pi/180
  st = sin(theta)
  ct = cos(theta)
  
  for i in range(x):
    for j in range(y):
      fi = float(i)-cx
      fj = float(j)-cy
      j2 = int(round (st * fi + ct * fj + cy))
      i2 = int(round (ct * fi - st * fj + cx))
      if i2<0 or i2>=x or j2<0 or j2>=y:
        ans[i,j,:] = 0
      else:
        ans[i,j,:] = im[i2,j2,:]
  return ans
  
if __name__=="__main__":
  main()
