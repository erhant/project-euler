#!/usr/bin/env python3

Q = int(input())
beat = {}
for i in range(Q):
  q = [int(x) for x in input().split()]
  if q[0] == 1:
    beat[q[1]] = True;
  else: 
    if ((q[1] - q[2]) in beat) and (q[1] in beat) and ((q[1] + q[2]) in beat):
      print("GG EZ")
    else:
      print("GLHF")