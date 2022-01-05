import numpy as np
import galois

GF = galois.GF(
    21888242871839275222246405745257275088548364400416034343698204186575808495617)

A = GF([
    [340282366920938463463374557953744960808, 1080,
        340282366920938463463374557953744961147, 40],
    [340282366920938463463374557953744932377, 42471,
        340282366920938463463374557953744947017, 1210],
    [340282366920938463463374557953744079447, 1277640,
        340282366920938463463374557953744532108, 33880],
    [340282366920938463463374557953720263017, 35708310,
        340282366920938463463374557953733025977, 925771]
])

print(np.linalg.inv(A))