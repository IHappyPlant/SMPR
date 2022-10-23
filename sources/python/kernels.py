import math


def gaussian_kernel(r):
    return (2 * math.pi) ** (-0.5) * math.exp(-0.5 * (r ** 2))
