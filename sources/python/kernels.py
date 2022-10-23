import math


def gaussian_kernel(r):
    return (2 * math.pi) ** (-0.5) * math.exp(-0.5 * (r ** 2))


def rectangular_kernel(r):
    return 1 / 2 * (abs(r) <= 1)


def triangular_kernel(r):
    return (1 - abs(r)) * (abs(r) <= 1)


def epanechnikov_kernel(r):
    return 3 / 4 * (1 - r**2) * (abs(r) <= 1)


def quartic_kernel(r):
    return 15 / 16 * (1 - r**2) * (abs(r) <= 1)
