"""This module contains distance metrics for all other algorithms."""
import math


def euclidean(x, y):
    """
    :type x: typing.Iterable[int|float]
    :type y: typing.Iterable[int|float]
    """
    return math.sqrt(sum((x_ - y_) ** 2 for x_, y_ in zip(x, y)))
