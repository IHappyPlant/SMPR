"""This module contains metric classifiers."""
from abc import ABC

import base
from metrics import euclidean
from weight_calculators import (
    KernelWeightsCalculator, UniformWeightsCalculator, WeightsByOrderCalculator
)


class MetricClassifier(base.BasePredictor, ABC):
    """
    Base class for all metric classifiers.

    :param typing.Callable metric: callable metric function to
        calculate metric between dataset objects.
    """

    def __init__(self, metric=euclidean,
                 weights_calculator=UniformWeightsCalculator(), **kwargs):
        self._metric = metric
        self._weights_calculator = weights_calculator
        self._dataset = None

    def get_params(self):
        return {
            **super().get_params(),
            "metric": self._metric,
            "weights_calculator": self._weights_calculator,
            "dataset": self._dataset
        }

    def fit(self, data):
        self._dataset = data


class KWNN(MetricClassifier):
    """
    K Weighted Nearest Neighbors classifier.

    :param n: number of nearest neighbors to use.
    :type n: int
    :param weights_calculator: instance of weights calculator object
    :type weights_calculator: weights_calculators.DataWeightsCalculator
    """

    def __init__(self, n=1, weights_calculator=WeightsByOrderCalculator(),
                 **kwargs):
        super().__init__(weights_calculator=weights_calculator, **kwargs)
        self._n = n

    def get_params(self):
        return {
            **super().get_params(),
            "n": self._n
        }

    def _sort_objects_by_dist(self, objects, obj):
        """
        Sort objects list by their distances to obj.

        :type objects: typing.Iterable[base.DataObject]
        :type obj: base.DataObject
        :rtype: list[base.DataObject]
        """
        return sorted(objects, key=lambda x: self._metric(x, obj))

    def predict(self, data):
        sorted_objects = self._sort_objects_by_dist(self._dataset, data)
        n_objects = sorted_objects[:self._n]
        weights = self._weights_calculator.get_weights(n_objects)
        cls_weights = [{
            "class": o.classcode,
            "weight": w
        } for o, w in zip(n_objects, weights)]
        result_table = {
            cl: sum((cw["weight"] for cw in cls_weights if cw["class"] == cl))
            for cl in set((obj.classcode for obj in n_objects))
        }
        return max(result_table, key=result_table.get)


class ParzenWindow(MetricClassifier):

    def __init__(self, h, weights_calculator=KernelWeightsCalculator(),
                 **kwargs):
        super().__init__(weights_calculator=weights_calculator, **kwargs)
        self._h = h

    def get_params(self):
        return {
            **super().get_params(),
            "h": self._h
        }

    def predict(self, data):
        self._h = 1
        distances = (self._metric(data, other) for other in self._dataset)
        weights = (self._weights_calculator.get_weights(
            (d / self._h for d in distances)))
        cls_weights = [{
            "class": o.classcode,
            "weight": w
        } for o, w in zip(self._dataset, weights)]
        res_table = {
            cl: sum((cw["weight"] for cw in cls_weights if cw["class"] == cl))
            for cl in set((obj.classcode for obj in self._dataset))
        }
        max_weight = max(res_table.values())
        return max(res_table, key=res_table.get) if max_weight > 0 else None
