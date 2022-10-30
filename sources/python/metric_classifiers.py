"""This module contains metric classifiers."""
from abc import ABC

import numpy as np

import base
from cross_validation import RawEstimator
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

    def _get_weights_by_class(self, classes_weights):
        return {
            cl: sum((cw["weight"] for cw in classes_weights
                     if cw["class"] == cl))
            for cl in set(cw["class"] for cw in classes_weights)
        }

    @staticmethod
    def _get_classes_weights(objects, weights):
        return [{"class": o.classcode, "weight": w}
                for o, w in zip(objects, weights)]

    def fit(self, data, **kwargs):
        self._dataset = data

    def predict(self, data):
        weights_table = self.predict_proba(data)
        max_weight = max(weights_table.values())
        return max(weights_table, key=weights_table.get) if max_weight > 0 \
            else None


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

    def predict_proba(self, data):
        sorted_objects = self._sort_objects_by_dist(self._dataset, data)
        n_objects = sorted_objects[:self._n]
        weights = self._weights_calculator.get_weights(n_objects)
        cls_weights = self._get_classes_weights(n_objects, weights)
        result_table = self._get_weights_by_class(cls_weights)
        return result_table


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

    def _get_weights(self, distances):
        return self._weights_calculator.get_weights(
            np.array(list(distances)) / self._h)

    def predict_proba(self, data):
        distances = (self._metric(data, other) for other in self._dataset)
        weights = self._get_weights(distances)
        cls_weights = self._get_classes_weights(self._dataset, weights)
        result_table = self._get_weights_by_class(cls_weights)
        return result_table


class PotentialFunctions(ParzenWindow):

    def __init__(self, h, **kwargs):
        super().__init__(h=h, **kwargs)
        self._point_potentials = None
        self._fitness_thresh = None
        self._max_iter = None
        self._cross_validator = None

    def get_params(self):
        return {
            **super().get_params(),
            "point_potentials": self._point_potentials,
            "fitness_threshold": self._fitness_thresh,
            "fitness_max_iterations": self._max_iter,
            "cross_validator": self._cross_validator
        }

    @staticmethod
    def _select_random_item(dataset):
        random_index = np.random.randint(0, len(dataset))
        return random_index, dataset[random_index]

    def fit(self, data, **kwargs):
        super().fit(data, **kwargs)
        self._fitness_thresh = kwargs.get("fitness_threshold", 0)
        self._max_iter = kwargs.get("fitness_max_iter", 5000)
        self._cross_validator = kwargs.get("cross_validator", RawEstimator())
        self._point_potentials = np.zeros(len(self._dataset))
        iters_count = 0
        error = self._fitness_thresh + 1
        while error > self._fitness_thresh and iters_count < self._max_iter:
            item_index, rand_item = self._select_random_item(data)
            item_class = self.predict(rand_item)
            self._point_potentials[item_index] += \
                (item_class != rand_item.classcode)
            error = self._cross_validator.estimate(self, data)
            print(f"Iter: {iters_count}, error: {error}")
            iters_count += 1
        not_null = self._point_potentials > 0
        self._dataset = [item for i, item in enumerate(self._dataset)
                         if not_null[i]]
        self._h = np.array(self._h)[not_null]
        self._point_potentials = self._point_potentials[not_null]

    def _get_weights(self, distances):
        weights = super()._get_weights(distances)
        return self._point_potentials * np.array(list(weights))
