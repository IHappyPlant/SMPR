"""This module contains metric classifiers."""
import abc

from metrics import euclidean
from weight_calculators import WeightsByOrderCalculator


class MetricClassifier:
    """
    Base class for all metric classifiers.

    :param typing.Callable metric: callable metric function to
        calculate metric between dataset objects.
    """

    def __init__(self, metric=euclidean, **kwargs):
        self._metric = metric

    def get_params(self):
        return {
            "metric": self._metric
        }

    @abc.abstractmethod
    def fit(self, dataset):
        """
        :type dataset: list[base.DataObject]|tuple[base.DataObject]|
            numpy.ndarray[base.DataObject]
        """
        pass

    @abc.abstractmethod
    def predict(self, obj):
        """
        :type obj: base.DataObject
        :rtype: int
        """
        pass

    def fit_predict(self, dataset, obj):
        """
        :type dataset: list[base.DataObject]|tuple[base.DataObject]|
            numpy.ndarray[base.DataObject]
        :type obj: base.DataObject
        :rtype: int
        """
        self.fit(dataset)
        return self.predict(obj)


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
        super().__init__(**kwargs)
        self._dataset = None
        self._n = n
        self._weights_calculator = weights_calculator

    def get_params(self):
        return {
            **super().get_params(),
            "n": self._n,
            "weights_calculator": self._weights_calculator
        }

    def _sort_objects_by_dist(self, objects, obj):
        """
        Sort objects list by their distances to obj.

        :type objects: typing.Iterable[base.DataObject]
        :type obj: base.DataObject
        :rtype: list[base.DataObject]
        """
        return sorted(objects, key=lambda x: self._metric(x, obj))

    def fit(self, dataset):
        self._dataset = dataset

    def predict(self, obj):
        sorted_objects = self._sort_objects_by_dist(self._dataset, obj)
        n_objects = sorted_objects[:self._n]
        weights = self._weights_calculator.get_weights(n_objects)
        classes_weights = ({
            "class": o.classcode,
            "weight": w
        } for o, w in zip(n_objects, weights))
        unique_classes = set((obj.classcode for obj in n_objects))
        result_table = {
            cls: sum(
                (cw["weight"] for cw in classes_weights if cw["class"] == cls))
            for cls in unique_classes
        }
        return max(result_table, key=result_table.get)
