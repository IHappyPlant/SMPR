"""Dataset objects weights calculators."""
import abc

from kernels import gaussian_kernel


class DataWeightsCalculator:
    """Abstract class for dataset weights calculators."""

    def get_params(self):
        return {}

    @abc.abstractmethod
    def get_weight(self, item, **kwargs):
        """
        Get item weight.

        :type item: base.DataObject
        :rtype: float
        """
        pass

    def get_weights(self, data):
        """
        Get weight for each dataset item.

        :type data: list|tuple|numpy.ndarray|typing.Generator
        :rtype: typing.Generator[float]
        """
        return (self.get_weight(obj) for obj in data)


class UniformWeightsCalculator(DataWeightsCalculator):
    """
    Weights calculator that assign equal weights
    to all dataset objects.

    :param float w: the same weight for all objects
    """

    def __init__(self, w=1.):
        self._w = w

    def get_params(self):
        return {
            **super().get_params(),
            "w": self._w
        }

    def get_weight(self, item, **kwargs):
        return self._w

    def get_weights(self, dataset):
        return (self.get_weight(obj) for obj in dataset)


class WeightsByOrderCalculator(DataWeightsCalculator):
    """
    Weights calculator that assign weights to data item accordingly
    to its position in dataset using formula: q^position.

    :param float q: param for weight calculation formula: q^position
    """

    def __init__(self, q=0.7):
        self._q = q

    def get_params(self):
        return {
            **super().get_params(),
            "q": self._q
        }

    def get_weight(self, item, **kwargs):
        """
        Get item weight.

        :type item: base.DataObject
        :key index: item integer index in data
        :rtype: float
        """
        return self._q ** kwargs.get("index", 0)

    def get_weights(self, dataset):
        return (self.get_weight(obj, index=i)
                for i, obj in enumerate(dataset))


class KernelWeightsCalculator(DataWeightsCalculator):

    def __init__(self, kernel_func=gaussian_kernel):
        self._kernel = kernel_func

    def get_weight(self, item, **kwargs):
        return self._kernel(item)
