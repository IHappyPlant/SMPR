"""This module contains cross-validation algorithms."""
import abc

from loss_functions import indicator_loss


class CrossValidator:

    def __init__(self, loss_func=indicator_loss):
        self._loss_func = loss_func
        self._losses = []

    @property
    def losses(self):
        return self._losses

    @property
    def quality(self):
        return sum(self._losses)

    @abc.abstractmethod
    def _estimate(self, model, data):
        pass

    def estimate(self, model, data):
        self._losses = []
        self._estimate(model, data)
        return self.quality


class LeaveOneOut(CrossValidator):

    def __init__(self, **kwargs):
        super().__init__(**kwargs)

    def _estimate(self, model, data):
        for i, item in enumerate(data):
            data_without_item = data[:i] + data[i + 1:]
            model.fit(data_without_item)
            prediction = model.predict(item)
            self._losses.append(self._loss_func(item.classcode, prediction))
