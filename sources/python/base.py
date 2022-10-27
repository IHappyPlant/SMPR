import abc


class DataObject:
    """
    Class that represents data object from dataset.

    :type features: list|tuple|numpy.ndarray
    :type classcode: int|None
    """

    def __init__(self, features, classcode=None):
        self._features = features
        self._classcode = classcode
        self.__index = 0

    @property
    def classcode(self):
        return self._classcode

    @classcode.setter
    def classcode(self, x):
        self._classcode = x

    def __repr__(self):
        return f"Features: {self._features}; Class: {self._classcode}"

    def __getitem__(self, item):
        return self._features[item]

    def __iter__(self):
        self.__index = 0
        return self

    def __next__(self):
        if self.__index < len(self._features):
            item = self._features[self.__index]
            self.__index += 1
            return item
        raise StopIteration

    def __len__(self):
        return len(self._features)


class BasePredictor(abc.ABC):

    def get_params(self):
        return {}

    @abc.abstractmethod
    def fit(self, data):
        """
        :type data: list[DataObject]|tuple[DataObject]|
            numpy.ndarray[DataObject]
        """
        pass

    @abc.abstractmethod
    def predict(self, data):
        """
        :type data: DataObject
        """
        pass

    def fit_predict(self, data, data_object):
        """
        :type data: list[base.DataObject]|tuple[base.DataObject]|
            numpy.ndarray[base.DataObject]
        :type data_object: base.DataObject
        :rtype: int
        """
        self.fit(data)
        return self.predict(data_object)
