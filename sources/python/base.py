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
