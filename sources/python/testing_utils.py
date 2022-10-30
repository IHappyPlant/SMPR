import numpy as np
from tqdm import tqdm

from base import DataObject
from cross_validation import LeaveOneOut


def prepare_iris(dataset):
    return [DataObject(d[2:], t) for d, t in
            zip(dataset.data, dataset.target)]


def prepare_data():
    return [DataObject([x, y])
            for y in np.arange(0, 3, 0.1) for x in np.arange(0, 7, 0.1)]


def estimate_models(models, xl, cross_validator=LeaveOneOut()):
    qualities = [cross_validator.estimate(m, xl)
                 for m in tqdm(models, desc="Models evaluation: ")]
    return {
        "qualities": qualities,
        "best": models[np.argmin(qualities)]
    }
