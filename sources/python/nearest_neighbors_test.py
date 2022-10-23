from copy import deepcopy

import numpy as np
import tqdm
from matplotlib import pyplot as plt
from sklearn.datasets import load_iris

from base import DataObject
from cross_validation import LeaveOneOut
from metric_classifiers import KWNN
from weight_calculators import (
    UniformWeightsCalculator,
    WeightsByOrderCalculator
)

CLASS_NUM_STR_MAP = {
    0: "setosa",
    1: "virginica",
    2: "versicolor"
}

COLORS = {
    "setosa": "red",
    "versicolor": "green",
    "virginica": "blue"
}


def prepare_iris(dataset):
    return [DataObject(d[2:], t) for d, t in
            zip(dataset.data, dataset.target)]


def prepare_data():
    return [DataObject([x, y])
            for y in np.arange(0, 3, 0.1) for x in np.arange(0, 7, 0.1)]


def fit_knn(models, xl):
    loo = LeaveOneOut()
    qualities = [loo.estimate(m, xl)
                 for m in tqdm.tqdm(models, desc="Models evaluation: ")]
    return {
        "qualities": qualities,
        "best": models[np.argmin(qualities)]
    }


def display_data(xl, classified):
    plt.scatter([i[0] for i in classified],
                [i[1] for i in classified],
                facecolors="none",
                marker="s",
                edgecolor=[COLORS.get(CLASS_NUM_STR_MAP.get(i.classcode))
                           for i in classified])
    plt.scatter([i[0] for i in xl],
                [i[1] for i in xl],
                c=[COLORS.get(CLASS_NUM_STR_MAP.get(i.classcode)) for i in xl],
                edgecolor="black")
    plt.show()


def display_comparison_data(xl, *classified):
    _, plots = plt.subplots(len(classified), 2)
    for plots_row, classified_ in zip(plots, classified):
        plot = plots_row[0]
        plot.set_title(classified_.get("title"))
        c_data = classified_["data"]
        plot.scatter([i[0] for i in c_data],
                     [i[1] for i in c_data],
                     facecolors="none",
                     marker="s",
                     edgecolor=[COLORS.get(CLASS_NUM_STR_MAP.get(i.classcode))
                                for i in c_data])
        plot.scatter([i[0] for i in xl],
                     [i[1] for i in xl],
                     c=[COLORS.get(CLASS_NUM_STR_MAP.get(i.classcode)) for i in
                        xl],
                     edgecolor="none")

        if classified_.get("cv_data"):
            loo_plot = plots_row[1]
            k_values = np.arange(0,
                                 len(classified_["cv_data"]["qualities"])) + 1
            best_k = classified_["cv_data"]["best_k"]
            best_q = classified_["cv_data"].get("best_q")
            best_quality = classified_["cv_data"]["qualities"][best_k - 1]
            loo_plot.plot(k_values, classified_["cv_data"]["qualities"])
            loo_text = f"k = {best_k}\nerrors = {best_quality}"
            if best_q:
                loo_text += f"\nq = {best_q}"
            loo_plot.scatter([best_k], [best_quality], c=["red"])
            loo_plot.text(best_k, best_quality, loo_text)
    plt.show()


if __name__ == '__main__':
    iris = prepare_iris(load_iris())
    data = prepare_data()

    one_nn = KWNN(n=1)
    one_nn.fit(iris)
    one_nn_test = deepcopy(data)
    for item in one_nn_test:
        prediction = one_nn.predict(item)
        item.classcode = prediction
    # display_data(iris, one_nn_test)

    knns = fit_knn(
        [KWNN(n=n, weights_calculator=UniformWeightsCalculator())
         for n in range(1, len(iris) // 3)],
        iris
    )
    knn = knns["best"]
    knn.fit(iris)
    knn_test = deepcopy(data)
    for item in knn_test:
        prediction = knn.predict(item)
        item.classcode = prediction
    # display_data(iris, knn_test)

    kwnns = fit_knn(
        [KWNN(n=n, weights_calculator=WeightsByOrderCalculator(q=q))
         for n in range(1, len(iris) // 3)
         for q in np.arange(0.1, 1., 0.1)],
        iris
    )
    kwnn = kwnns["best"]
    kwnn.fit(iris)
    kwnn_test = deepcopy(data)
    for item in kwnn_test:
        prediction = kwnn.predict(item)
        item.classcode = prediction
    # display_data(iris, kwnn_test)
    display_comparison_data(
        iris,
        {"title": "1NN", "data": one_nn_test},
        {"title": "KNN", "data": knn_test,
         "cv_data": {
             "qualities": knns["qualities"],
             "best_k": knn.get_params()["n"]
         }},
        {"title": "KWNN", "data": kwnn_test,
         "cv_data": {
             "qualities": kwnns["qualities"],
             "best_k": kwnn.get_params()["n"],
             "best_q": kwnn.get_params()[
                 "weights_calculator"].get_params()["q"]
         }}
    )
