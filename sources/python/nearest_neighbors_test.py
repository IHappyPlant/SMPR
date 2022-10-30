from copy import deepcopy

import numpy as np
from matplotlib import pyplot as plt
from sklearn.datasets import load_iris

from metric_classifiers import KWNN
from testing_utils import (
    estimate_models, prepare_data, prepare_iris
)
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


def display_data(xl, classified):
    _, plots = plt.subplots(1, 2)
    main_plot, cv_plot = plots[0][0], plots[0][1]

    main_plot.set_title(classified.get("title"))
    c_data = classified["data"]
    main_plot.scatter(
        [i[0] for i in c_data],
        [i[1] for i in c_data],
        facecolors="none",
        marker="s",
        edgecolor=[COLORS.get(CLASS_NUM_STR_MAP.get(i.classcode))
                   for i in c_data])
    main_plot.scatter(
        [i[0] for i in xl],
        [i[1] for i in xl],
        c=[COLORS.get(CLASS_NUM_STR_MAP.get(i.classcode)) for i in xl],
        edgecolor="none")

    if classified.get("cv_data"):
        k_values = np.arange(0,
                             len(classified["cv_data"]["qualities"])) + 1
        best_k = classified["cv_data"]["best_k"]
        best_q = classified["cv_data"].get("best_q")
        best_quality = classified["cv_data"]["qualities"][best_k - 1]
        cv_plot.plot(k_values, classified["cv_data"]["qualities"])
        loo_text = f"k = {best_k}\nerrors = {best_quality}"
        if best_q:
            loo_text += f"\nq = {best_q}"
        cv_plot.scatter([best_k], [best_quality], c=["red"])
        cv_plot.text(best_k, best_quality, loo_text)
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
        plot.set_xlabel("Petal length")
        plot.set_ylabel("Petal width")

        if classified_.get("cv_data"):
            cv_data = classified_["cv_data"]
            qualities = cv_data["qualities"]
            loo_plot = plots_row[1]
            iters = np.arange(0, len(qualities)) + 1
            best_k = cv_data["best_k"]
            best_q = cv_data.get("best_q")
            best_quality = min(qualities)
            loo_plot.plot(iters, qualities)
            loo_text = f"k = {best_k}\nerrors = {best_quality}"
            if best_q:
                loo_text += f"\nq = {best_q}"
            loo_plot.scatter([np.argmin(qualities) + 1], [best_quality],
                             c=["red"])
            loo_plot.text(np.argmin(qualities) + 1, best_quality, loo_text)
            loo_plot.set_xlabel("Iterations")
            loo_plot.set_ylabel("Num of errors")
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

    knns = estimate_models(
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

    kwnns = estimate_models(
        [KWNN(n=n, weights_calculator=WeightsByOrderCalculator(q=q))
         for n in range(1, len(iris) // 3)
         for q in np.linspace(0.1, 1, num=10)],
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
