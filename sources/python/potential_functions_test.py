from copy import deepcopy

import numpy as np
from matplotlib import pyplot as plt
from sklearn.datasets import load_iris

from kernels import gaussian_kernel
from metric_classifiers import PotentialFunctions
from testing_utils import (
    prepare_data, prepare_iris
)
from weight_calculators import KernelWeightsCalculator

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


def display_comparison_data(xl, *classified):
    _, plots = plt.subplots(len(classified), 2)
    if len(classified) == 1:
        plots = [plots]
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
            potentials = cv_data["potentials"]
            h = cv_data["h"]
            not_null_data = cv_data["not_null_data"]
            p_map_plot = plots_row[1]
            p_map_plot.scatter([i[0] for i in xl],
                               [i[1] for i in xl],
                               c=[COLORS.get(
                                   CLASS_NUM_STR_MAP.get(i.classcode)) for
                                  i in xl],
                               edgecolor="none")
            for potential, h_, item in zip(potentials, h, not_null_data):
                circle = plt.Circle((item[0], item[1]), h_,
                                    color=COLORS.get(
                                        CLASS_NUM_STR_MAP.get(item.classcode)),
                                    alpha=0.5
                                    )
                p_map_plot.add_patch(circle)

            p_map_plot.set_title(cv_data.get("title", ""))
            p_map_plot.set_xlabel("Petal length")
            p_map_plot.set_ylabel("Petal width")
    plt.show()


if __name__ == '__main__':
    iris = prepare_iris(load_iris())
    data = prepare_data()

    h = np.where(np.array([i.classcode for i in iris]) == 0, 1, 0.4)
    p_f_quartic = PotentialFunctions(
        h=h,
        weights_calculator=KernelWeightsCalculator(gaussian_kernel))
    p_f_quartic.fit(iris, fitness_threshold=7, fitness_max_iter=1000)
    p_f_quartic_test = deepcopy(data)
    for item in p_f_quartic_test:
        prediction = p_f_quartic.predict(item)
        item.classcode = prediction
    p_f_quartic_test = [i for i in p_f_quartic_test if i.classcode is not None]
    params = p_f_quartic.get_params()

    display_comparison_data(
        iris,
        {"title": "Quartic kernel", "data": p_f_quartic_test,
         "cv_data": {
             "title": "Potentials map",
             "potentials": params["point_potentials"],
             "not_null_data": params["dataset"],
             "h": params["h"]
         }}
    )
