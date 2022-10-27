from copy import deepcopy

import numpy as np
from matplotlib import pyplot as plt
from sklearn.datasets import load_iris

from kernels import (
    epanechnikov_kernel, gaussian_kernel, quartic_kernel, rectangular_kernel,
    triangular_kernel
)
from metric_classifiers import ParzenWindow
from testing_utils import (
    estimate_models,
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
            best_h = cv_data["best_h"]
            best_quality = min(qualities)
            loo_plot.plot(iters, qualities)
            loo_text = f"h = {best_h}\nerrors = {best_quality}"
            loo_plot.scatter([np.argmin(qualities) + 1], [best_quality],
                             c=["red"])
            loo_plot.text(np.argmin(qualities) + 1, best_quality, loo_text)
            loo_plot.set_xlabel("Iterations")
            loo_plot.set_ylabel("Num of errors")
    plt.show()


if __name__ == '__main__':
    iris = prepare_iris(load_iris())
    data = prepare_data()

    p_w_rect_models = estimate_models(
        [ParzenWindow(
            h=h,
            weights_calculator=KernelWeightsCalculator(rectangular_kernel))
            for h in np.arange(0.1, 2, 0.1)],
        iris
    )
    p_w_rect = p_w_rect_models["best"]
    p_w_rect.fit(iris)
    p_w_rect_test = deepcopy(data)
    for item in p_w_rect_test:
        prediction = p_w_rect.predict(item)
        item.classcode = prediction
    p_w_rect_test = [i for i in p_w_rect_test if i.classcode is not None]

    p_w_triang_models = estimate_models(
        [ParzenWindow(
            h=h,
            weights_calculator=KernelWeightsCalculator(triangular_kernel))
            for h in np.arange(0.1, 2, 0.1)],
        iris
    )
    p_w_triang = p_w_triang_models["best"]
    p_w_triang.fit(iris)
    p_w_triang_test = deepcopy(data)
    for item in p_w_triang_test:
        prediction = p_w_triang.predict(item)
        item.classcode = prediction
    p_w_triang_test = [i for i in p_w_triang_test if i.classcode is not None]

    p_w_ep_models = estimate_models(
        [ParzenWindow(
            h=h,
            weights_calculator=KernelWeightsCalculator(epanechnikov_kernel))
            for h in np.arange(0.1, 2, 0.1)],
        iris
    )
    p_w_ep = p_w_ep_models["best"]
    p_w_ep.fit(iris)
    p_w_ep_test = deepcopy(data)
    for item in p_w_ep_test:
        prediction = p_w_ep.predict(item)
        item.classcode = prediction
    p_w_ep_test = [i for i in p_w_ep_test if i.classcode is not None]

    p_w_quartic_models = estimate_models(
        [ParzenWindow(
            h=h,
            weights_calculator=KernelWeightsCalculator(quartic_kernel))
            for h in np.arange(0.1, 2, 0.1)],
        iris
    )
    p_w_quartic = p_w_quartic_models["best"]
    p_w_quartic.fit(iris)
    p_w_quartic_test = deepcopy(data)
    for item in p_w_quartic_test:
        prediction = p_w_ep.predict(item)
        item.classcode = prediction
    p_w_quartic_test = [i for i in p_w_quartic_test if i.classcode is not None]

    p_w_gauss_models = estimate_models(
        [ParzenWindow(
            h=h,
            weights_calculator=KernelWeightsCalculator(gaussian_kernel))
            for h in np.arange(0.1, 2, 0.1)],
        iris
    )
    p_w_gauss = p_w_gauss_models["best"]
    p_w_gauss.fit(iris)
    p_w_gauss_test = deepcopy(data)
    for item in p_w_gauss_test:
        prediction = p_w_gauss.predict(item)
        item.classcode = prediction
    p_w_gauss_test = [i for i in p_w_gauss_test if i.classcode is not None]

    display_comparison_data(
        iris,
        {"title": "Rectangular kernel", "data": p_w_rect_test,
         "cv_data": {
             "qualities": p_w_rect_models["qualities"],
             "best_h": p_w_rect.get_params()["h"]
         }},
        {"title": "Triangular kernel", "data": p_w_triang_test,
         "cv_data": {
             "qualities": p_w_triang_models["qualities"],
             "best_h": p_w_triang.get_params()["h"]
         }},
        {"title": "Epanechnikov kernel", "data": p_w_ep_test,
         "cv_data": {
             "qualities": p_w_ep_models["qualities"],
             "best_h": p_w_ep.get_params()["h"]
         }},
        {"title": "Quartic kernel", "data": p_w_quartic_test,
         "cv_data": {
             "qualities": p_w_quartic_models["qualities"],
             "best_h": p_w_quartic.get_params()["h"]
         }},
        {"title": "Gaussian kernel", "data": p_w_gauss_test,
         "cv_data": {
             "qualities": p_w_gauss_models["qualities"],
             "best_h": p_w_gauss.get_params()["h"]
         }},
    )
