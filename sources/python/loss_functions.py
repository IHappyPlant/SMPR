"""This module contains loss functions for algorithms."""


def indicator_loss(gt, prediction):
    return gt != prediction
