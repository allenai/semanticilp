""" a helper function to make calls to the offcial evaluation script """
from __future__ import print_function
import sys
from evaluatev11 import *

def evaluate(prediction, ground_truths):
    exact_match = metric_max_over_ground_truths(exact_match_score, prediction, ground_truths)
    f1 = metric_max_over_ground_truths(f1_score, prediction, ground_truths)

    return str(1.0 * exact_match) + "\t" + str(f1)

def main(argv):
    # example run:
    print(evaluate(argv[0], argv[1:]))

if __name__ == '__main__':
    main(sys.argv[1:])
