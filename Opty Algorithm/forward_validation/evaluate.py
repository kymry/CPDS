#!/usr/bin/env python3

from subprocess import check_output
import os.path

def evaluate(cwd, name):
    print("testing", name)
    output = check_output(
        [os.path.join(os.path.abspath("."), "evaluate.escript")],
        cwd=cwd).decode('utf-8')
    # print(output)
    with open(name + ".dat", "w") as f:
        num, sum, last = 0, 0, None
        for s in output.split():
            if s == "Stopped":
                f.write(str(num) + " " + str(sum / num) +"\n")
                print("finished", num)
                num, sum, last = 0, 0, None
            if s == "%":
                num += 1
                sum += float(last)
            last = s


evaluate(".", "forward")
evaluate("..", "backward")
