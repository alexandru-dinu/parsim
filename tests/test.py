import ast
import subprocess
import tempfile

from hypothesis import given, settings
from hypothesis import strategies as st

NUM_VALUES = 2
OPS = ["+", "-", "*"]
MIN_VALUE = 0
MAX_VALUE = 100


@st.composite
def expr_gen(draw):
    xs = []
    op = []

    for _ in range(NUM_VALUES):
        xs.append(draw(st.integers(min_value=MIN_VALUE, max_value=MAX_VALUE)))

    for _ in range(NUM_VALUES - 1):
        op.append(draw(st.sampled_from(OPS)))

    out = xs + op
    out[::2] = xs
    out[1::2] = op

    return " ".join(map(str, out))


def safe_eval(expr: str):
    return eval(compile(ast.parse(expr, mode="eval"), "<node>", "eval"), {}, {})


@settings(max_examples=50, deadline=None)
@given(expr_gen())
def test_expression(expr):
    with tempfile.NamedTemporaryFile("wt") as f:
        f.write(f"return {expr};")
        f.flush()
        result = subprocess.run(
            f"stack run monadic-parser-exe {f.name}",
            shell=True,
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
        )

    actual = result.stdout.decode().strip()
    expected = safe_eval(expr)
    if expected < 0:
        assert actual == f"P (Right ({expected}))"
    else:
        assert actual == f"P (Right {expected})"
