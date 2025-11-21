"""Tools defined for agent to use."""

import subprocess
from pathlib import Path


def produce_and_execute_r(rscript: str) -> str:
    """
    Produce an executable R script and write and execute the r script returning stdout and stderr.

    Args:
    rscript: The rscript to execute. This should be correct R.

    """
    rscript_filename = Path("rpred.R")
    rscript_filename.write_text(rscript, encoding="utf-8")

    # Execute the R script and capture stdout and stderr
    result = subprocess.run(  # noqa: S603
        ["Rscript", rscript_filename],  # noqa: S607
        capture_output=True,
        text=True,
        check=False,
    )

    return f"STDOUT:\n{result.stdout}\n\nSTDERR:\n{result.stderr}"
