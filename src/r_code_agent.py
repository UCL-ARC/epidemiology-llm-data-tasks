"""R Code Agent — a CodeAgent that natively executes R code."""

import importlib.resources
import subprocess
import tempfile
from pathlib import Path
from typing import Any

import yaml
from loguru import logger
from smolagents import CodeAgent
from smolagents.local_python_executor import CodeOutput, PythonExecutor
from smolagents.models import Model
from smolagents.tools import Tool

# ---------------------------------------------------------------------------
# R Executor
# ---------------------------------------------------------------------------

_DEFAULT_MAX_OUTPUT_LEN = 50_000


class RExecutor(PythonExecutor):
    """Execute R code via ``Rscript`` while satisfying the smolagents executor API."""

    def __init__(
        self,
        max_print_outputs_length: int = _DEFAULT_MAX_OUTPUT_LEN,
        timeout: int = 300,
    ) -> None:
        """
        Initialise the R executor.

        Args:
            max_print_outputs_length: Maximum character length for captured output.
            timeout: Maximum seconds to wait for an R script to finish.

        """
        self.max_print_outputs_length = max_print_outputs_length
        self.timeout = timeout
        # smolagents inspects these attributes during execution
        self.state: dict[str, Any] = {"_print_outputs": ""}
        self.custom_tools: dict[str, Tool] = {}

    def send_tools(self, tools: dict[str, Tool]) -> None:
        """Store tools (not used directly — R code runs standalone)."""
        self.custom_tools = tools

    def send_variables(self, variables: dict[str, Any]) -> None:
        """No-op — state is managed within R scripts."""

    def __call__(self, code_action: str) -> CodeOutput:
        """Write *code_action* to a temp ``.R`` file, execute it, and return output."""
        is_final_answer = "final_answer" in code_action

        # Strip the sentinel so Rscript doesn't choke on it
        code_to_run = code_action.replace('final_answer("done")', "")
        code_to_run = code_to_run.replace("final_answer('done')", "")

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".R", delete=False, encoding="utf-8"
        ) as tmp:
            tmp.write(code_to_run)
            script_path = Path(tmp.name)

        try:
            result = subprocess.run(  # noqa: S603
                ["Rscript", str(script_path)],  # noqa: S607
                capture_output=True,
                text=True,
                check=False,
                timeout=self.timeout,
            )

            logs = result.stdout or ""
            if result.stderr:
                logs += "\nSTDERR:\n" + result.stderr

            if len(logs) > self.max_print_outputs_length:
                logs = logs[: self.max_print_outputs_length] + "\n...(truncated)"

            self.state["_print_outputs"] = logs

            if result.returncode != 0:
                msg = f"R script failed (exit {result.returncode}):\n{logs}"
                raise RuntimeError(msg)

            stdout = result.stdout.strip() if result.stdout else ""
            return CodeOutput(
                output=stdout or "R script completed.",
                logs=logs,
                is_final_answer=is_final_answer,
            )
        finally:
            script_path.unlink(missing_ok=True)


# ---------------------------------------------------------------------------
# R-specific prompt template
# ---------------------------------------------------------------------------

# Start from the default CodeAgent prompts so we keep planning / managed-agent
# sections intact, and only override the system_prompt.
_DEFAULT_PROMPTS: dict[str, Any] = yaml.safe_load(
    importlib.resources.files("smolagents.prompts")
    .joinpath("code_agent.yaml")
    .read_text()
)

_R_SYSTEM_PROMPT = """\
You are an expert assistant who solves tasks by writing R code.

To solve the task you must plan forward in a cycle of **Thought → Code → \
Observation** sequences.

* In the **Thought** section explain your reasoning.
* In the **Code** section write valid R inside ``<code>`` / ``</code>`` tags.
* Use ``cat()`` or ``print()`` for any intermediate values you need — they \
will appear in the **Observation** for the next step.
* When the task is finished, write one last code block containing only: \
``final_answer("done")``

Example
-------
Task: "Read data/input/raw.csv, remove rows with missing values, save to \
data/output/cleaned_data.csv"

Thought: I will read the CSV, drop NAs, and write the cleaned version.
<code>
library(readr)
raw <- read_csv("data/input/raw.csv")
cat("Original rows:", nrow(raw), "\\n")
cleaned <- na.omit(raw)
cat("Cleaned rows:", nrow(cleaned), "\\n")
write_csv(cleaned, "data/output/cleaned_data.csv")
cat("Saved cleaned data\\n")
</code>
Observation: Original rows: 1000
Cleaned rows: 950
Saved cleaned data

Thought: The data has been cleaned and saved.
<code>
final_answer("done")
</code>

Rules
-----
1. Always provide a **Thought:** then a ``<code>`` block closed with \
``</code>``.
2. Write **R code only** — never Python.
3. Load packages with ``library()`` (e.g. haven, dplyr, readr, tidyr).
4. Use ``cat()`` / ``print()`` to surface intermediate results.
5. Do not give up — you are in charge of solving the task.
6. Signal completion with ``final_answer("done")``.

{%- if custom_instructions %}
{{ custom_instructions }}
{%- endif %}

Now Begin!"""

_R_PROMPT_TEMPLATES: dict[str, Any] = {
    **_DEFAULT_PROMPTS,
    "system_prompt": _R_SYSTEM_PROMPT,
}


# ---------------------------------------------------------------------------
# Factory
# ---------------------------------------------------------------------------


def create_r_code_agent(
    model: Model,
    tools: list[Tool] | None = None,
    timeout: int = 300,
    *,
    stream_outputs: bool = False,
    return_full_result: bool = False,
) -> CodeAgent:
    """
    Create a :class:`CodeAgent` configured to execute R code.

    Args:
        model: The LLM model to use.
        tools: Optional list of smolagents ``Tool`` objects.
        timeout: Max seconds per R script execution.
        stream_outputs: Whether to stream LLM outputs.
        return_full_result: Whether to return the full RunResult.

    Returns:
        A ``CodeAgent`` wired with an R executor and R-specific prompts.

    """
    r_executor = RExecutor(timeout=timeout)
    logger.info("Creating R CodeAgent with RExecutor")
    return CodeAgent(
        model=model,
        tools=tools or [],
        executor=r_executor,
        prompt_templates=_R_PROMPT_TEMPLATES,
        stream_outputs=stream_outputs,
        return_full_result=return_full_result,
    )
