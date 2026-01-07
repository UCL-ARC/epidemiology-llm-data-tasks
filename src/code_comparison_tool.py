"""Tool to compare two code snippets using linters and LLMs."""

import subprocess
from enum import Enum
from pathlib import Path

from loguru import logger
from smolagents import LiteLLMModel  # just using this for consistency
from smolagents.models import ChatMessage

from src.rubrics import CodeQualityAssessment, CodeQualityRubric


class AvailableLanguages(Enum):
    """Supported programming languages for code comparison."""

    R = "R"


class AvailableLinters(Enum):
    """Supported linters for code comparison."""

    LINTR = "lintr"


class CodeComparisonTool:
    """Tool to compare two code snippets using linters and LLMs."""

    def __init__(
        self,
        language: AvailableLanguages = AvailableLanguages.R,
        linter: AvailableLinters = AvailableLinters.LINTR,
        model_name: str = "default_model",
        api_key: str = "default_api_key",
    ) -> None:
        """Initialise the comparison tool with language, linter, and LLM model."""
        self.language = language
        self.linter = linter
        self.model_name = model_name
        self.api_key = api_key
        self.model = LiteLLMModel(model_id=self.model_name, api_key=self.api_key)
        self.rubric = CodeQualityRubric()

    def _run_r_lintr(self, file_path: Path) -> str:
        """Run lintr::lint() on a snippet of R code and return the lint output as a string."""
        file_path = Path(file_path).resolve()

        r_cmd = f"""
        suppressPackageStartupMessages(library(lintr))
        l <- lintr::lint("{file_path.as_posix()}")
        if (length(l) == 0) {{
          cat("NO_LINTS\\n")
        }} else {{
          for (x in l) {{
            cat(sprintf("%s:%d:%d:%s\\n",
                        x$filename, x$line_number, x$column_number, x$message))
          }}
        }}
        """

        proc = subprocess.run(  # noqa: S603
            ["Rscript", "-e", r_cmd],  # noqa: S607
            text=True,
            capture_output=True,
            check=False,
        )

        # Optional: check proc.returncode / log stderr if needed
        return proc.stdout.strip()

    # TO DO: this may not generalise to other linters/languages
    @staticmethod
    def _extract_lint_messages(lint_output: str) -> list[str]:
        """Extract only the message part from each lint line."""
        if not lint_output or lint_output == "NO_LINTS":
            return []
        msgs: list[str] = []
        for line in lint_output.splitlines():
            parts = line.split(":", 3)  # file, line, col, message
            if len(parts) == 4:  # noqa: PLR2004
                msgs.append(parts[3].strip())
        return msgs

    def linter_comparison(
        self, code1_path: Path, code2_path: Path
    ) -> tuple[int, int, bool]:
        """
        Compare two pieces of code using a linter approach.
        Returns the number of lint errors found in each code snippet and whether they are identical after linting.
        """
        # For simplicity, we will just strip whitespace for this example.
        # Here you would integrate with the linter to compare the code

        if (
            self.language == AvailableLanguages.R
            and self.linter == AvailableLinters.LINTR
        ):
            lint_output1 = self._run_r_lintr(code1_path)
            lint_output2 = self._run_r_lintr(code2_path)

            msgs1 = self._extract_lint_messages(lint_output1)
            msgs2 = self._extract_lint_messages(lint_output2)

            return len(msgs1), len(msgs2), msgs1 == msgs2
        error_msg = (
            f"Unsupported language/linter combination: {self.language}, {self.linter}"
        )
        raise ValueError(error_msg)

    def llm_comparison(
        self, code1_path: Path, code2_path: Path
    ) -> CodeQualityAssessment:
        """
        Use an LLM + PydanticOutputParser to:
        - score code quality,
        - assess functional equivalence,
        - return a structured CodeQualityAssessment.
        """
        with Path.open(code1_path) as f:
            code1 = f.read()
        with Path.open(code2_path) as f:
            code2 = f.read()

        system_msg = self.rubric.system_message(
            language=self.language.value, code1=code1
        )
        user_msg = self.rubric.user_message(code2=code2)

        messages = [
            ChatMessage(
                role="system",
                content=[{"type": "text", "text": system_msg}],
            ),
            ChatMessage(
                role="user",
                content=[{"type": "text", "text": user_msg}],
            ),
        ]
        raw_response = self.model.generate(messages)
        raw_text = raw_response.content

        return self.rubric.parse(raw_text)


if __name__ == "__main__":
    model_id = "gpt-oss:120b-cloud"
    model_name = f"ollama_chat/{model_id}"
    api_key = "ollama"

    for truth_path_str in Path("tmp/smolagent_context").glob("sample*/rtruth.R"):
        code_file1 = Path(truth_path_str)
        code_file2 = code_file1.with_name("rpred.R")
        tool = CodeComparisonTool(
            language=AvailableLanguages.R,
            linter=AvailableLinters.LINTR,
            model_name=model_name,
            api_key=api_key,
        )
        linter_errors_1, linter_errors_2, are_equal = tool.linter_comparison(
            code_file1, code_file2
        )

        logger.info(f"Linter errors in code 1: {linter_errors_1}")
        logger.info(f"Linter errors in code 2: {linter_errors_2}")
        logger.info(f"Are the code snippets equivalent after linting? {are_equal}")

        assessment = tool.llm_comparison(
            code_file1,
            code_file2,
        )
        logger.info("LLM Code Quality Assessment:")
        logger.info(assessment.model_dump_json(indent=2))
