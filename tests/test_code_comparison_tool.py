"""Tests for src/code_comparison_tool.py."""

from pathlib import Path
from unittest.mock import Mock, patch

import pytest

from src.code_comparison_tool import (
    AvailableLanguages,
    AvailableLinters,
    CodeComparisonTool,
)
from src.rubrics import CodeQualityAssessment


class TestAvailableLanguages:
    """Tests for AvailableLanguages enum."""

    def test_r_language_exists(self):
        """R should be an available language."""
        assert AvailableLanguages.R.value == "R"


class TestAvailableLinters:
    """Tests for AvailableLinters enum."""

    def test_lintr_exists(self):
        """lintr should be an available linter."""
        assert AvailableLinters.LINTR.value == "lintr"


class TestCodeComparisonToolInit:
    """Tests for CodeComparisonTool initialisation."""

    @patch("src.code_comparison_tool.LiteLLMModel")
    @patch("src.code_comparison_tool.CodeQualityRubric")
    def test_init_sets_defaults(self, mock_rubric, mock_model):
        """Should initialise with default values."""
        tool = CodeComparisonTool()

        assert tool.language == AvailableLanguages.R
        assert tool.linter == AvailableLinters.LINTR
        assert tool.model_name == "default_model"
        assert tool.api_key == "default_api_key"
        mock_model.assert_called_once_with(
            model_id="default_model", api_key="default_api_key"
        )
        mock_rubric.assert_called_once()

    @patch("src.code_comparison_tool.LiteLLMModel")
    @patch("src.code_comparison_tool.CodeQualityRubric")
    def test_init_accepts_custom_values(self, mock_rubric, mock_model):
        """Should accept custom language, linter, model_name, and api_key."""
        tool = CodeComparisonTool(
            language=AvailableLanguages.R,
            linter=AvailableLinters.LINTR,
            model_name="custom-model",
            api_key="custom-key",
        )

        assert tool.language == AvailableLanguages.R
        assert tool.linter == AvailableLinters.LINTR
        assert tool.model_name == "custom-model"
        assert tool.api_key == "custom-key"
        mock_model.assert_called_once_with(
            model_id="custom-model", api_key="custom-key"
        )


class TestExtractLintMessages:
    """Tests for CodeComparisonTool._extract_lint_messages."""

    def test_returns_empty_list_for_no_lints(self):
        """Should return empty list when lint output is NO_LINTS."""
        result = CodeComparisonTool._extract_lint_messages("NO_LINTS")
        assert result == []

    def test_returns_empty_list_for_empty_string(self):
        """Should return empty list for empty string."""
        result = CodeComparisonTool._extract_lint_messages("")
        assert result == []

    def test_returns_empty_list_for_none(self):
        """Should return empty list for None."""
        result = CodeComparisonTool._extract_lint_messages(None)
        assert result == []

    def test_extracts_messages_from_lint_output(self):
        """Should extract message part from lint output lines."""
        lint_output = (
            "/path/to/file.R:10:5:Variable name should be snake_case\n"
            "/path/to/file.R:20:1:Line is too long"
        )
        result = CodeComparisonTool._extract_lint_messages(lint_output)

        assert len(result) == 2
        assert result[0] == "Variable name should be snake_case"
        assert result[1] == "Line is too long"

    def test_handles_malformed_lines(self):
        """Should skip lines that don't have expected format."""
        lint_output = (
            "/path/to/file.R:10:5:Valid message\n"
            "malformed line without colons\n"
            "/path/to/file.R:20:1:Another valid message"
        )
        result = CodeComparisonTool._extract_lint_messages(lint_output)

        assert len(result) == 2
        assert result[0] == "Valid message"
        assert result[1] == "Another valid message"


class TestRunRLintr:
    """Tests for CodeComparisonTool._run_r_lintr."""

    @patch("src.code_comparison_tool.LiteLLMModel")
    @patch("src.code_comparison_tool.CodeQualityRubric")
    @patch("subprocess.run")
    def test_runs_rscript_with_correct_command(
        self, mock_subprocess, mock_rubric, mock_model, tmp_path
    ):
        """Should run Rscript with lintr command."""
        tool = CodeComparisonTool()

        # Create a temp R file
        r_file = tmp_path / "test.R"
        r_file.write_text("x <- 1")

        mock_result = Mock()
        mock_result.stdout = "NO_LINTS\n"
        mock_subprocess.return_value = mock_result

        result = tool._run_r_lintr(r_file)

        mock_subprocess.assert_called_once()
        call_args = mock_subprocess.call_args
        assert call_args.kwargs["text"] is True
        assert call_args.kwargs["capture_output"] is True
        assert call_args.kwargs["check"] is False
        assert "Rscript" in call_args.args[0]
        assert result == "NO_LINTS"

    @patch("src.code_comparison_tool.LiteLLMModel")
    @patch("src.code_comparison_tool.CodeQualityRubric")
    @patch("subprocess.run")
    def test_returns_stdout_stripped(
        self, mock_subprocess, mock_rubric, mock_model, tmp_path
    ):
        """Should return stripped stdout from subprocess."""
        tool = CodeComparisonTool()

        r_file = tmp_path / "test.R"
        r_file.write_text("x <- 1")

        mock_result = Mock()
        mock_result.stdout = "  some output  \n"
        mock_subprocess.return_value = mock_result

        result = tool._run_r_lintr(r_file)

        assert result == "some output"


class TestLinterComparison:
    """Tests for CodeComparisonTool.linter_comparison."""

    @patch("src.code_comparison_tool.LiteLLMModel")
    @patch("src.code_comparison_tool.CodeQualityRubric")
    @patch.object(CodeComparisonTool, "_run_r_lintr")
    def test_returns_lint_counts_and_equality(
        self, mock_run_lintr, mock_rubric, mock_model, tmp_path
    ):
        """Should return lint error counts and whether messages are equal."""
        tool = CodeComparisonTool(
            language=AvailableLanguages.R,
            linter=AvailableLinters.LINTR,
        )

        file1 = tmp_path / "code1.R"
        file2 = tmp_path / "code2.R"
        file1.write_text("x <- 1")
        file2.write_text("y <- 2")

        # Both files have same lint messages
        mock_run_lintr.side_effect = [
            "/path:1:1:Message A\n/path:2:1:Message B",
            "/path:1:1:Message A\n/path:2:1:Message B",
        ]

        count1, count2, are_equal = tool.linter_comparison(file1, file2)

        assert count1 == 2
        assert count2 == 2
        assert are_equal is True

    @patch("src.code_comparison_tool.LiteLLMModel")
    @patch("src.code_comparison_tool.CodeQualityRubric")
    @patch.object(CodeComparisonTool, "_run_r_lintr")
    def test_returns_false_when_messages_differ(
        self, mock_run_lintr, mock_rubric, mock_model, tmp_path
    ):
        """Should return False when lint messages differ."""
        tool = CodeComparisonTool(
            language=AvailableLanguages.R,
            linter=AvailableLinters.LINTR,
        )

        file1 = tmp_path / "code1.R"
        file2 = tmp_path / "code2.R"
        file1.write_text("x <- 1")
        file2.write_text("y <- 2")

        mock_run_lintr.side_effect = [
            "/path:1:1:Message A",
            "/path:1:1:Different message",
        ]

        count1, count2, are_equal = tool.linter_comparison(file1, file2)

        assert count1 == 1
        assert count2 == 1
        assert are_equal is False

    @patch("src.code_comparison_tool.LiteLLMModel")
    @patch("src.code_comparison_tool.CodeQualityRubric")
    def test_raises_for_unsupported_language_linter(self, mock_rubric, mock_model):
        """Should raise ValueError for unsupported language/linter combo."""
        tool = CodeComparisonTool()
        # Manually set unsupported combo
        tool.language = Mock()
        tool.linter = Mock()

        with pytest.raises(ValueError, match="Unsupported language/linter combination"):
            tool.linter_comparison(Path("a.R"), Path("b.R"))


class TestLLMComparison:
    """Tests for CodeComparisonTool.llm_comparison."""

    @patch("src.code_comparison_tool.LiteLLMModel")
    @patch("src.code_comparison_tool.CodeQualityRubric")
    def test_returns_code_quality_assessment(
        self, mock_rubric_class, mock_model_class, tmp_path
    ):
        """Should return a CodeQualityAssessment from LLM response."""
        # Setup mocks
        mock_model = Mock()
        mock_model_class.return_value = mock_model

        mock_rubric = Mock()
        mock_rubric_class.return_value = mock_rubric

        mock_rubric.system_message.return_value = "system message with rubric"
        mock_rubric.user_message.return_value = "Code to compare:\n```\ny <- 2\n```"

        expected_assessment = CodeQualityAssessment(
            functional_equivalence_score=8,
            readability_score=7,
            style_score=6,
            robustness_score=5,
            overall_quality_score=7,
            functionally_equivalent=True,
            explanation="Both scripts are equivalent.",
        )
        mock_rubric.parse.return_value = expected_assessment

        mock_response = Mock()
        mock_response.content = '{"functional_equivalence_score": 8, ...}'
        mock_model.generate.return_value = mock_response

        # Create temp files
        file1 = tmp_path / "code1.R"
        file2 = tmp_path / "code2.R"
        file1.write_text("x <- 1")
        file2.write_text("y <- 2")

        tool = CodeComparisonTool()
        result = tool.llm_comparison(file1, file2)

        assert result == expected_assessment
        mock_rubric.system_message.assert_called_once_with(
            language="R",
            code1="x <- 1",
        )
        mock_rubric.user_message.assert_called_once_with(code2="y <- 2")
        mock_model.generate.assert_called_once()
        mock_rubric.parse.assert_called_once_with(mock_response.content)

    @patch("src.code_comparison_tool.LiteLLMModel")
    @patch("src.code_comparison_tool.CodeQualityRubric")
    def test_sends_correct_chat_messages(
        self, mock_rubric_class, mock_model_class, tmp_path
    ):
        """Should send system and user ChatMessages to model."""
        mock_model = Mock()
        mock_model_class.return_value = mock_model

        mock_rubric = Mock()
        mock_rubric_class.return_value = mock_rubric
        mock_rubric.system_message.return_value = "system prompt"
        mock_rubric.user_message.return_value = "user prompt"
        mock_rubric.parse.return_value = Mock()

        mock_response = Mock()
        mock_response.content = "{}"
        mock_model.generate.return_value = mock_response

        file1 = tmp_path / "code1.R"
        file2 = tmp_path / "code2.R"
        file1.write_text("code1")
        file2.write_text("code2")

        tool = CodeComparisonTool()
        tool.llm_comparison(file1, file2)

        # Check the messages sent to generate
        call_args = mock_model.generate.call_args
        messages = call_args.args[0]

        assert len(messages) == 2
        assert messages[0].role == "system"
        assert messages[0].content[0]["text"] == "system prompt"
        assert messages[1].role == "user"
        assert messages[1].content[0]["text"] == "user prompt"
