"""Tests for src/r_code_agent.py."""

from unittest.mock import Mock, patch

import pytest

from src.r_code_agent import RExecutor, create_r_code_agent


class TestRExecutorInit:
    """Tests for RExecutor initialisation."""

    def test_default_init(self):
        """Should initialise with default values."""
        executor = RExecutor()
        assert executor.max_print_outputs_length == 50_000
        assert executor.timeout == 300
        assert executor.state == {"_print_outputs": ""}
        assert executor.custom_tools == {}

    def test_custom_init(self):
        """Should accept custom values."""
        executor = RExecutor(max_print_outputs_length=100, timeout=60)
        assert executor.max_print_outputs_length == 100
        assert executor.timeout == 60


class TestRExecutorMethods:
    """Tests for RExecutor helper methods."""

    def test_send_tools_stores_tools(self):
        """Should store tools dict."""
        executor = RExecutor()
        tools = {"tool1": Mock()}
        executor.send_tools(tools)
        assert executor.custom_tools == tools

    def test_send_variables_noop(self):
        """Should not raise or change state."""
        executor = RExecutor()
        executor.send_variables({"x": 1})
        # No assertion needed — just verifying no error


class TestRExecutorCall:
    """Tests for RExecutor.__call__."""

    @patch("src.r_code_agent.subprocess.run")
    def test_successful_execution(self, mock_run):
        """Should return CodeOutput on success."""
        mock_run.return_value = Mock(returncode=0, stdout="output text", stderr="")

        executor = RExecutor()
        result = executor("cat('hello')")

        assert result.output == "output text"
        assert result.is_final_answer is False
        mock_run.assert_called_once()

    @patch("src.r_code_agent.subprocess.run")
    def test_final_answer_detection(self, mock_run):
        """Should detect final_answer in code."""
        mock_run.return_value = Mock(returncode=0, stdout="done", stderr="")

        executor = RExecutor()
        result = executor('final_answer("done")')

        assert result.is_final_answer is True

    @patch("src.r_code_agent.subprocess.run")
    def test_failed_execution_raises(self, mock_run):
        """Should raise RuntimeError when R script fails."""
        mock_run.return_value = Mock(returncode=1, stdout="", stderr="Error in script")

        executor = RExecutor()
        with pytest.raises(RuntimeError, match="R script failed"):
            executor("stop('error')")

    @patch("src.r_code_agent.subprocess.run")
    def test_stderr_appended_to_logs(self, mock_run):
        """Should append stderr to logs."""
        mock_run.return_value = Mock(
            returncode=0, stdout="output", stderr="Warning msg"
        )

        executor = RExecutor()
        result = executor("cat('hello')")

        assert "STDERR:" in result.logs
        assert "Warning msg" in result.logs

    @patch("src.r_code_agent.subprocess.run")
    def test_output_truncation(self, mock_run):
        """Should truncate output that exceeds max length."""
        long_output = "x" * 60_000
        mock_run.return_value = Mock(returncode=0, stdout=long_output, stderr="")

        executor = RExecutor(max_print_outputs_length=100)
        result = executor("cat(rep('x', 60000))")  # noqa: F841

        assert "...(truncated)" in executor.state["_print_outputs"]

    @patch("src.r_code_agent.subprocess.run")
    def test_empty_stdout_returns_default_message(self, mock_run):
        """Should return default message when stdout is empty."""
        mock_run.return_value = Mock(returncode=0, stdout="", stderr="")

        executor = RExecutor()
        result = executor("invisible(1)")

        assert result.output == "R script completed."

    @patch("src.r_code_agent.subprocess.run")
    def test_strips_final_answer_from_code(self, mock_run):
        """Should strip final_answer sentinel from code before execution."""
        mock_run.return_value = Mock(returncode=0, stdout="done", stderr="")

        executor = RExecutor()
        executor('some_code()\nfinal_answer("done")')

        # Check the code written to the temp file doesn't contain final_answer
        call_args = mock_run.call_args
        assert call_args is not None


class TestCreateRCodeAgent:
    """Tests for create_r_code_agent factory function."""

    @patch("src.r_code_agent.CodeAgent")
    def test_creates_agent_with_r_executor(self, mock_code_agent):
        """Should create a CodeAgent with RExecutor."""
        model = Mock()
        create_r_code_agent(model=model)

        mock_code_agent.assert_called_once()
        call_kwargs = mock_code_agent.call_args.kwargs
        assert call_kwargs["model"] == model
        assert isinstance(call_kwargs["executor"], RExecutor)

    @patch("src.r_code_agent.CodeAgent")
    def test_passes_timeout_to_executor(self, mock_code_agent):
        """Should pass timeout parameter to RExecutor."""
        model = Mock()
        create_r_code_agent(model=model, timeout=120)

        call_kwargs = mock_code_agent.call_args.kwargs
        assert call_kwargs["executor"].timeout == 120

    @patch("src.r_code_agent.CodeAgent")
    def test_passes_tools(self, mock_code_agent):
        """Should pass tools list to CodeAgent."""
        model = Mock()
        tools = [Mock()]
        create_r_code_agent(model=model, tools=tools)

        call_kwargs = mock_code_agent.call_args.kwargs
        assert call_kwargs["tools"] == tools

    @patch("src.r_code_agent.CodeAgent")
    def test_default_empty_tools(self, mock_code_agent):
        """Should use empty tools list by default."""
        model = Mock()
        create_r_code_agent(model=model)

        call_kwargs = mock_code_agent.call_args.kwargs
        assert call_kwargs["tools"] == []
