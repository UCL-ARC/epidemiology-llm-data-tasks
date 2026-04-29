"""Tests for src/rebuild_experiments.py."""

import json
from unittest.mock import Mock, patch

from src.rebuild_experiments import (
    copy_raw_data,
    get_model_run_dirs,
    get_task_dirs,
    load_metadata,
    run_r_script,
)


class TestLoadMetadata:
    """Tests for load_metadata."""

    def test_loads_valid_json(self, tmp_path):
        """Should load and return valid JSON metadata."""
        meta_file = tmp_path / "metadata.json"
        meta_file.write_text(json.dumps({"file1.tab": {"cols": 5}}))

        result = load_metadata(meta_file)
        assert result == {"file1.tab": {"cols": 5}}

    def test_returns_empty_on_missing_file(self, tmp_path):
        """Should return empty dict when file doesn't exist."""
        result = load_metadata(tmp_path / "nonexistent.json")
        assert result == {}

    def test_returns_empty_on_invalid_json(self, tmp_path):
        """Should return empty dict on malformed JSON."""
        meta_file = tmp_path / "metadata.json"
        meta_file.write_text("{invalid json")

        result = load_metadata(meta_file)
        assert result == {}

    def test_returns_empty_on_generic_error(self, tmp_path):
        """Should return empty dict on unexpected errors."""
        # A directory instead of a file triggers a different error
        meta_dir = tmp_path / "metadata.json"
        meta_dir.mkdir()

        result = load_metadata(meta_dir)
        assert result == {}


class TestCopyRawData:
    """Tests for copy_raw_data."""

    def test_copies_files_successfully(self, tmp_path):
        """Should copy matching input files to task directory."""
        input_dir = tmp_path / "input"
        input_dir.mkdir()
        (input_dir / "data.tab").write_text("col1\n1")

        task_dir = tmp_path / "task_input"

        metadata = {"data.tab": {"info": "test"}}
        failed = copy_raw_data(input_dir, task_dir, metadata)

        assert failed == []
        assert (task_dir / "data.tab").exists()
        assert (task_dir / "data.tab").read_text() == "col1\n1"

    def test_reports_missing_files(self, tmp_path):
        """Should return filenames that don't exist in input dir."""
        input_dir = tmp_path / "input"
        input_dir.mkdir()

        task_dir = tmp_path / "task_input"

        metadata = {"missing.tab": {"info": "test"}}
        failed = copy_raw_data(input_dir, task_dir, metadata)

        assert failed == ["missing.tab"]

    def test_creates_task_dir(self, tmp_path):
        """Should create task input directory if it doesn't exist."""
        input_dir = tmp_path / "input"
        input_dir.mkdir()
        (input_dir / "data.tab").write_text("ok")

        task_dir = tmp_path / "deeply" / "nested" / "task_input"
        metadata = {"data.tab": {}}
        copy_raw_data(input_dir, task_dir, metadata)

        assert task_dir.exists()


class TestRunRScript:
    """Tests for run_r_script."""

    def test_returns_false_for_missing_script(self, tmp_path):
        """Should return False when script doesn't exist."""
        result = run_r_script(tmp_path / "nonexistent.R", verbose=False)
        assert result is False

    @patch("src.rebuild_experiments.subprocess.run")
    def test_returns_true_on_success(self, mock_run, tmp_path):
        """Should return True when script runs successfully."""
        script = tmp_path / "test.R"
        script.write_text("cat('hello')")

        mock_run.return_value = Mock(returncode=0, stdout="hello", stderr="")

        result = run_r_script(script, verbose=False)
        assert result is True

    @patch("src.rebuild_experiments.subprocess.run")
    def test_returns_false_on_failure(self, mock_run, tmp_path):
        """Should return False when script exits non-zero."""
        script = tmp_path / "test.R"
        script.write_text("stop('error')")

        mock_run.return_value = Mock(returncode=1, stdout="", stderr="Error")

        result = run_r_script(script, verbose=False)
        assert result is False

    @patch("src.rebuild_experiments.subprocess.run")
    def test_verbose_logs_stdout(self, mock_run, tmp_path):
        """Should not fail when verbose=True and there's stdout."""
        script = tmp_path / "test.R"
        script.write_text("cat('hello')")

        mock_run.return_value = Mock(returncode=0, stdout="hello world", stderr="")

        result = run_r_script(script, verbose=True)
        assert result is True

    @patch("src.rebuild_experiments.subprocess.run")
    def test_logs_stderr_warnings(self, mock_run, tmp_path):
        """Should handle stderr without failing."""
        script = tmp_path / "test.R"
        script.write_text("warning('warn')")

        mock_run.return_value = Mock(returncode=0, stdout="", stderr="Warning message")

        result = run_r_script(script, verbose=False)
        assert result is True

    @patch("src.rebuild_experiments.subprocess.run")
    def test_handles_exception(self, mock_run, tmp_path):
        """Should return False if subprocess raises exception."""
        script = tmp_path / "test.R"
        script.write_text("cat('hello')")

        mock_run.side_effect = OSError("Command not found")

        result = run_r_script(script, verbose=False)
        assert result is False


class TestGetModelRunDirs:
    """Tests for get_model_run_dirs."""

    def test_finds_model_dirs(self, tmp_path):
        """Should find directories starting with smolagent_context_."""
        (tmp_path / "smolagent_context_model1").mkdir()
        (tmp_path / "smolagent_context_model2").mkdir()
        (tmp_path / "other_dir").mkdir()

        dirs = get_model_run_dirs(tmp_path, None)
        assert len(dirs) == 2

    def test_filters_by_model_name(self, tmp_path):
        """Should filter dirs containing model substring."""
        (tmp_path / "smolagent_context_qwen3.5_1").mkdir()
        (tmp_path / "smolagent_context_gemma_1").mkdir()

        dirs = get_model_run_dirs(tmp_path, "qwen3.5")
        assert len(dirs) == 1
        assert "qwen3.5" in dirs[0].name

    def test_returns_empty_when_no_match(self, tmp_path):
        """Should return empty list when no dirs match."""
        (tmp_path / "smolagent_context_model1").mkdir()

        dirs = get_model_run_dirs(tmp_path, "nonexistent")
        assert dirs == []

    def test_returns_sorted(self, tmp_path):
        """Should return dirs in sorted order."""
        (tmp_path / "smolagent_context_b").mkdir()
        (tmp_path / "smolagent_context_a").mkdir()

        dirs = get_model_run_dirs(tmp_path, None)
        assert dirs[0].name < dirs[1].name


class TestGetTaskDirs:
    """Tests for get_task_dirs."""

    def test_finds_task_dirs(self, tmp_path):
        """Should find directories matching task[d+] pattern."""
        (tmp_path / "task1").mkdir()
        (tmp_path / "task2").mkdir()
        (tmp_path / "other").mkdir()

        dirs = get_task_dirs(tmp_path, None)
        assert len(dirs) == 2

    def test_sorts_by_task_number(self, tmp_path):
        """Should sort by numeric task number."""
        (tmp_path / "task10").mkdir()
        (tmp_path / "task2").mkdir()
        (tmp_path / "task1").mkdir()

        dirs = get_task_dirs(tmp_path, None)
        names = [d.name for d in dirs]
        assert names == ["task1", "task2", "task10"]

    def test_filters_by_task_number(self, tmp_path):
        """Should filter to specific task number."""
        (tmp_path / "task1").mkdir()
        (tmp_path / "task2").mkdir()
        (tmp_path / "task3").mkdir()

        dirs = get_task_dirs(tmp_path, 2)
        assert len(dirs) == 1
        assert dirs[0].name == "task2"

    def test_returns_empty_when_no_tasks(self, tmp_path):
        """Should return empty list when no task dirs exist."""
        (tmp_path / "other").mkdir()

        dirs = get_task_dirs(tmp_path, None)
        assert dirs == []
