"""Tests for src/initialise_ground_truth.py."""

from pathlib import Path
from unittest.mock import Mock, mock_open, patch

from src.initialise_ground_truth import (
    copy_raw_data,
    get_and_sort_task_dirs,
    load_metadata,
    main,
    run_r_script,
)


class TestLoadMetadata:
    """Test load_metadata function."""

    @patch(
        "pathlib.Path.open", new_callable=mock_open, read_data='{"file1.csv": "data"}'
    )
    def test_load_metadata_success(self, mock_file):
        """Test successful metadata loading."""
        metadata_path = Path("test_metadata.json")
        result = load_metadata(metadata_path)

        assert result == {"file1.csv": "data"}
        mock_file.assert_called_once()  # Remove the empty parentheses

    @patch("pathlib.Path.open")
    def test_load_metadata_file_not_found(self, mock_file):
        """Test metadata loading when file doesn't exist."""
        mock_file.side_effect = FileNotFoundError()
        metadata_path = Path("nonexistent.json")

        result = load_metadata(metadata_path)

        assert result == {}

    @patch("pathlib.Path.open", new_callable=mock_open, read_data="invalid json}")
    def test_load_metadata_invalid_json(self, mock_file):
        """Test metadata loading with invalid JSON."""
        metadata_path = Path("invalid.json")

        result = load_metadata(metadata_path)

        assert result == {}

    @patch("pathlib.Path.open")
    def test_load_metadata_generic_exception(self, mock_file):
        """Test metadata loading with generic exception."""
        mock_file.side_effect = Exception("Generic error")
        metadata_path = Path("test.json")

        result = load_metadata(metadata_path)

        assert result == {}


class TestCopyRawData:
    """Test copy_raw_data function."""

    @patch("pathlib.Path.mkdir")
    @patch("pathlib.Path.open", new_callable=mock_open, read_data=b"test data")
    def test_copy_raw_data_success(self, mock_file, mock_mkdir):
        """Test successful file copying."""
        input_dir = Path("input")
        task_input_dir = Path("task/input")
        metadata = {"file1.csv": "data", "file2.txt": "data"}

        failed_files = copy_raw_data(input_dir, task_input_dir, metadata)

        assert failed_files == []
        # The function calls mkdir with the path and exist_ok=True
        mock_mkdir.assert_called_with(task_input_dir, exist_ok=True, parents=True)
        assert mock_file.call_count == 4  # 2 files x 2 opens each (read + write)

    @patch("pathlib.Path.mkdir")
    @patch("pathlib.Path.open")
    def test_copy_raw_data_file_not_found(self, mock_file, mock_mkdir):
        """Test file copying when source file doesn't exist."""
        mock_file.side_effect = FileNotFoundError()
        input_dir = Path("input")
        task_input_dir = Path("task/input")
        metadata = {"missing_file.csv": "data"}

        failed_files = copy_raw_data(input_dir, task_input_dir, metadata)

        assert failed_files == ["missing_file.csv"]

    @patch("pathlib.Path.mkdir")
    @patch("pathlib.Path.open")
    def test_copy_raw_data_permission_error(self, mock_file, mock_mkdir):
        """Test file copying with permission error."""
        mock_file.side_effect = PermissionError()
        input_dir = Path("input")
        task_input_dir = Path("task/input")
        metadata = {"file1.csv": "data"}

        failed_files = copy_raw_data(input_dir, task_input_dir, metadata)

        assert failed_files == ["file1.csv"]


class TestRunRScript:
    """Test run_r_script function."""

    @patch("subprocess.run")
    def test_run_r_script_success(self, mock_run):
        """Test successful R script execution."""
        mock_run.return_value = Mock(
            returncode=0, stdout="Script executed successfully", stderr=""
        )

        r_script_path = Path("task1/rtruth.R")
        result = run_r_script(r_script_path, verbose=True)

        assert result is True
        mock_run.assert_called_once_with(
            ["Rscript", "rtruth.R"],
            cwd=Path("task1"),
            capture_output=True,
            text=True,
            check=False,
        )

    @patch("subprocess.run")
    def test_run_r_script_failure(self, mock_run):
        """Test R script execution failure."""
        mock_run.return_value = Mock(returncode=1, stdout="", stderr="Error in script")

        r_script_path = Path("task1/rtruth.R")
        result = run_r_script(r_script_path, verbose=False)

        assert result is False

    @patch("subprocess.run")
    def test_run_r_script_exception(self, mock_run):
        """Test R script execution with exception."""
        mock_run.side_effect = Exception("Subprocess error")

        r_script_path = Path("task1/rtruth.R")
        result = run_r_script(r_script_path, verbose=False)

        assert result is False

    @patch("subprocess.run")
    def test_run_r_script_verbose_output(self, mock_run):
        """Test R script execution with verbose output."""
        mock_run.return_value = Mock(returncode=0, stdout="Verbose output", stderr="")

        r_script_path = Path("task1/rtruth.R")
        result = run_r_script(r_script_path, verbose=True)

        assert result is True


class TestGetAndSortTaskDirs:
    """Test get_and_sort_task_dirs function."""

    @patch("pathlib.Path.iterdir")
    def test_get_and_sort_task_dirs_success(self, mock_iterdir):
        """Test successful task directory sorting."""
        mock_dirs = []
        for name in ["task10", "task2", "task1", "other_dir"]:
            mock_dir = Mock(spec=Path)
            mock_dir.name = name
            mock_dirs.append(mock_dir)

        mock_iterdir.return_value = mock_dirs

        ground_truth_dir = Path("ground_truth")
        result = get_and_sort_task_dirs(ground_truth_dir)

        # Should return task directories sorted by number
        expected_names = ["task1", "task2", "task10"]
        result_names = [d.name for d in result]
        assert result_names == expected_names

    @patch("pathlib.Path.iterdir")
    def test_get_and_sort_task_dirs_no_tasks(self, mock_iterdir):
        """Test when no task directories exist."""
        mock_dirs = []
        for name in ["other_dir", "not_task"]:
            mock_dir = Mock(spec=Path)
            mock_dir.name = name
            mock_dirs.append(mock_dir)

        mock_iterdir.return_value = mock_dirs

        ground_truth_dir = Path("ground_truth")
        result = get_and_sort_task_dirs(ground_truth_dir)

        assert result == []

    @patch("pathlib.Path.iterdir")
    def test_get_and_sort_task_dirs_malformed_names(self, mock_iterdir):
        """Test with malformed task directory names."""
        mock_dirs = []
        for name in ["task", "task1", "taskabc"]:
            mock_dir = Mock(spec=Path)
            mock_dir.name = name
            mock_dirs.append(mock_dir)

        mock_iterdir.return_value = mock_dirs

        ground_truth_dir = Path("ground_truth")
        result = get_and_sort_task_dirs(ground_truth_dir)

        # Should only return properly formatted task directories
        assert len(result) == 1
        assert result[0].name == "task1"


class TestMain:
    """Test main function."""

    @patch("src.initialise_ground_truth.run_r_script")
    @patch("src.initialise_ground_truth.copy_raw_data")
    @patch("src.initialise_ground_truth.load_metadata")
    @patch("src.initialise_ground_truth.get_and_sort_task_dirs")
    @patch("argparse.ArgumentParser.parse_args")
    def test_main_success(
        self, mock_args, mock_get_dirs, mock_load_metadata, mock_copy_data, mock_run_r
    ):
        """Test successful main execution."""
        # Setup mocks
        mock_args.return_value = Mock(
            input_dir=Path("input"),
            ground_truth_dir=Path("ground_truth"),
            verbose=False,
        )

        # Use real Path objects instead of mocks
        mock_get_dirs.return_value = [Path("ground_truth/task1")]

        mock_load_metadata.return_value = {"file1.csv": "data"}
        mock_copy_data.return_value = []  # No failed files
        mock_run_r.return_value = True

        # Run main
        main()

        # Verify calls
        mock_get_dirs.assert_called_once()
        mock_load_metadata.assert_called_once()
        mock_copy_data.assert_called_once()
        mock_run_r.assert_called_once()

    @patch("src.initialise_ground_truth.get_and_sort_task_dirs")
    @patch("argparse.ArgumentParser.parse_args")
    def test_main_no_tasks(self, mock_args, mock_get_dirs):
        """Test main execution with no task directories."""
        mock_args.return_value = Mock(
            input_dir=Path("input"),
            ground_truth_dir=Path("ground_truth"),
            verbose=False,
        )

        mock_get_dirs.return_value = []

        # Should complete without error
        main()

        mock_get_dirs.assert_called_once()

    @patch("src.initialise_ground_truth.run_r_script")
    @patch("src.initialise_ground_truth.copy_raw_data")
    @patch("src.initialise_ground_truth.load_metadata")
    @patch("src.initialise_ground_truth.get_and_sort_task_dirs")
    @patch("argparse.ArgumentParser.parse_args")
    def test_main_metadata_failure(
        self, mock_args, mock_get_dirs, mock_load_metadata, mock_copy_data, mock_run_r
    ):
        """Test main execution with metadata loading failure."""
        mock_args.return_value = Mock(
            input_dir=Path("input"),
            ground_truth_dir=Path("ground_truth"),
            verbose=False,
        )

        # Use real Path objects instead of mocks
        mock_get_dirs.return_value = [Path("ground_truth/task1")]

        mock_load_metadata.return_value = {}  # Empty metadata (failure)

        main()

        # Should not proceed to copy data or run R script
        mock_copy_data.assert_not_called()
        mock_run_r.assert_not_called()

    @patch("src.initialise_ground_truth.run_r_script")
    @patch("src.initialise_ground_truth.copy_raw_data")
    @patch("src.initialise_ground_truth.load_metadata")
    @patch("src.initialise_ground_truth.get_and_sort_task_dirs")
    @patch("argparse.ArgumentParser.parse_args")
    def test_main_copy_failure(
        self, mock_args, mock_get_dirs, mock_load_metadata, mock_copy_data, mock_run_r
    ):
        """Test main execution with file copying failure."""
        mock_args.return_value = Mock(
            input_dir=Path("input"),
            ground_truth_dir=Path("ground_truth"),
            verbose=False,
        )

        # Use real Path objects instead of mocks
        mock_get_dirs.return_value = [Path("ground_truth/task1")]

        mock_load_metadata.return_value = {"file1.csv": "data"}
        mock_copy_data.return_value = ["file1.csv"]  # Failed file

        main()

        # Should not proceed to run R script
        mock_run_r.assert_not_called()

    @patch("src.initialise_ground_truth.run_r_script")
    @patch("src.initialise_ground_truth.copy_raw_data")
    @patch("src.initialise_ground_truth.load_metadata")
    @patch("src.initialise_ground_truth.get_and_sort_task_dirs")
    @patch("argparse.ArgumentParser.parse_args")
    def test_main_r_script_failure(
        self, mock_args, mock_get_dirs, mock_load_metadata, mock_copy_data, mock_run_r
    ):
        """Test main execution with R script failure."""
        mock_args.return_value = Mock(
            input_dir=Path("input"),
            ground_truth_dir=Path("ground_truth"),
            verbose=False,
        )

        # Use real Path objects instead of mocks
        mock_get_dirs.return_value = [Path("ground_truth/task1")]

        mock_load_metadata.return_value = {"file1.csv": "data"}
        mock_copy_data.return_value = []  # No failed files
        mock_run_r.return_value = False  # R script failure

        main()

        # All functions should be called
        mock_load_metadata.assert_called_once()
        mock_copy_data.assert_called_once()
        mock_run_r.assert_called_once()
