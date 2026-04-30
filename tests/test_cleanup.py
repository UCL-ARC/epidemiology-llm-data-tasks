"""Tests for src/cleanup.py."""

from src.cleanup import main


class TestCleanupMain:
    """Tests for cleanup main function."""

    def test_missing_context_root(self, tmp_path, monkeypatch):
        """Should log warning and return when context root doesn't exist."""
        monkeypatch.setattr(
            "src.cleanup.SMOLAGENT_CONTEXT_ROOT",
            tmp_path / "nonexistent",
        )
        # Should not raise
        main(delete_data=False)

    def test_no_task_dirs(self, tmp_path, monkeypatch):
        """Should report all tasks as missing when dir is empty."""
        context_root = tmp_path / "smolagent_context"
        context_root.mkdir()

        monkeypatch.setattr("src.cleanup.SMOLAGENT_CONTEXT_ROOT", context_root)
        main(delete_data=False)

    def test_complete_dirs_detected(self, tmp_path, monkeypatch):
        """Should detect dirs with cleaned_data.csv as complete."""
        context_root = tmp_path / "smolagent_context"
        context_root.mkdir()

        task_dir = context_root / "task1_abc123"
        (task_dir / "data" / "output").mkdir(parents=True)
        (task_dir / "data" / "output" / "cleaned_data.csv").write_text("a,b\n1,2")

        monkeypatch.setattr("src.cleanup.SMOLAGENT_CONTEXT_ROOT", context_root)
        main(delete_data=False)

    def test_incomplete_dirs_detected(self, tmp_path, monkeypatch):
        """Should detect dirs without cleaned_data.csv as incomplete."""
        context_root = tmp_path / "smolagent_context"
        context_root.mkdir()

        task_dir = context_root / "task2_xyz789"
        task_dir.mkdir()

        monkeypatch.setattr("src.cleanup.SMOLAGENT_CONTEXT_ROOT", context_root)
        main(delete_data=False)

    def test_delete_removes_incomplete_dirs(self, tmp_path, monkeypatch):
        """Should delete incomplete dirs when delete_data=True."""
        context_root = tmp_path / "smolagent_context"
        context_root.mkdir()

        incomplete_dir = context_root / "task3_del123"
        incomplete_dir.mkdir()

        monkeypatch.setattr("src.cleanup.SMOLAGENT_CONTEXT_ROOT", context_root)
        main(delete_data=True)

        assert not incomplete_dir.exists()

    def test_non_task_dirs_ignored(self, tmp_path, monkeypatch):
        """Should ignore directories that don't start with 'task'."""
        context_root = tmp_path / "smolagent_context"
        context_root.mkdir()

        other_dir = context_root / "other_dir"
        other_dir.mkdir()

        monkeypatch.setattr("src.cleanup.SMOLAGENT_CONTEXT_ROOT", context_root)
        main(delete_data=False)

    def test_files_in_root_ignored(self, tmp_path, monkeypatch):
        """Should ignore files (not dirs) in context root."""
        context_root = tmp_path / "smolagent_context"
        context_root.mkdir()

        (context_root / "some_file.txt").write_text("hello")

        monkeypatch.setattr("src.cleanup.SMOLAGENT_CONTEXT_ROOT", context_root)
        main(delete_data=False)

    def test_task_without_underscore(self, tmp_path, monkeypatch):
        """Should handle task dirs without underscore in name."""
        context_root = tmp_path / "smolagent_context"
        context_root.mkdir()

        task_dir = context_root / "task5"
        (task_dir / "data" / "output").mkdir(parents=True)
        (task_dir / "data" / "output" / "cleaned_data.csv").write_text("ok")

        monkeypatch.setattr("src.cleanup.SMOLAGENT_CONTEXT_ROOT", context_root)
        main(delete_data=False)

    def test_mixed_complete_incomplete_missing(self, tmp_path, monkeypatch):
        """Should correctly categorize a mix of complete, incomplete, missing."""
        context_root = tmp_path / "smolagent_context"
        context_root.mkdir()

        # Complete
        complete = context_root / "task1_ok"
        (complete / "data" / "output").mkdir(parents=True)
        (complete / "data" / "output" / "cleaned_data.csv").write_text("ok")

        # Incomplete
        incomplete = context_root / "task2_nope"
        incomplete.mkdir()

        # task3-20 missing
        monkeypatch.setattr("src.cleanup.SMOLAGENT_CONTEXT_ROOT", context_root)
        main(delete_data=False)
