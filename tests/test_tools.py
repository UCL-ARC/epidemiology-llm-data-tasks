from unittest.mock import Mock, patch

from src.tools import produce_and_execute_r


class TestTools:
    """Tests for tools defined in src/tools.py."""

    @patch("subprocess.run")
    @patch("pathlib.Path.write_text")
    def test_produce_and_execute_r(self, mock_write_text: Mock, mock_run: Mock):
        """Test the produce_and_execute_r tool."""
        r_script = 'print("Hello, World!")'
        mock_completed_process = Mock()
        mock_completed_process.stdout = "Hello, World!\n"
        mock_completed_process.stderr = ""
        mock_run.return_value = mock_completed_process

        result = produce_and_execute_r(r_script)

        mock_write_text.assert_called_once_with(r_script, encoding="utf-8")
        mock_run.assert_called_once()
        assert "STDOUT:\nHello, World!\n" in result
        assert "STDERR:\n" in result
