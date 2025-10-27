"""Tests for src/ollama_utils.py."""

import subprocess
from pathlib import Path
from unittest.mock import Mock, patch

import pytest
from pydantic import ValidationError

from src.ollama_utils import (
    Model,
    OllamaError,
    OllamaModelHandler,
    install_ollama,
    uninstall_ollama,
)


@patch("src.ollama_utils.subprocess.run")
@patch("src.ollama_utils.subprocess.Popen")
def test_install_ollama_success(mock_popen, mock_run):
    """Test successful ollama installation."""
    mock_process = Mock()
    mock_process.stdout = Mock()
    mock_popen.return_value = mock_process

    mock_run.return_value = Mock(returncode=0)

    install_ollama()

    mock_popen.assert_called_once_with(
        ["/usr/bin/curl", "-fsSL", "https://ollama.com/install.sh"],
        stdout=subprocess.PIPE,
    )

    mock_run.assert_called_once_with(
        ["/usr/bin/sh"], stdin=mock_process.stdout, check=True
    )

    mock_process.stdout.close.assert_called_once()
    mock_process.wait.assert_called_once()


@patch("src.ollama_utils.subprocess.run")
@patch("src.ollama_utils.subprocess.Popen")
def test_install_ollama_run_failure(mock_popen, mock_run):
    """Test installation failure when subprocess.run fails."""
    mock_process = Mock()
    mock_process.stdout = Mock()
    mock_popen.return_value = mock_process

    mock_run.side_effect = subprocess.CalledProcessError(1, ["/usr/bin/sh"])

    with pytest.raises(subprocess.CalledProcessError):
        install_ollama()

    mock_process.stdout.close.assert_called_once()
    mock_process.wait.assert_called_once()


@patch("src.ollama_utils.subprocess.run")
@patch("src.ollama_utils.subprocess.Popen")
def test_install_ollama_no_stdout(mock_popen, mock_run):
    """Test installation when stdout is None."""
    mock_process = Mock()
    mock_process.stdout = None
    mock_popen.return_value = mock_process

    mock_run.return_value = Mock(returncode=0)

    install_ollama()

    mock_process.wait.assert_called_once()


@patch("src.ollama_utils.shutil.rmtree")
@patch("src.ollama_utils.shutil.which")
@patch("src.ollama_utils.subprocess.run")
@patch("src.ollama_utils.Path.unlink")
@patch("src.ollama_utils.Path.exists")
def test_uninstall_ollama_success(
    mock_exists, mock_unlink, mock_run, mock_which, mock_rmtree
):
    """Test successful ollama uninstallation."""
    mock_which.return_value = "/usr/local/bin/ollama"
    mock_exists.return_value = True
    mock_run.return_value = Mock(returncode=0)

    uninstall_ollama()

    assert mock_run.call_count >= 2


@patch("src.ollama_utils.shutil.rmtree")
@patch("src.ollama_utils.shutil.which")
@patch("src.ollama_utils.subprocess.run")
@patch("src.ollama_utils.Path.home")
def test_uninstall_ollama_permission_errors(
    mock_home, mock_run, mock_which, mock_rmtree
):
    """Test uninstallation with permission errors requiring sudo."""
    mock_which.return_value = None
    mock_home.return_value = Path("/home/test")
    mock_rmtree.side_effect = PermissionError()

    uninstall_ollama()

    sudo_calls = [c for c in mock_run.call_args_list if "/usr/bin/sudo" in str(c)]
    assert len(sudo_calls) > 0


@patch("src.ollama_utils.shutil.rmtree")
@patch("src.ollama_utils.Path.exists")
@patch("src.ollama_utils.Path.home")
@patch("src.ollama_utils.shutil.which")
@patch("src.ollama_utils.subprocess.run")
def test_uninstall_ollama_service_file_removed(
    mock_run, mock_which, mock_home, mock_exists, mock_rmtree
):
    """Test that service file removal is attempted."""
    mock_which.return_value = None
    mock_home.return_value = Path("/home/test")
    mock_exists.return_value = False

    uninstall_ollama()

    calls = [str(call) for call in mock_run.call_args_list]
    assert any("systemctl" in call and "stop" in call for call in calls)
    assert any("systemctl" in call and "disable" in call for call in calls)


@patch("src.ollama_utils.Path.exists")
def test_handler_initialization_success(mock_exists):
    """Test successful handler initialization."""
    mock_exists.return_value = True

    handler = OllamaModelHandler(
        model=Model.LLAMA_SMALL, ollama_path=Path("/usr/local/bin/ollama")
    )

    assert handler.model == Model.LLAMA_SMALL
    assert handler.ollama_path == Path("/usr/local/bin/ollama")


@patch("src.ollama_utils.Path.exists")
def test_handler_initialization_missing_ollama(mock_exists):
    """Test handler initialization fails when ollama not found."""
    mock_exists.return_value = False

    with pytest.raises(ValidationError, match="ollama executable not found"):
        OllamaModelHandler(
            model=Model.LLAMA_SMALL, ollama_path=Path("/nonexistent/ollama")
        )


@patch("src.ollama_utils.subprocess.run")
@patch("src.ollama_utils.Path.exists")
def test_install_model_success(mock_exists, mock_run):
    """Test successful model installation."""
    mock_exists.return_value = True
    mock_run.return_value = Mock(returncode=0)

    handler = OllamaModelHandler(
        model=Model.LLAMA_SMALL, ollama_path=Path("/usr/local/bin/ollama")
    )
    handler.install_model()

    mock_run.assert_called_once_with(
        [Path("/usr/local/bin/ollama"), "pull", "llama3.1:8b"],
        capture_output=True,
        text=True,
        check=True,
    )


@patch("src.ollama_utils.subprocess.run")
@patch("src.ollama_utils.Path.exists")
def test_install_model_failure(mock_exists, mock_run):
    """Test model installation failure."""
    mock_exists.return_value = True
    mock_run.return_value = Mock(returncode=1)

    handler = OllamaModelHandler(
        model=Model.LLAMA_SMALL, ollama_path=Path("/usr/local/bin/ollama")
    )

    with pytest.raises(OllamaError, match="failed to install llama3.1:8b"):
        handler.install_model()


@patch("src.ollama_utils.subprocess.run")
@patch("src.ollama_utils.Path.exists")
def test_serve_model_success(mock_exists, mock_run):
    """Test successful model serving."""
    mock_exists.return_value = True
    mock_run.return_value = Mock(returncode=0)

    handler = OllamaModelHandler(
        model=Model.LLAMA_SMALL, ollama_path=Path("/usr/local/bin/ollama")
    )
    handler.serve_model()

    mock_run.assert_called_once_with(
        [Path("/usr/local/bin/ollama"), "serve", "llama3.1:8b"],
        capture_output=True,
        text=True,
        check=True,
    )


@patch("src.ollama_utils.subprocess.run")
@patch("src.ollama_utils.Path.exists")
def test_serve_model_failure(mock_exists, mock_run):
    """Test model serving failure."""
    mock_exists.return_value = True
    mock_run.return_value = Mock(returncode=1)

    handler = OllamaModelHandler(
        model=Model.LLAMA_SMALL, ollama_path=Path("/usr/local/bin/ollama")
    )

    with pytest.raises(OllamaError, match="failed to serve llama3.1:8b"):
        handler.serve_model()


@patch("src.ollama_utils.subprocess.run")
@patch("src.ollama_utils.Path.exists")
def test_stop_model_success(mock_exists, mock_run):
    """Test successful model stopping."""
    mock_exists.return_value = True
    mock_run.return_value = Mock(returncode=0)

    handler = OllamaModelHandler(
        model=Model.LLAMA_SMALL, ollama_path=Path("/usr/local/bin/ollama")
    )
    handler.stop_model()

    mock_run.assert_called_once_with(
        [Path("/usr/local/bin/ollama"), "stop", "llama3.1:8b"],
        capture_output=True,
        text=True,
        check=True,
    )


@patch("src.ollama_utils.subprocess.run")
@patch("src.ollama_utils.Path.exists")
def test_stop_model_failure(mock_exists, mock_run):
    """Test model stopping failure."""
    mock_exists.return_value = True
    mock_run.return_value = Mock(returncode=1)

    handler = OllamaModelHandler(
        model=Model.LLAMA_SMALL, ollama_path=Path("/usr/local/bin/ollama")
    )

    with pytest.raises(OllamaError, match="failed to serve llama3.1:8b"):
        handler.stop_model()


@patch("src.ollama_utils.subprocess.run")
@patch("src.ollama_utils.Path.exists")
def test_delete_model_success(mock_exists, mock_run):
    """Test successful model deletion."""
    mock_exists.return_value = True
    mock_run.return_value = Mock(returncode=0)

    handler = OllamaModelHandler(
        model=Model.LLAMA_SMALL, ollama_path=Path("/usr/local/bin/ollama")
    )
    handler.delete_model()

    mock_run.assert_called_once_with(
        [Path("/usr/local/bin/ollama"), "rm", "llama3.1:8b"],
        capture_output=True,
        text=True,
        check=True,
    )


@patch("src.ollama_utils.subprocess.run")
@patch("src.ollama_utils.Path.exists")
def test_delete_model_failure(mock_exists, mock_run):
    """Test model deletion failure."""
    mock_exists.return_value = True
    mock_run.return_value = Mock(returncode=1)

    handler = OllamaModelHandler(
        model=Model.LLAMA_SMALL, ollama_path=Path("/usr/local/bin/ollama")
    )

    with pytest.raises(OllamaError, match="failed to serve llama3.1:8b"):
        handler.delete_model()


@patch("src.ollama_utils.Path.exists")
def test_handler_with_different_models(mock_exists):
    """Test handler initialization with different model types."""
    mock_exists.return_value = True

    for model in [
        Model.LLAMA_MID,
        Model.LLAMA_BIG,
        Model.GPTOSS_SMALL,
        Model.GPTOSS_BIG,
    ]:
        handler = OllamaModelHandler(
            model=model, ollama_path=Path("/usr/local/bin/ollama")
        )
        assert handler.model == model


@patch("src.ollama_utils.subprocess.run")
@patch("src.ollama_utils.Path.exists")
def test_handler_custom_ollama_path(mock_exists, mock_run):
    """Test handler with custom ollama path."""
    mock_exists.return_value = True
    mock_run.return_value = Mock(returncode=0)

    custom_path = Path("/custom/path/ollama")
    handler = OllamaModelHandler(model=Model.LLAMA_SMALL, ollama_path=custom_path)
    handler.install_model()

    mock_run.assert_called_once_with(
        [custom_path, "pull", "llama3.1:8b"],
        capture_output=True,
        text=True,
        check=True,
    )
