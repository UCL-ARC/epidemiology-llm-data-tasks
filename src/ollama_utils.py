"""A set of utils wrapping around shell calls to handle ollama."""

import shutil
import subprocess
from contextlib import suppress
from enum import Enum
from pathlib import Path

from pydantic import BaseModel, field_validator


class OllamaError(Exception):
    """A generic ollama error."""


def install_ollama() -> None:
    """Install ollama using default installer."""
    p1 = subprocess.Popen(  # noqa: S603
        ["/usr/bin/curl", "-fsSL", "https://ollama.com/install.sh"],
        stdout=subprocess.PIPE,
    )
    try:
        subprocess.run(["/usr/bin/sh"], stdin=p1.stdout, check=True)  # noqa: S603
    finally:
        if p1.stdout:
            p1.stdout.close()
        p1.wait()


def uninstall_ollama() -> None:
    """Uninstall ollama. Prefer Path operations; fall back to sudo when permissions prevent removal."""
    # stop/disable service (ignore failures)
    for cmd in (["systemctl", "stop", "ollama"], ["systemctl", "disable", "ollama"]):
        with suppress(subprocess.CalledProcessError):
            subprocess.run(  # noqa: S603
                ["/usr/bin/sudo", *cmd], check=True, capture_output=True, text=True
            )

    #  systemd unit
    service_file = Path("/etc/systemd/system/ollama.service")
    try:
        service_file.unlink(missing_ok=True)
    except PermissionError:
        subprocess.run(["/usr/bin/sudo", "rm", str(service_file)], check=False)  # noqa: S603

    # binary found on PATH
    bin_path = shutil.which("ollama")
    if bin_path:
        bin_p = Path(bin_path)
        try:
            bin_p.unlink(missing_ok=True)
        except PermissionError:
            subprocess.run(["/usr/bin/sudo", "rm", str(bin_p)], check=False)  # noqa: S603

    # shared data dir
    share_dir = Path("/usr/share/ollama")
    try:
        shutil.rmtree(share_dir)
    except PermissionError:
        subprocess.run(["/usr/bin/sudo", "rm", "-r", str(share_dir)], check=False)  # noqa: S603

    # user data dir
    user_dir = Path.home() / ".ollama"
    if user_dir.exists():
        shutil.rmtree(user_dir, ignore_errors=True)


class Model(str, Enum):
    """Enum of permitted models to install/use. Extend as required."""

    LLAMA_SMALL = "llama3.1:8b"
    LLAMA_MID = "llama3.1:70b"
    LLAMA_BIG = "llama3.1:405b"
    GPTOSS_SMALL = "gpt-oss:20b"
    GPTOSS_BIG = "gpt-oss:120b"


class OllamaModelHandler(BaseModel):
    """Class to manage common ollama operations via python."""

    model: Model
    ollama_path: Path = Path("/usr/local/bin/ollama")

    @field_validator("ollama_path", mode="after")
    @classmethod
    def ensure_ollama_installed(cls, v: Path) -> Path:
        """Ensure the provided ollama path exists."""
        if not v.exists():
            missing_ollama_path = f"ollama executable not found at {v}."
            raise ValueError(missing_ollama_path)
        return v

    def install_model(self) -> None:
        """Install target ollama model."""
        output = subprocess.run(  # noqa: S603
            [self.ollama_path, "pull", self.model.value],
            capture_output=True,
            text=True,
            check=True,
        )
        if output.returncode != 0:
            install_error = f"failed to install {self.model.value}"
            raise OllamaError(install_error)

    def serve_model(self) -> None:
        """Serve target model."""
        output = subprocess.run(  # noqa: S603
            [self.ollama_path, "serve", self.model.value],
            capture_output=True,
            text=True,
            check=True,
        )
        if output.returncode != 0:
            serve_error = f"failed to serve {self.model.value}"
            raise OllamaError(serve_error)

    def stop_model(self) -> None:
        """Stop target model."""
        output = subprocess.run(  # noqa: S603
            [self.ollama_path, "stop", self.model.value],
            capture_output=True,
            text=True,
            check=True,
        )
        if output.returncode != 0:
            serve_error = f"failed to serve {self.model.value}"
            raise OllamaError(serve_error)

    def delete_model(self) -> None:
        """Delete target ollama model."""
        output = subprocess.run(  # noqa: S603
            [self.ollama_path, "rm", self.model.value],
            capture_output=True,
            text=True,
            check=True,
        )
        if output.returncode != 0:
            serve_error = f"failed to serve {self.model.value}"
            raise OllamaError(serve_error)
