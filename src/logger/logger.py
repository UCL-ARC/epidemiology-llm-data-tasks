"""Logger configuration module for ground truth initialization."""

from loguru import logger as _loguru_logger
from rich.console import Console
from rich.live import Live
from rich.spinner import Spinner
from rich.theme import Theme

# --- Setup ---
custom_theme = Theme(
    {
        "info": "bold blue",
        "success": "bold green",
        "error": "bold red",
        "warn": "yellow",
    }
)

# Create instances that other modules can import
console = Console(theme=custom_theme)
logger = _loguru_logger

# Export Rich components so other modules don't need to import them directly
__all__ = ["Live", "Spinner", "console", "logger"]
