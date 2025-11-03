"""Logger configuration module for ground truth initialization."""

from loguru import logger as _loguru_logger
from rich.console import Console
from rich.live import Live
from rich.spinner import Spinner
from rich.theme import Theme


def setup_logger(*, verbose: bool = False) -> None:
    """
    Configure Loguru logging.

    Args:
        verbose: Enable debug level logging if True, info level if False.

    """
    _loguru_logger.remove()
    # Console handler (integrates with Rich)
    _loguru_logger.add(
        console.print,
        level="DEBUG" if verbose else "INFO",
        colorize=True,
        format="<level>{level: <8}</level> | <cyan>{message}</cyan>",
    )
    # File handler
    _loguru_logger.add(
        "ground_truth_init.log",
        level="DEBUG",
        rotation="1 MB",
        format="{time:YYYY-MM-DD HH:mm:ss} | {level} | {message}",
    )


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
__all__ = ["Live", "Spinner", "console", "logger", "setup_logger"]
