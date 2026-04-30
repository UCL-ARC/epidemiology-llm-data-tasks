"""Script to run the epidemiology LLM agent experiment."""

from __future__ import annotations

import argparse
import json
import os
from typing import TYPE_CHECKING, Literal

if TYPE_CHECKING:
    from collections.abc import Callable

from pathlib import Path

import yaml
from loguru import logger

from src.agents import SmolAgent
from src.tools import produce_and_execute_r


def _load_config(config_path: Path) -> dict:
    """Load experiment config from a YAML file."""
    with config_path.open() as f:
        return yaml.safe_load(f)


def _parse_args() -> argparse.Namespace:
    """Parse CLI arguments. Any provided arg overrides the YAML config."""
    parser = argparse.ArgumentParser(
        description="Run the epidemiology LLM agent experiment."
    )
    parser.add_argument(
        "--config",
        type=Path,
        default=Path("./experiment.yml"),
        help="Path to experiment YAML config (default: ./experiment.yml)",
    )
    parser.add_argument("--model_id", type=str, help="Model ID (overrides config)")
    parser.add_argument(
        "--provider",
        type=str,
        choices=["ollama", "huggingface"],
        help="Model provider (overrides config)",
    )
    parser.add_argument(
        "--temperature", type=float, help="Sampling temperature (overrides config)"
    )
    parser.add_argument(
        "--agent_type",
        type=str,
        choices=["tool_calling", "code"],
        help="Agent type (overrides config)",
    )
    parser.add_argument(
        "--runs", type=int, help="Number of repeat runs (overrides config)"
    )
    parser.add_argument(
        "--tasks",
        type=int,
        nargs="+",
        help="Task numbers to run, e.g. --tasks 1 2 3 (overrides config)",
    )
    parser.add_argument(
        "--use_overrides",
        action="store_true",
        default=None,
        help="Use per-task override prompts where present (overrides config)",
    )
    parser.add_argument(
        "--no_overrides",
        dest="use_overrides",
        action="store_false",
        help="Ignore per-task override prompts (overrides config)",
    )
    parser.add_argument(
        "--persist_context",
        action="store_true",
        default=None,
        help="Keep temp working directories after runs (overrides config)",
    )
    parser.add_argument(
        "--no_persist_context",
        dest="persist_context",
        action="store_false",
        help="Delete temp working directories after runs (overrides config)",
    )
    return parser.parse_args()


def main() -> None:  # noqa: PLR0915
    """Run the epidemiology LLM agent experiment."""
    args = _parse_args()

    # Load base config from YAML
    cfg = _load_config(args.config)

    # Apply CLI overrides (only when explicitly provided)
    model_cfg = cfg.get("model", {})
    agent_cfg = cfg.get("agent", {})
    exp_cfg = cfg.get("experiment", {})

    provider = args.provider or model_cfg.get("provider", "ollama")
    model_id = args.model_id or model_cfg.get("model_id", "gemma4:31b")
    temperature = (
        args.temperature
        if args.temperature is not None
        else model_cfg.get("temperature", 0.8)
    )
    agent_type: Literal["tool_calling", "code"] = args.agent_type or agent_cfg.get(
        "type", "tool_calling"
    )
    runs = args.runs if args.runs is not None else exp_cfg.get("runs", 1)
    use_overrides = (
        args.use_overrides
        if args.use_overrides is not None
        else exp_cfg.get("use_overrides", False)
    )
    persist_context = (
        args.persist_context
        if args.persist_context is not None
        else exp_cfg.get("persist_context", True)
    )
    task_numbers = args.tasks or exp_cfg.get("tasks", [])

    api_key: str | None = None
    # Resolve model name and API key per provider
    if provider == "ollama":
        model_name = f"ollama_chat/{model_id}"
        api_key = "ollama"
    elif provider == "huggingface":
        model_name = f"huggingface/{model_id}"
        api_key = os.environ.get("HF_TOKEN")
    else:
        msg = f"Unknown provider: {provider}"
        raise ValueError(msg)

    logger.info(
        f"Config: provider={provider}, model={model_id}, agent_type={agent_type}, "
        f"temperature={temperature}, runs={runs}, use_overrides={use_overrides}, "
        f"persist_context={persist_context}, tasks={task_numbers or 'all'}"
    )

    tools: list[Callable] = (
        [produce_and_execute_r] if agent_type == "tool_calling" else []
    )

    agent = SmolAgent(
        tools=tools,
        model_name=model_name,
        api_key=api_key,
        agent_type=agent_type,
        temperature=temperature,
    )

    tasks_path = Path("./ground_truth/tasks.yml")
    with tasks_path.open() as f:
        tasks = yaml.safe_load(f)["tasks"]

    ground_truth_dir = Path("./ground_truth")
    all_dirs = sorted(
        [
            p
            for p in ground_truth_dir.iterdir()
            if p.is_dir() and p.name.startswith("task") and p.name[4:].isdigit()
        ],
        key=lambda p: int(p.name[4:]),
    )
    test_dirs = (
        [Path(f"./ground_truth/task{x}") for x in task_numbers]
        if task_numbers
        else all_dirs
    )

    for i in range(runs):
        logger.info(f"\n\n=== Agent Run {i+1} ===")
        for test_dir in test_dirs:
            logger.info(f"\n=== Testing with context: {test_dir} ===")

            task_path = Path(test_dir) / "task.yml"
            with task_path.open() as f:
                task = yaml.safe_load(f)

            metadata_path = Path(test_dir) / "metadata.json"
            with metadata_path.open() as f:
                metadata = json.load(f)

            task_type = task.get("task_type", "unknown")
            override = task.get("override", None)
            additional_requirements = task.get("additional_requirements_lite", "")

            if use_overrides and override:
                prompt = override
            else:
                task_data = next(
                    (item for item in tasks if item.get("task_type") == task_type), None
                )
                if task_data is None:
                    logger.warning(f"No task data found for type: {task_type}")
                    continue

                prompt = task_data["prompt"]

            prompt = prompt.format(
                metadata=metadata,
                additional_requirements=additional_requirements,
            )

            result = agent.forward(
                prompt,
                context_path=Path(test_dir),
                persist_context=persist_context,
            )

            logger.info(f"\n=== Result for context: {test_dir} ===")
            logger.info(result)


if __name__ == "__main__":
    main()
