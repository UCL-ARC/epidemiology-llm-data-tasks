"""Agents for epidemiology LLM data tasks."""

# utils imports
import argparse
import json
import os
import shutil
import tempfile
from abc import ABC, abstractmethod
from collections.abc import Callable, Generator
from contextlib import contextmanager
from pathlib import Path
from typing import Literal

# data imports
import yaml
from loguru import logger
from pydantic import BaseModel

# smolagent imports
from smolagents import CodeAgent, LiteLLMModel, ToolCallingAgent
from smolagents import tool as stool  # renamed to avoid conflict - teehee
from smolagents.tools import Tool

from src.r_code_agent import create_r_code_agent


# TO DO: When more frameworks are supported be more specific here
class AgentResult(BaseModel):
    """Structured result from a SmolAgent run."""

    result: str
    state: str
    time_taken: float
    steps: int
    token_usage: int


class Agent(ABC):
    """Abstract base class for agents."""

    def __init__(self) -> None:
        """Initialise the Agent base class."""
        # Base temp directory root for all agents
        self.temp_root = Path("./tmp/agent_contexts")

    @abstractmethod
    def forward(self, *args, **kwargs) -> AgentResult:  # noqa: ANN002
        """Abstract method to run the agent."""
        ...

    def _initialise_context(self, context_path: Path) -> Path:
        """
        Copy necessary files from context_path to a temporary directory
        for the agent to use.

        The final temp dir is: temp_root / context_path.name
        """
        # Ensure base temp_root exists
        self.temp_root.mkdir(parents=True, exist_ok=True)

        # Create a unique temp directory inside temp_root
        temp_dir_path = Path(
            tempfile.mkdtemp(prefix=f"{context_path.name}_", dir=self.temp_root)
        )

        logger.info(f"Setting up context from: {context_path} -> {temp_dir_path}")
        for item in context_path.iterdir():
            dest = temp_dir_path / item.name
            if item.is_file():
                shutil.copy2(item, dest)
                logger.info(f"Copied file: {item.name}")
            elif item.is_dir():
                shutil.copytree(item, dest)
                logger.info(f"Copied directory: {item.name}")

        return temp_dir_path

    @contextmanager
    def _execution_context(
        self, context_path: Path | None, *, persist: bool = False
    ) -> Generator[Path, None, None]:
        """
        Context manager for temporary execution environment.

        Args:
            context_path: Path to directory to copy files from.
            persist: If True, don't delete temp dir after execution.

        Yields:
            Path: Path to the temporary directory.

        """
        original_cwd = Path.cwd()
        temp_dir: Path | None = None

        # If no context path provided, do nothing

        if context_path is None:
            logger.info("No context path provided, using current working directory.")
            try:
                yield original_cwd
            finally:
                logger.info("Exiting context manager, no changes made.")
            return  # Avoid falling through to the second try
        try:
            temp_dir = self._initialise_context(context_path)
            os.chdir(temp_dir)
            logger.info(f"Working directory changed to: {temp_dir}")
            yield temp_dir
        finally:
            os.chdir(original_cwd)
            logger.info(f"Working directory restored to: {original_cwd}")

            if not persist and temp_dir and temp_dir.exists():
                shutil.rmtree(temp_dir)
                logger.info(f"Cleaned up temporary context: {temp_dir}")
            elif persist and temp_dir:
                logger.info(f"Persisted temporary context: {temp_dir}")


class SmolAgent(Agent):
    """Agent using smolagents with temporary directory context management."""

    def __init__(
        self,
        tools: list[Callable],
        model_name: str,
        api_key: str | None = None,
        temperature: float = 0.3,
        agent_type: Literal["tool_calling", "code"] = "tool_calling",
    ) -> None:
        """Initialise the SmolAgent with tools and model configuration."""
        super().__init__()
        # Override temp_root for SmolAgent
        self.temp_root = Path("./tmp/smolagent_context")

        self.tools = self._initialise_tools(tools)
        self.model_name = model_name
        self.api_key = api_key
        self.agent_type = agent_type
        self.temperature = temperature
        self.agent = self._initialise_agent()

    def _initialise_tools(self, tools: list[Callable]) -> list[Tool]:
        """Wrap provided tool functions into smolagents Tool objects."""
        return [stool(t) for t in tools]

    def _initialise_agent(self) -> CodeAgent | ToolCallingAgent:
        """Initialise the agent with the specified model and tools."""
        llm_model = LiteLLMModel(
            model_id=self.model_name,
            api_key=self.api_key,
            temperature=self.temperature,
            # num_ctx=128000,
        )
        if self.agent_type == "code":
            return create_r_code_agent(
                model=llm_model,
                tools=self.tools,
                stream_outputs=False,
                return_full_result=True,
            )
        return ToolCallingAgent(
            model=llm_model,
            tools=self.tools,
            stream_outputs=False,
            return_full_result=True,
        )

    def forward(
        self,
        prompt: str,
        context_path: Path | None = None,
        *,
        persist_context: bool = False,
    ) -> AgentResult:
        """
        Run the agent in a temporary directory context.

        Args:
            prompt: The prompt to send to the agent
            context_path: Path to directory containing files to copy to context
            persist_context: If True, don't delete temp dir after execution

        Returns:
            result

        """
        with self._execution_context(
            context_path, persist=persist_context
        ) as working_dir:
            logger.info(f"Running agent with prompt: {prompt}")

            # Run the agent
            result = self.agent.run(prompt)
            state = result.state
            output = result.output
            time_taken = result.timing.duration
            steps = len(result.steps)
            token_usage = result.token_usage.total_tokens

            agent_result = AgentResult(
                result=output,
                state=state,
                time_taken=time_taken,
                steps=steps,
                token_usage=token_usage,
            )

            # Save agent result metadata to the working directory
            result_path = working_dir / "runtime_data.json"
            result_path.write_text(agent_result.model_dump_json(indent=2))

            return agent_result


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
        "--samples",
        type=int,
        nargs="+",
        help="Sample numbers to run, e.g. --samples 1 2 3 (overrides config)",
    )
    parser.add_argument(
        "--use_overrides",
        action="store_true",
        default=None,
        help="Use per-sample override prompts where present (overrides config)",
    )
    parser.add_argument(
        "--no_overrides",
        dest="use_overrides",
        action="store_false",
        help="Ignore per-sample override prompts (overrides config)",
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


if __name__ == "__main__":
    from .tools import produce_and_execute_r

    args = _parse_args()

    # Load base config from YAML
    cfg = _load_config(args.config)

    # Apply CLI overrides (only when explicitly provided)
    model_cfg = cfg.get("model", {})
    agent_cfg = cfg.get("agent", {})
    exp_cfg = cfg.get("experiment", {})

    provider = args.provider or model_cfg.get("provider", "ollama")
    model_id = args.model_id or model_cfg.get("model_id", "gemma4:31b-cloud")
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
    sample_numbers = args.samples or exp_cfg.get("samples", [])

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
        f"persist_context={persist_context}, samples={sample_numbers or 'all'}"
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
            if p.is_dir() and p.name.startswith("sample") and p.name[6:].isdigit()
        ],
        key=lambda p: int(p.name[6:]),
    )
    test_dirs = (
        [Path(f"./ground_truth/sample{x}") for x in sample_numbers]
        if sample_numbers
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
            additional_requirements = task.get("additional_requirements", "")

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
