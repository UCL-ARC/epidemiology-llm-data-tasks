"""Agents for epidemiology LLM data tasks."""

# utils imports
import json
import os
import shutil
from abc import ABC, abstractmethod
from collections.abc import Callable, Generator
from contextlib import contextmanager
from pathlib import Path

# data imports
import yaml
from loguru import logger
from pydantic import BaseModel

# smolagent imports
from smolagents import LiteLLMModel, ToolCallingAgent
from smolagents import tool as stool  # renamed to avoid conflict - teehee
from smolagents.tools import Tool


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
        raise NotImplementedError

    def _initialise_context(self, context_path: Path) -> Path:
        """
        Copy necessary files from context_path to a temporary directory
        for the agent to use.

        The final temp dir is: temp_root / context_path.name
        """
        temp_dir = self.temp_root / context_path.name

        if temp_dir.exists():
            shutil.rmtree(temp_dir)

        temp_dir.mkdir(parents=True, exist_ok=True)

        logger.info(f"Setting up context from: {context_path} -> {temp_dir}")
        for item in context_path.iterdir():
            if item.is_file():
                shutil.copy2(item, temp_dir / item.name)
                logger.info(f"Copied file: {item.name}")
            elif item.is_dir():
                shutil.copytree(item, temp_dir / item.name)
                logger.info(f"Copied directory: {item.name}")

        return temp_dir

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
        self, tools: list[Callable], model_name: str, api_key: str | None = None
    ) -> None:
        """Initialise the SmolAgent with tools and model configuration."""
        super().__init__()
        # Override temp_root for SmolAgent
        self.temp_root = Path("./tmp/smolagent_context")

        self.tools = self._initialise_tools(tools)
        self.model_name = model_name
        self.api_key = api_key
        self.agent = self._initialise_agent()

    def _initialise_tools(self, tools: list[Callable]) -> list[Tool]:
        """Wrap provided tool functions into smolagents Tool objects."""
        return [stool(t) for t in tools]

    def _initialise_agent(self) -> ToolCallingAgent:
        """Initialise the ToolCallingAgent with the specified model and tools."""
        llm_model = LiteLLMModel(model_id=self.model_name, api_key=self.api_key)
        return ToolCallingAgent(
            model=llm_model,
            tools=self.tools,
            stream_outputs=False,  # To do: this doesnt stop live printing
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
        ) as working_dir:  # noqa: F841
            logger.info(f"Running agent with prompt: {prompt}")

            # Run the agent
            result = self.agent.run(prompt)
            state = result.state
            output = result.output
            time_taken = result.timing.duration
            steps = len(result.steps)
            token_usage = result.token_usage.total_tokens
            return AgentResult(
                result=output,
                state=state,
                time_taken=time_taken,
                steps=steps,
                token_usage=token_usage,
            )


if __name__ == "__main__":
    # This is to demo how the pipeline would/could work
    from .tools import produce_and_execute_r

    tools = [produce_and_execute_r]
    model_id = "gpt-oss:120b-cloud"
    model_name = f"ollama_chat/{model_id}"
    api_key = "ollama"

    agent = SmolAgent(tools=tools, model_name=model_name, api_key=api_key)
    tasks_path = Path("./ground_truth/tasks.yml")
    with tasks_path.open() as f:
        tasks = yaml.safe_load(f)["tasks"]

    for test_dir in [
        "./ground_truth/sample1",
        "./ground_truth/sample2",
        "./ground_truth/sample3",
        "./ground_truth/sample4",
    ]:
        logger.info(f"\n=== Testing with context: {test_dir} ===")

        task_path = Path(test_dir) / "task.yml"
        with task_path.open() as f:
            task = yaml.safe_load(f)

        task_type = task.get("task_type", "unknown")
        # Get  the first item where key1 equals val
        task_data = next(
            (item for item in tasks if item.get("task_type") == task_type), None
        )

        if task_data is None:
            logger.warning(f"No task data found for type: {task_type}")
            continue

        prompt = task_data["prompt"]

        metadata_path = Path(test_dir) / "metadata.json"
        with metadata_path.open() as f:
            metadata = json.load(f)

        prompt = prompt.format(metadata=metadata)

        # Run agent with context
        result = agent.forward(
            prompt,
            context_path=Path(test_dir),
            persist_context=True,  # Set to True to inspect the temp directory
        )

        logger.info(f"\n=== Result for context: {test_dir} ===")
        logger.info(result)
