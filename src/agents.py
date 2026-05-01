"""Agents for epidemiology LLM data tasks."""

# utils imports
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
from loguru import logger
from pydantic import BaseModel

# smolagent imports
from smolagents import CodeAgent, LiteLLMModel, ToolCallingAgent
from smolagents import tool as stool  # renamed to avoid conflict - teehee
from smolagents.tools import Tool

from src.config import AGENT_CONTEXTS_ROOT, SMOLAGENT_CONTEXT_ROOT
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
        self.temp_root = AGENT_CONTEXTS_ROOT

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


def _strip_raw(obj: object) -> object:
    """Recursively remove 'raw' keys from dicts to trim verbose API response objects."""
    if isinstance(obj, dict):
        return {k: _strip_raw(v) for k, v in obj.items() if k != "raw"}
    if isinstance(obj, list):
        return [_strip_raw(item) for item in obj]
    return obj


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
        self.temp_root = SMOLAGENT_CONTEXT_ROOT

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

            # Save full smolagents RunResult to the working directory
            result_path = working_dir / "runtime_data.json"
            result_path.write_text(
                json.dumps(_strip_raw(result.dict()), indent=2, default=str)
            )

            return agent_result
