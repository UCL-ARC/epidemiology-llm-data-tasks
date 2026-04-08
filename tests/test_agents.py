"""Tests for src/agents.py."""

from pathlib import Path
from unittest.mock import Mock, patch

from src.agents import Agent, AgentResult, SmolAgent


class DummyAgent(Agent):
    """Concrete implementation of abstract Agent for testing."""

    def forward(self, *args, **kwargs):  # noqa: ANN002
        """Make dummy forward method."""
        return AgentResult(
            result="ok",
            state="success",
            time_taken=0.1,
            steps=1,
            token_usage=5,
        )


class TestAgentResult:
    """Tests for AgentResult datamodel."""

    def test_agent_result_fields(self):
        """AgentResult should store and expose fields correctly."""
        result = AgentResult(
            result="some output",
            state="success",
            time_taken=1.23,
            steps=3,
            token_usage=456,
        )

        assert result.result == "some output"
        assert result.state == "success"
        assert result.time_taken == 1.23
        assert result.steps == 3
        assert result.token_usage == 456


class TestInitialiseContext:
    """Tests for Agent._initialise_context."""

    @patch("shutil.copy2")
    @patch("shutil.copytree")
    def test_initialise_context_copies_files_and_dirs(
        self,
        mock_copytree,
        mock_copy2,
    ):
        """Should copy files and directories from context_path into temp_root/name."""
        base = DummyAgent()
        base.temp_root = Path("./tmp/test_context_root")

        # Fake directory contents
        file1 = Mock(spec=Path)
        file1.is_file.return_value = True
        file1.is_dir.return_value = False
        file1.name = "file1.txt"

        dir1 = Mock(spec=Path)
        dir1.is_file.return_value = False
        dir1.is_dir.return_value = True
        dir1.name = "dir1"

        context_path = Mock(spec=Path)
        context_path.name = "sample_context"
        # Make context_path.iterdir() yield our fake items
        context_path.iterdir.return_value = [file1, dir1]

        # Call the method under test
        temp_dir = base._initialise_context(context_path)

        # Assertions
        assert temp_dir.parent.resolve() == base.temp_root.resolve()
        assert temp_dir.name.startswith(f"{context_path.name}_")
        mock_copy2.assert_called_once_with(file1, temp_dir / file1.name)
        mock_copytree.assert_called_once_with(dir1, temp_dir / dir1.name)

    @patch("pathlib.Path.exists")
    @patch("pathlib.Path.mkdir")
    def test_initialise_context_removes_existing_temp_dir(
        self, mock_mkdir, mock_exists
    ):
        """Existing temp dir should be removed before recreation."""
        base = DummyAgent()
        base.temp_root = Path("./tmp/test_context_root")

        context_path = Mock(spec=Path)
        context_path.name = "sample_context"
        # No files/dirs to copy; we only care about the rmtree branch here
        context_path.iterdir.return_value = []

        mock_exists.return_value = True

        base._initialise_context(context_path)

        mock_mkdir.assert_called_once_with(parents=True, exist_ok=True)


class TestExecutionContext:
    """Tests for Agent._execution_context."""

    @patch("os.chdir")
    @patch.object(DummyAgent, "_initialise_context")
    def test_execution_context_with_context_path_cleans_up(
        self, mock_init_ctx, mock_chdir, tmp_path
    ):
        """With context_path, should chdir into temp dir and clean it up by default."""
        base = DummyAgent()
        base.temp_root = tmp_path / "agent_contexts"
        temp_dir = tmp_path / "agent_contexts" / "sample"
        mock_init_ctx.return_value = temp_dir

        # Ensure temp_dir exists so cleanup path is exercised
        temp_dir.mkdir(parents=True, exist_ok=True)

        original_cwd = Path.cwd()

        with base._execution_context(Path("some/context")) as work_dir:
            assert work_dir == temp_dir
            mock_init_ctx.assert_called_once()
            mock_chdir.assert_any_call(temp_dir)

        # After context, chdir should restore cwd and temp dir should be removed
        mock_chdir.assert_any_call(original_cwd)
        assert not temp_dir.exists()

    @patch("os.chdir")
    @patch.object(DummyAgent, "_initialise_context")
    def test_execution_context_persist_true_keeps_temp_dir(
        self, mock_init_ctx, mock_chdir, tmp_path
    ):
        """When persist=True, temp dir should not be removed."""
        base = DummyAgent()
        base.temp_root = tmp_path / "agent_contexts"
        temp_dir = tmp_path / "agent_contexts" / "sample"
        mock_init_ctx.return_value = temp_dir

        temp_dir.mkdir(parents=True, exist_ok=True)

        with base._execution_context(Path("some/context"), persist=True) as work_dir:
            assert work_dir == temp_dir

        assert temp_dir.exists()  # not removed

    def test_execution_context_without_context_path_uses_cwd(self):
        """
        When context_path=None, should just yield original cwd and not change
        dirs.
        """
        base = DummyAgent()
        original_cwd = Path.cwd()

        with base._execution_context(None) as work_dir:
            assert work_dir == original_cwd

        # No exception and cwd unchanged is enough; no need to mock os.chdir


class TestSmolAgentInit:
    """Tests for SmolAgent initialisation."""

    @patch("src.agents.ToolCallingAgent")
    @patch("src.agents.LiteLLMModel")
    def test_smolagent_initialises_model_and_agent(
        self, mock_lite_model, mock_tool_agent
    ):
        """
        SmolAgent should construct LiteLLMModel and ToolCallingAgent with correct
        args.
        """

        def dummy_tool(x: int) -> int:
            """
            Make dummy tool function for testing.

            Args:
                x: Input value.

            Returns:
                The output value.

            """
            return x

        tools = [dummy_tool]
        model_name = "test-model"
        api_key = "abc123"

        agent = SmolAgent(tools=tools, model_name=model_name, api_key=api_key)

        # Tools should be wrapped by smolagents.tool decorator into Tool objects
        assert len(agent.tools) == 1

        # LiteLLMModel called as expected
        mock_lite_model.assert_called_once_with(
            model_id=model_name, api_key=api_key, temperature=0.3
        )
        # ToolCallingAgent constructed with that model and our tools
        mock_tool_agent.assert_called_once()
        args, kwargs = mock_tool_agent.call_args
        assert kwargs["model"] == mock_lite_model.return_value
        assert kwargs["tools"] == agent.tools
        assert kwargs["stream_outputs"] is False
        assert kwargs["return_full_result"] is True


class TestSmolAgentForward:
    """Tests for SmolAgent.forward."""

    @patch("src.agents.ToolCallingAgent")
    @patch("src.agents.LiteLLMModel")
    def test_forward_uses_cwd_when_no_context(
        self, mock_lite_model, mock_tool_agent, tmp_path, monkeypatch
    ):
        """
        If context_path is None, forward should still work and not fail
        on context manager.
        """

        def dummy_tool(x: int) -> int:
            """
            Make dummy tool function for testing.

            Args:
                x: Input value.

            Returns:
                The output value.

            """
            return x

        tools = [dummy_tool]
        model_name = "test-model"

        mock_agent_instance = Mock()
        mock_tool_agent.return_value = mock_agent_instance

        fake_token_usage = Mock()
        fake_token_usage.total_tokens = 10

        fake_timing = Mock()
        fake_timing.duration = 0.2

        fake_result = Mock(
            output="ok",
            state="success",
            steps=[1],
            token_usage=fake_token_usage,
            timing=fake_timing,
        )

        mock_agent_instance.run.return_value = fake_result

        agent = SmolAgent(tools=tools, model_name=model_name, api_key=None)

        monkeypatch.chdir(tmp_path)

        res = agent.forward("task with no context", context_path=None)

        assert isinstance(res, AgentResult)
        assert res.result == "ok"
        assert res.steps == 1
        assert res.token_usage == 10
