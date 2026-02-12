"""Tests for column_matcher module."""

from unittest.mock import MagicMock, patch

import pandas as pd
import pytest

from src.dataset_comparison.column_matcher import ColumnMatcher
from src.dataset_comparison.models import ColumnMatch, MatchMethod


# Maybe this test is not needed
class TestColumnMatcherInit:
    """Tests for ColumnMatcher initialisation."""

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_init_default_params(self, mock_cross_encoder: MagicMock) -> None:
        """Test initialisation with default parameters."""
        matcher = ColumnMatcher()

        mock_cross_encoder.assert_called_once_with("cross-encoder/stsb-roberta-base")
        assert matcher.match_threshold == 0.5

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_init_custom_params(self, mock_cross_encoder: MagicMock) -> None:
        """Test initialisation with custom parameters."""
        matcher = ColumnMatcher(
            cross_encoder_model_name="custom-model",
            match_threshold=0.7,
        )

        mock_cross_encoder.assert_called_once_with("custom-model")
        assert matcher.match_threshold == 0.7


class TestNormalise:
    """Tests for the _normalise static method."""

    def test_normalise_lowercase(self) -> None:
        """Test that strings are lowercased."""
        assert ColumnMatcher._normalise("HELLO") == "hello"
        assert ColumnMatcher._normalise("HeLLo WoRLd") == "hello world"

    def test_normalise_strips_whitespace(self) -> None:
        """Test that leading/trailing whitespace is stripped."""
        assert ColumnMatcher._normalise("  hello  ") == "hello"
        assert ColumnMatcher._normalise("\thello\n") == "hello"

    def test_normalise_replaces_underscores(self) -> None:
        """Test that underscores are replaced with spaces."""
        assert ColumnMatcher._normalise("hello_world") == "hello world"
        assert ColumnMatcher._normalise("one_two_three") == "one two three"

    def test_normalise_replaces_hyphens(self) -> None:
        """Test that hyphens are replaced with spaces."""
        assert ColumnMatcher._normalise("hello-world") == "hello world"
        assert ColumnMatcher._normalise("one-two-three") == "one two three"

    def test_normalise_combined(self) -> None:
        """Test normalisation with multiple transformations."""
        assert ColumnMatcher._normalise("  Hello_World-Test  ") == "hello world test"


class TestLevenshteinSimilarity:
    """Tests for the _levenshtein_similarity method."""

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_identical_strings(self, mock_cross_encoder: MagicMock) -> None:
        """Test that identical strings return 1.0."""
        matcher = ColumnMatcher()

        assert matcher._levenshtein_similarity("hello", "hello") == 1.0
        assert matcher._levenshtein_similarity("test", "test") == 1.0

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_completely_different_strings(self, mock_cross_encoder: MagicMock) -> None:
        """Test that completely different strings return low score."""
        matcher = ColumnMatcher()

        score = matcher._levenshtein_similarity("abc", "xyz")
        assert score == 0.0

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_similar_strings(self, mock_cross_encoder: MagicMock) -> None:
        """Test similarity for similar strings."""
        matcher = ColumnMatcher()

        # "hello" vs "hallo" - 1 character difference out of 5
        score = matcher._levenshtein_similarity("hello", "hallo")
        assert score == pytest.approx(0.8, abs=0.01)

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_normalisation_applied(self, mock_cross_encoder: MagicMock) -> None:
        """Test that normalisation is applied before comparison."""
        matcher = ColumnMatcher()

        # Should be identical after normalisation
        assert matcher._levenshtein_similarity("hello_world", "hello-world") == 1.0
        assert matcher._levenshtein_similarity("HELLO", "hello") == 1.0

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_empty_string(self, mock_cross_encoder: MagicMock) -> None:
        """Test that empty strings return 0.0."""
        matcher = ColumnMatcher()

        assert matcher._levenshtein_similarity("", "hello") == 0.0
        assert matcher._levenshtein_similarity("hello", "") == 0.0
        assert matcher._levenshtein_similarity("", "") == 0.0

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_whitespace_only_string(self, mock_cross_encoder: MagicMock) -> None:
        """Test that whitespace-only strings return 0.0 after normalisation."""
        matcher = ColumnMatcher()

        assert matcher._levenshtein_similarity("   ", "hello") == 0.0


class TestSemanticSimilarity:
    """Tests for the _semantic_similarity method."""

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_returns_clamped_score(self, mock_cross_encoder: MagicMock) -> None:
        """Test that scores are clamped to 0-1 range."""
        mock_instance = MagicMock()
        mock_cross_encoder.return_value = mock_instance

        matcher = ColumnMatcher()

        # Test normal score
        mock_instance.predict.return_value = 0.75
        assert matcher._semantic_similarity("a", "b") == 0.75

        # Test score above 1 is clamped
        mock_instance.predict.return_value = 1.5
        assert matcher._semantic_similarity("a", "b") == 1.0

        # Test negative score is clamped
        mock_instance.predict.return_value = -0.5
        assert matcher._semantic_similarity("a", "b") == 0.0

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_normalisation_applied(self, mock_cross_encoder: MagicMock) -> None:
        """Test that normalised strings are passed to the encoder."""
        mock_instance = MagicMock()
        mock_instance.predict.return_value = 0.5
        mock_cross_encoder.return_value = mock_instance

        matcher = ColumnMatcher()
        matcher._semantic_similarity("HELLO_WORLD", "hello-world")

        # Both should be normalised to "hello world"
        mock_instance.predict.assert_called_with([["hello world", "hello world"]])


class TestBestSimilarity:
    """Tests for the _best_similarity method."""

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_returns_levenshtein_when_higher(
        self, mock_cross_encoder: MagicMock
    ) -> None:
        """Test that Levenshtein score is returned when higher."""
        mock_instance = MagicMock()
        mock_instance.predict.return_value = 0.3  # Low semantic score
        mock_cross_encoder.return_value = mock_instance

        matcher = ColumnMatcher()

        # Identical strings = 1.0 Levenshtein
        score, method = matcher._best_similarity("test", "test", semantic_weighting=1.0)

        assert score == 1.0
        assert method == MatchMethod.LEVENSHTEIN

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_returns_semantic_when_higher(self, mock_cross_encoder: MagicMock) -> None:
        """Test that semantic score is returned when higher."""
        mock_instance = MagicMock()
        mock_instance.predict.return_value = 0.9  # High semantic score
        mock_cross_encoder.return_value = mock_instance

        matcher = ColumnMatcher()

        # Very different strings = low Levenshtein
        score, method = matcher._best_similarity(
            "age", "years_old", semantic_weighting=1.0
        )

        assert score == 0.9
        assert method == MatchMethod.SEMANTIC

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_semantic_weighting_applied(self, mock_cross_encoder: MagicMock) -> None:
        """Test that semantic weighting is applied correctly."""
        mock_instance = MagicMock()
        mock_instance.predict.return_value = 0.8
        mock_cross_encoder.return_value = mock_instance

        matcher = ColumnMatcher()

        # With weighting of 0.5, semantic score becomes 0.4
        score, method = matcher._best_similarity("abc", "xyz", semantic_weighting=0.5)

        # Levenshtein of "abc" vs "xyz" is 0.0, weighted semantic is 0.4
        assert score == pytest.approx(0.4, abs=0.01)
        assert method == MatchMethod.SEMANTIC

    # TO DO: this test is a bit pointless
    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_levenshtein_preferred_on_tie(self, mock_cross_encoder: MagicMock) -> None:
        """Test that Levenshtein is preferred when scores are equal."""
        mock_instance = MagicMock()
        mock_instance.predict.return_value = 0.8
        mock_cross_encoder.return_value = mock_instance

        matcher = ColumnMatcher()

        # Create a scenario where both might be equal
        # "test" vs "tast" = 0.75 Levenshtein
        # Set semantic to return 0.75
        mock_instance.predict.return_value = 0.75

        score, method = matcher._best_similarity("test", "tast", semantic_weighting=1.0)

        # Levenshtein should be preferred (>=)
        assert method == MatchMethod.LEVENSHTEIN


class TestComputeSimilarityMatrix:
    """Tests for the _compute_similarity_matrix method."""

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_computes_all_pairs(self, mock_cross_encoder: MagicMock) -> None:
        """Test that similarity is computed for all column pairs."""
        mock_instance = MagicMock()
        mock_instance.predict.return_value = 0.5
        mock_cross_encoder.return_value = mock_instance

        matcher = ColumnMatcher()

        gt_columns = ["a", "b"]
        pred_columns = ["x", "y", "z"]

        matrix = matcher._compute_similarity_matrix(gt_columns, pred_columns)

        # Check matrix structure
        assert set(matrix.keys()) == {"a", "b"}
        assert set(matrix["a"].keys()) == {"x", "y", "z"}
        assert set(matrix["b"].keys()) == {"x", "y", "z"}

        # Check each entry is a tuple of (score, method)
        for gt_col in gt_columns:
            for pred_col in pred_columns:
                score, method = matrix[gt_col][pred_col]
                assert isinstance(score, float)
                assert isinstance(method, MatchMethod)

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_empty_columns(self, mock_cross_encoder: MagicMock) -> None:
        """Test with empty column lists."""
        matcher = ColumnMatcher()

        matrix = matcher._compute_similarity_matrix([], [])
        assert matrix == {}

        matrix = matcher._compute_similarity_matrix(["a"], [])
        assert matrix == {"a": {}}


class TestMatchColumns:
    """Tests for the match_columns method."""

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_exact_matches(self, mock_cross_encoder: MagicMock) -> None:
        """Test matching with identical column names."""
        mock_instance = MagicMock()
        mock_instance.predict.return_value = 0.5
        mock_cross_encoder.return_value = mock_instance

        matcher = ColumnMatcher(match_threshold=0.5)

        gt_df = pd.DataFrame({"name": [], "age": [], "city": []})
        pred_df = pd.DataFrame({"name": [], "age": [], "city": []})

        matches = matcher.match_columns(gt_df, pred_df)

        assert len(matches) == 3
        matched_pairs = {(m.gt_column, m.pred_column) for m in matches}
        assert ("name", "name") in matched_pairs
        assert ("age", "age") in matched_pairs
        assert ("city", "city") in matched_pairs

        for match in matches:
            assert match.score == 1.0
            assert match.method == MatchMethod.LEVENSHTEIN

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_no_matches_below_threshold(self, mock_cross_encoder: MagicMock) -> None:
        """Test that no matches are made below threshold."""
        mock_instance = MagicMock()
        mock_instance.predict.return_value = 0.1  # Low semantic similarity
        mock_cross_encoder.return_value = mock_instance

        matcher = ColumnMatcher(match_threshold=0.9)

        gt_df = pd.DataFrame({"abc": []})
        pred_df = pd.DataFrame({"xyz": []})

        matches = matcher.match_columns(gt_df, pred_df)

        assert len(matches) == 1
        assert matches[0].gt_column == "abc"
        assert matches[0].pred_column is None
        assert matches[0].method is None

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_greedy_matching(self, mock_cross_encoder: MagicMock) -> None:
        """Test that greedy algorithm assigns best matches first."""
        mock_instance = MagicMock()
        # Make semantic scores low so Levenshtein dominates
        mock_instance.predict.return_value = 0.0
        mock_cross_encoder.return_value = mock_instance

        matcher = ColumnMatcher(match_threshold=0.5)

        # "test" matches "test" perfectly (1.0)
        # "tast" matches "test" at 0.75, "tost" at 0.75
        gt_df = pd.DataFrame({"test": [], "tast": []})
        pred_df = pd.DataFrame({"test": [], "tost": []})

        matches = matcher.match_columns(gt_df, pred_df)

        # "test" should get "test" (best match overall)
        test_match = next(m for m in matches if m.gt_column == "test")
        assert test_match.pred_column == "test"

        # "tast" should get "tost" (only remaining option)
        tast_match = next(m for m in matches if m.gt_column == "tast")
        assert tast_match.pred_column == "tost"

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_more_gt_than_pred_columns(self, mock_cross_encoder: MagicMock) -> None:
        """Test when GT has more columns than pred."""
        mock_instance = MagicMock()
        mock_instance.predict.return_value = 0.0
        mock_cross_encoder.return_value = mock_instance

        matcher = ColumnMatcher(match_threshold=0.5)

        gt_df = pd.DataFrame({"a": [], "b": [], "c": []})
        pred_df = pd.DataFrame({"a": []})

        matches = matcher.match_columns(gt_df, pred_df)

        assert len(matches) == 3

        matched = [m for m in matches if m.pred_column is not None]
        unmatched = [m for m in matches if m.pred_column is None]

        assert len(matched) == 1
        assert len(unmatched) == 2

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_more_pred_than_gt_columns(self, mock_cross_encoder: MagicMock) -> None:
        """Test when pred has more columns than GT."""
        mock_instance = MagicMock()
        mock_instance.predict.return_value = 0.0
        mock_cross_encoder.return_value = mock_instance

        matcher = ColumnMatcher(match_threshold=0.5)

        gt_df = pd.DataFrame({"a": []})
        pred_df = pd.DataFrame({"a": [], "b": [], "c": []})

        matches = matcher.match_columns(gt_df, pred_df)

        # Should only have 1 match (for the 1 GT column)
        assert len(matches) == 1
        assert matches[0].gt_column == "a"
        assert matches[0].pred_column == "a"

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_semantic_weighting_parameter(self, mock_cross_encoder: MagicMock) -> None:
        """Test that semantic_weighting is passed through correctly."""
        mock_instance = MagicMock()
        mock_instance.predict.return_value = 0.8
        mock_cross_encoder.return_value = mock_instance

        matcher = ColumnMatcher(match_threshold=0.3)

        gt_df = pd.DataFrame({"abc": []})
        pred_df = pd.DataFrame({"xyz": []})

        # With weighting=0, semantic should be 0, so no match
        matches = matcher.match_columns(gt_df, pred_df, semantic_weighting=0.0)
        assert matches[0].pred_column is None

        # With weighting=1.0, semantic should be 0.8, so match
        matches = matcher.match_columns(gt_df, pred_df, semantic_weighting=1.0)
        assert matches[0].pred_column == "xyz"

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_returns_column_match_objects(self, mock_cross_encoder: MagicMock) -> None:
        """Test that results are ColumnMatch dataclass instances."""
        mock_instance = MagicMock()
        mock_instance.predict.return_value = 0.5
        mock_cross_encoder.return_value = mock_instance

        matcher = ColumnMatcher()

        gt_df = pd.DataFrame({"test": []})
        pred_df = pd.DataFrame({"test": []})

        matches = matcher.match_columns(gt_df, pred_df)

        assert len(matches) == 1
        assert isinstance(matches[0], ColumnMatch)
        assert matches[0].gt_column == "test"
        assert matches[0].pred_column == "test"
        assert isinstance(matches[0].score, float)
        assert isinstance(matches[0].method, MatchMethod)

    @patch("src.dataset_comparison.column_matcher.CrossEncoder")
    def test_empty_dataframes(self, mock_cross_encoder: MagicMock) -> None:
        """Test matching with empty dataframes."""
        matcher = ColumnMatcher()

        gt_df = pd.DataFrame()
        pred_df = pd.DataFrame()

        matches = matcher.match_columns(gt_df, pred_df)

        assert matches == []
