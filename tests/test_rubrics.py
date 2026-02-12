"""Tests for src/rubrics.py."""

import pytest
from pydantic import ValidationError

from src.rubrics import CodeQualityAssessment, CodeQualityRubric


class TestCodeQualityAssessment:
    """Tests for CodeQualityAssessment model."""

    def test_valid_assessment(self) -> None:
        """Test creating a valid assessment."""
        assessment = CodeQualityAssessment(
            functional_equivalence_score=8,
            readability_score=7,
            style_score=6,
            robustness_score=5,
            overall_quality_score=7,
            functionally_equivalent=True,
            explanation="Good code.",
        )
        assert assessment.functional_equivalence_score == 8
        assert assessment.functionally_equivalent is True

    def test_score_boundaries(self) -> None:
        """Test that scores must be between 0 and 10."""
        with pytest.raises(ValidationError):
            CodeQualityAssessment(
                functional_equivalence_score=11,
                readability_score=7,
                style_score=6,
                robustness_score=5,
                overall_quality_score=7,
                functionally_equivalent=True,
                explanation="Bad score.",
            )


class TestCodeQualityRubric:
    """Tests for CodeQualityRubric."""

    def test_init(self) -> None:
        """Test rubric initialisation creates parser and prompt."""
        rubric = CodeQualityRubric()
        assert rubric.parser is not None
        assert rubric.prompt is not None

    def test_system_message_contains_language_and_code(self) -> None:
        """Test that system_message includes the language and code."""
        rubric = CodeQualityRubric()
        msg = rubric.system_message(language="R", code1="x <- 1")
        assert "R" in msg
        assert "x <- 1" in msg

    def test_user_message_contains_code(self) -> None:
        """Test that user_message includes the code."""
        rubric = CodeQualityRubric()
        msg = rubric.user_message(code2="y <- 2")
        assert "y <- 2" in msg
        assert "Code to compare" in msg

    def test_parse_valid_json(self) -> None:
        """Test parsing valid JSON output."""
        rubric = CodeQualityRubric()
        valid_json = """{
            "functional_equivalence_score": 8,
            "readability_score": 7,
            "style_score": 6,
            "robustness_score": 5,
            "overall_quality_score": 7,
            "functionally_equivalent": true,
            "explanation": "Good code quality."
        }"""
        result = rubric.parse(valid_json)
        assert isinstance(result, CodeQualityAssessment)
        assert result.functional_equivalence_score == 8
