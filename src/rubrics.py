"""Rubrics for code comparison: Pydantic schemas + prompt templates kept together."""

from langchain_core.output_parsers import PydanticOutputParser
from langchain_core.prompts import PromptTemplate
from pydantic import BaseModel, Field


class CodeQualityAssessment(BaseModel):
    """Schema for LLM-scored code comparison."""

    functional_equivalence_score: int = Field(..., ge=0, le=10)
    readability_score: int = Field(..., ge=0, le=10)
    style_score: int = Field(..., ge=0, le=10)
    robustness_score: int = Field(..., ge=0, le=10)
    overall_quality_score: int = Field(..., ge=0, le=10)
    functionally_equivalent: bool
    explanation: str


# Keep the template string next to the model it's tied to
_CODE_QUALITY_TEMPLATE = """
Compare the following two {language} code snippets.

CODE_SNIPPET_1:
```{language}
{code1}
```

CODE_SNIPPET_2:
```{language}
{code2}
```

Evaluate them according to this rubric (scores must be integers 0-10):

- functional_equivalence_score
- readability_score
- style_score
- robustness_score
- overall_quality_score

Also decide:
- functionally_equivalent: true/false
- explanation: short explanation string.

{format_instructions}
"""


class CodeQualityRubric:
    """Bundles the Pydantic model and prompt template together."""

    schema = CodeQualityAssessment

    def __init__(self) -> None:
        """Initialise the rubric with parser and prompt."""
        self.parser = PydanticOutputParser(pydantic_object=self.schema)
        self.prompt = PromptTemplate(
            template=_CODE_QUALITY_TEMPLATE,
            input_variables=["language", "code1", "code2"],
            partial_variables={
                "format_instructions": self.parser.get_format_instructions()
            },
        )

    def format_prompt(self, language: str, code1: str, code2: str) -> str:
        """Return the fully formatted prompt string."""
        return self.prompt.format(language=language, code1=code1, code2=code2)

    def parse(self, raw_text: str) -> CodeQualityAssessment:
        """Parse raw LLM output into a CodeQualityAssessment."""
        return self.parser.parse(raw_text)

    def system_message(self, language: str) -> str:
        """Return the system message for the code review assistant."""
        return (
            f"You are a code-review assistant reviewing {language} code snippets. "
            "You MUST respond exactly in the format described in the user's instructions."
        )
