"""Rubrics for code comparison: Pydantic schemas + prompt templates kept together."""

from langchain_core.output_parsers import PydanticOutputParser
from langchain_core.prompts import PromptTemplate
from pydantic import BaseModel, Field


class CodeQualityAssessment(BaseModel):
    """Schema for LLM-scored code comparison."""

    functional_equivalence_score: int = Field(
        ...,
        ge=0,
        le=10,
        description="Degree to which the candidate code produces "
        "the same resulting dataset as the reference.",
    )
    readability_score: int = Field(
        ...,
        ge=0,
        le=10,
        description="Clarity and ease of understanding of the candidate code.",
    )
    style_score: int = Field(
        ...,
        ge=0,
        le=10,
        description="Adherence to idiomatic and professional coding style.",
    )
    robustness_score: int = Field(
        ...,
        ge=0,
        le=10,
        description="Ability of the candidate code to handle "
        "edge cases and real-world data issues.",
    )
    overall_quality_score: int = Field(
        ...,
        ge=0,
        le=10,
        description="Holistic quality judgment weighting "
        "functional correctness most heavily.",
    )
    functionally_equivalent: bool = Field(
        ...,
        description="True if differences are superficial "
        "and outcomes are effectively the same.",
    )
    explanation: str = Field(
        ...,
        description="Brief justification focusing on data correctness and longitudinal "
        "logic.",
    )


# Keep the template string next to the model it's tied to
_CODE_QUALITY_TEMPLATE = """
You are an expert code reviewer evaluating a candidate {language} \
data preprocessing script against a reference (ground-truth) script.

Context and assumptions:
- Both scripts preprocess longitudinal data.
- The goal is semantic equivalence of the resulting dataset, \
not syntactic similarity.
- Differences in variable names, column names, intermediate representations, \
ordering of operations, or library idioms are expected and acceptable. - \
Difference in categorical names are also acceptable as long as the underlying \
data transformations are equivalent. - Focus on whether the same information is \
preserved and transformed correctly. Reference (ground-truth) \
code:
```{language} {code1}
```

Format instructions:

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
            input_variables=["language", "code1"],
            partial_variables={
                "format_instructions": self.parser.get_format_instructions()
            },
        )

    def system_message(self, language: str, code1: str) -> str:
        """Return the system message with the full rubric instructions."""
        return self.prompt.format(language=language, code1=code1)

    def user_message(self, code2: str) -> str:
        """Return the user message with the code to compare."""
        return f"Code to compare:\n```\n{code2}\n```"

    def parse(self, raw_text: str) -> CodeQualityAssessment:
        """Parse raw LLM output into a CodeQualityAssessment."""
        return self.parser.parse(raw_text)
