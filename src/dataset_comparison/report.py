"""Report generation for data comparison results."""

from rich.console import Console
from rich.panel import Panel
from rich.table import Table
from rich.text import Text

from .models import DataComparisonResult


def print_comparison_report(result: DataComparisonResult) -> None:  # noqa: PLR0915
    """Print a human-readable comparison report using rich."""
    console = Console()

    console.print()
    console.rule("[bold blue]DATA COMPARISON REPORT[/bold blue]")
    console.print(f"[bold]Primary Key Join:[/bold] {result.primary_key}\n")

    # Join Completeness Panel
    jc = result.join_completeness
    completeness_text = Text()
    completeness_text.append(f"Ground truth rows:    {jc.gt_row_count}\n")
    completeness_text.append(f"Predicted rows:       {jc.pred_row_count}\n")
    completeness_text.append(f"Joined rows:          {jc.joined_row_count}\n")
    completeness_text.append(
        f"Missing in pred:      {jc.missing_in_pred}",
        style="red" if jc.missing_in_pred > 0 else "green",
    )
    completeness_text.append("\n")
    completeness_text.append(
        f"Extra in pred:        {jc.extra_in_pred}",
        style="yellow" if jc.extra_in_pred > 0 else "green",
    )
    completeness_text.append("\n")
    completeness_text.append(
        f"Completeness:         {jc.completeness_score:.1%}",
        style="green" if jc.completeness_score == 1.0 else "yellow",
    )

    if jc.has_duplicates:
        completeness_text.append("\n\n")
        completeness_text.append("⚠ Duplicate keys detected!", style="bold red")

    console.print(
        Panel(
            completeness_text,
            title="[bold]Join Completeness[/bold]",
            border_style="blue",
        )
    )

    # Column Matches Table
    match_table = Table(
        title="Column Matches", show_header=True, header_style="bold cyan"
    )
    match_table.add_column("GT Column", style="white")
    match_table.add_column("Pred Column", style="white")
    match_table.add_column("Score", justify="right")
    match_table.add_column("Method", style="dim")

    for match in result.column_matches:
        if match.pred_column:
            score_style = (
                "green"
                if match.score >= 0.8  # noqa: PLR2004
                else "yellow"
                if match.score >= 0.5  # noqa: PLR2004
                else "red"
            )
            match_table.add_row(
                match.gt_column,
                match.pred_column,
                f"[{score_style}]{match.score:.3f}[/{score_style}]",
                match.method.value if match.method else "-",
            )
        else:
            match_table.add_row(
                match.gt_column,
                "[red](unmatched)[/red]",
                f"[red]{match.score:.3f}[/red]",
                "-",
            )

    console.print(match_table)
    console.print()

    # Column Comparisons
    for comp in result.column_comparisons:
        comp_table = Table(
            title=f"{comp.gt_column} ↔ {comp.pred_column} ({comp.column_type.value})",
            show_header=True,
            header_style="bold magenta",
        )
        comp_table.add_column("Metric", style="white")
        comp_table.add_column("Value", justify="right")

        if comp.numeric_comparison:
            nc = comp.numeric_comparison
            comp_table.add_row("RMSE", f"{nc.rmse:.4f}")
            comp_table.add_row("MAE", f"{nc.mae:.4f}")
            comp_table.add_row(
                "Correlation", f"{nc.correlation:.4f}" if nc.correlation else "N/A"
            )
            comp_table.add_row("GT mean / std", f"{nc.gt_mean:.4f} / {nc.gt_std:.4f}")
            comp_table.add_row(
                "Pred mean / std", f"{nc.pred_mean:.4f} / {nc.pred_std:.4f}"
            )

        if comp.categorical_comparison:
            cc = comp.categorical_comparison
            match_style = (
                "green"
                if cc.exact_match_rate >= 0.9  # noqa: PLR2004
                else "yellow"
                if cc.exact_match_rate >= 0.7  # noqa: PLR2004
                else "red"
            )
            comp_table.add_row(
                "Exact match rate",
                f"[{match_style}]{cc.exact_match_rate:.1%}[/{match_style}]",
            )
            comp_table.add_row("Category overlap", f"{cc.category_overlap_score:.3f}")
            comp_table.add_row(
                "Distribution similarity", f"{cc.distribution_similarity:.3f}"
            )

        console.print(comp_table)
        console.print()

    if result.unmatched_gt_columns:
        console.print(
            Panel(
                "\n".join(result.unmatched_gt_columns),
                title="[bold red]Unmatched GT Columns[/bold red]",
                border_style="red",
            )
        )

    if result.unmatched_pred_columns:
        console.print(
            Panel(
                "\n".join(result.unmatched_pred_columns),
                title="[bold yellow]Unmatched Pred Columns[/bold yellow]",
                border_style="yellow",
            )
        )

    console.rule("[bold blue]END REPORT[/bold blue]")
