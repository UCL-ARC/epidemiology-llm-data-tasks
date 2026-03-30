"""Report generation for data comparison results."""

import pandas as pd
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

    console.print(
        f"[bold]Task Completion Percentage:[/bold] \
            {result.task_completion_percentage:.1f}%\n"
    )

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
    match_table.add_column("Column Match Score", justify="right")
    match_table.add_column("Data Match", justify="right")
    match_table.add_column("Method", style="dim")

    # Build a lookup from gt_column to comparison for data_match
    comparison_lookup = {comp.gt_column: comp for comp in result.column_comparisons}

    for match in result.column_matches:
        if match.pred_column:
            score_style = (
                "green"
                if match.score >= 0.8  # noqa: PLR2004
                else "yellow"
                if match.score >= 0.5  # noqa: PLR2004
                else "red"
            )

            # TO DO: THIS COULD ALL BE SIMPLIFIED FOR SURE!
            # Look up data_match from the corresponding comparison
            comp = comparison_lookup.get(match.gt_column)
            data_match = None
            if comp:
                if comp.numeric_comparison:
                    data_match = comp.numeric_comparison.data_match
                elif comp.categorical_comparison:
                    data_match = comp.categorical_comparison.data_match

            data_match_score_style = (
                "green"
                if data_match is True
                else "red"
                if data_match is False
                else "dim"
            )
            match_table.add_row(
                match.gt_column,
                match.pred_column,
                f"[{score_style}]{match.score:.3f}[/{score_style}]",
                f"[{data_match_score_style}]{data_match if data_match is not None \
                                             else '-'}[/{data_match_score_style}]",
                match.method.value if match.method else "-",
            )
        else:
            match_table.add_row(
                match.gt_column,
                "[red](unmatched)[/red]",
                f"[red]{match.score:.3f}[/red]",
                "-",
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
            comp_table.add_row("NRMSE", f"{nc.nrmse:.4f}")
            comp_table.add_row("NMAE", f"{nc.nmae:.4f}")
            comp_table.add_row(
                "Correlation", f"{nc.correlation:.4f}" if nc.correlation else "N/A"
            )
            comp_table.add_row("GT mean / std", f"{nc.gt_mean:.4f} / {nc.gt_std:.4f}")
            comp_table.add_row(
                "Pred mean / std", f"{nc.pred_mean:.4f} / {nc.pred_std:.4f}"
            )
            comp_table.add_row(
                "Data Match",
                "[green]True[/green]"
                if nc.data_match
                else "[red]False[/red]"
                if nc.data_match is not None
                else "N/A",
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
            comp_table.add_row(
                "Data Match",
                "[green]True[/green]"
                if cc.data_match
                else "[red]False[/red]"
                if cc.data_match is not None
                else "N/A",
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


def aggregate_comparison_results(  # noqa: PLR0915, PLR0912
    results: list[tuple[str, DataComparisonResult, dict | None]],
) -> pd.DataFrame:
    """
    Aggregate comparison results across multiple samples into a summary dataframe.

    Args:
        results: List of (sample_name, DataComparisonResult, runtime_data) tuples.
            runtime_data is an optional dict with keys: token_usage, steps, time_taken.

    Returns:
        DataFrame with aggregated metrics for each sample plus an overall aggregate row.

    """
    results_summary = []

    for sample_name, result, runtime_data in results:
        # Count columns
        matched_cols = sum(1 for m in result.column_matches if m.pred_column)
        unmatched_gt_cols = len(result.unmatched_gt_columns)
        unmatched_pred_cols = len(result.unmatched_pred_columns)
        total_gt_cols = matched_cols + unmatched_gt_cols
        total_pred_cols = matched_cols + unmatched_pred_cols

        # Count matched column types
        matched_numeric_cols = sum(
            1 for comp in result.column_comparisons if comp.numeric_comparison
        )
        matched_categorical_cols = sum(
            1 for comp in result.column_comparisons if comp.categorical_comparison
        )

        # Count data matches for precision/recall/F1
        # TP: matched column with data_match = True
        # FP: matched column with no data_match = False
        # FN: unmatched GT columns
        true_positives = 0
        false_positives = 0
        for comp in result.column_comparisons:
            if comp.numeric_comparison:
                if comp.numeric_comparison.data_match is True:
                    true_positives += 1
                elif comp.numeric_comparison.data_match is False:
                    false_positives += 1
            elif comp.categorical_comparison:
                if comp.categorical_comparison.data_match is True:
                    true_positives += 1
                elif comp.categorical_comparison.data_match is False:
                    false_positives += 1

        false_negatives = unmatched_gt_cols

        # Calculate precision, recall, F1
        precision = (
            true_positives / (true_positives + false_positives)
            if (true_positives + false_positives) > 0
            else None
        )

        matched_with_data = true_positives + false_positives  # all matched columns
        recall = (
            matched_with_data / (matched_with_data + false_negatives)
            if (matched_with_data + false_negatives) > 0
            else None
        )

        f1_score = (
            2 * precision * recall / (precision + recall)
            if precision is not None and recall is not None and (precision + recall) > 0
            else None
        )

        # Calculate average numeric metrics
        numeric_rmse_values = [
            comp.numeric_comparison.rmse
            for comp in result.column_comparisons
            if comp.numeric_comparison
        ]
        numeric_corr_values = [
            comp.numeric_comparison.correlation
            for comp in result.column_comparisons
            if comp.numeric_comparison
            and comp.numeric_comparison.correlation is not None
        ]

        # Calculate average categorical metrics
        categorical_exact_match = [
            comp.categorical_comparison.exact_match_rate
            for comp in result.column_comparisons
            if comp.categorical_comparison
        ]
        categorical_overlap = [
            comp.categorical_comparison.category_overlap_score
            for comp in result.column_comparisons
            if comp.categorical_comparison
        ]

        results_summary.append(
            {
                "sample": sample_name,
                "gt_rows": result.join_completeness.gt_row_count,
                "pred_rows": result.join_completeness.pred_row_count,
                "joined_rows": result.join_completeness.joined_row_count,
                "row_completeness": result.join_completeness.completeness_score,
                "gt_cols": total_gt_cols,
                "pred_cols": total_pred_cols,
                "matched_cols": matched_cols,
                "matched_numeric_cols": matched_numeric_cols,
                "matched_categorical_cols": matched_categorical_cols,
                "unmatched_gt_cols": unmatched_gt_cols,
                "unmatched_pred_cols": unmatched_pred_cols,
                "col_match_rate": matched_cols / total_gt_cols
                if total_gt_cols > 0
                else 0,
                "true_positives": true_positives,
                "false_positives": false_positives,
                "false_negatives": false_negatives,
                "precision": precision,
                "recall": recall,
                "f1_score": f1_score,
                "avg_numeric_rmse": sum(numeric_rmse_values) / len(numeric_rmse_values)
                if numeric_rmse_values
                else None,
                "avg_numeric_corr": sum(numeric_corr_values) / len(numeric_corr_values)
                if numeric_corr_values
                else None,
                "avg_categorical_exact_match": sum(categorical_exact_match)
                / len(categorical_exact_match)
                if categorical_exact_match
                else None,
                "avg_categorical_overlap": sum(categorical_overlap)
                / len(categorical_overlap)
                if categorical_overlap
                else None,
                "task_completion_percentage": result.task_completion_percentage,
                "task_complete": result.task_completion_percentage == 100.0  # noqa: PLR2004
                if result.task_completion_percentage is not None
                else None,
                "token_usage": runtime_data.get("token_usage")
                if runtime_data
                else None,
                "steps": runtime_data.get("steps") if runtime_data else None,
                "time_taken": runtime_data.get("time_taken") if runtime_data else None,
            }
        )

    # Create summary dataframe
    summary_df = pd.DataFrame(results_summary)

    # Add aggregate row
    if len(summary_df) > 0:
        # Calculate aggregate precision/recall/F1 from totals (micro-averaging)
        total_tp = summary_df["true_positives"].sum()
        total_fp = summary_df["false_positives"].sum()
        total_fn = summary_df["false_negatives"].sum()

        agg_precision = (
            total_tp / (total_tp + total_fp) if (total_tp + total_fp) > 0 else None
        )

        total_matched = total_tp + total_fp
        agg_recall = (
            total_matched / (total_matched + total_fn)
            if (total_matched + total_fn) > 0
            else None
        )

        agg_f1 = (
            2 * agg_precision * agg_recall / (agg_precision + agg_recall)
            if agg_precision is not None
            and agg_recall is not None
            and (agg_precision + agg_recall) > 0
            else None
        )

        aggregate_row = {
            "sample": "AGGREGATE",
            "gt_rows": summary_df["gt_rows"].sum(),
            "pred_rows": summary_df["pred_rows"].sum(),
            "joined_rows": summary_df["joined_rows"].sum(),
            "row_completeness": summary_df["row_completeness"].mean(),
            "gt_cols": summary_df["gt_cols"].sum(),
            "pred_cols": summary_df["pred_cols"].sum(),
            "matched_cols": summary_df["matched_cols"].sum(),
            "matched_numeric_cols": summary_df["matched_numeric_cols"].sum(),
            "matched_categorical_cols": summary_df["matched_categorical_cols"].sum(),
            "unmatched_gt_cols": summary_df["unmatched_gt_cols"].sum(),
            "unmatched_pred_cols": summary_df["unmatched_pred_cols"].sum(),
            "col_match_rate": summary_df["col_match_rate"].mean(),
            "true_positives": total_tp,
            "false_positives": total_fp,
            "false_negatives": total_fn,
            "precision": agg_precision,
            "recall": agg_recall,
            "f1_score": agg_f1,
            "avg_numeric_rmse": summary_df["avg_numeric_rmse"].mean(),
            "avg_numeric_corr": summary_df["avg_numeric_corr"].mean(),
            "avg_categorical_exact_match": summary_df[
                "avg_categorical_exact_match"
            ].mean(),
            "avg_categorical_overlap": summary_df["avg_categorical_overlap"].mean(),
            "avg_task_completion_percentage": summary_df[
                "task_completion_percentage"
            ].mean(),
            "number_task_complete": summary_df["task_complete"].sum(),
            "total_token_usage": summary_df["token_usage"].sum(),
            "avg_token_usage": summary_df["token_usage"].mean(),
            "total_steps": summary_df["steps"].sum(),
            "avg_steps": summary_df["steps"].mean(),
            "total_time_taken": summary_df["time_taken"].sum(),
            "avg_time_taken": summary_df["time_taken"].mean(),
        }
        summary_df = pd.concat(
            [summary_df, pd.DataFrame([aggregate_row])], ignore_index=True
        )

        # Print aggregate metrics with rich formatting
        console = Console()
        console.print()
        console.rule("[bold blue]AGGREGATE EXPERIMENT METRICS[/bold blue]")

        metrics_table = Table(show_header=True, header_style="bold cyan")
        metrics_table.add_column("Metric", style="white")
        metrics_table.add_column("Value", justify="right")

        # Column counts
        metrics_table.add_row(
            "Total Matched Columns",
            f"[bold]{int(aggregate_row['matched_cols'])}[/bold]",
        )
        metrics_table.add_row(
            "  → Numeric Columns",
            f"[cyan]{int(aggregate_row['matched_numeric_cols'])}[/cyan]",
        )
        metrics_table.add_row(
            "  → Categorical Columns",
            f"[cyan]{int(aggregate_row['matched_categorical_cols'])}[/cyan]",
        )

        # Column match rate
        col_match_rate = aggregate_row["col_match_rate"]
        match_style = (
            "green"
            if col_match_rate >= 0.8  # noqa: PLR2004
            else "yellow"
            if col_match_rate >= 0.5  # noqa: PLR2004
            else "red"
        )
        metrics_table.add_row(
            "Column Match Rate", f"[{match_style}]{col_match_rate:.1%}[/{match_style}]"
        )

        # Precision/Recall/F1 section
        metrics_table.add_row("", "")  # Spacer row
        metrics_table.add_row(
            "[bold]Data Match Metrics[/bold]", "[dim](TP/FP/FN based)[/dim]"
        )
        metrics_table.add_row("  True Positives", f"[green]{int(total_tp)}[/green]")
        metrics_table.add_row("  False Positives", f"[red]{int(total_fp)}[/red]")
        metrics_table.add_row("  False Negatives", f"[yellow]{int(total_fn)}[/yellow]")

        if agg_precision is not None:
            prec_style = (
                "green"
                if agg_precision >= 0.8  # noqa: PLR2004
                else "yellow"
                if agg_precision >= 0.5  # noqa: PLR2004
                else "red"
            )
            metrics_table.add_row(
                "  Precision", f"[{prec_style}]{agg_precision:.3f}[/{prec_style}]"
            )
        else:
            metrics_table.add_row("  Precision", "[dim]N/A[/dim]")

        if agg_recall is not None:
            recall_style = (
                "green"
                if agg_recall >= 0.8  # noqa: PLR2004
                else "yellow"
                if agg_recall >= 0.5  # noqa: PLR2004
                else "red"
            )
            metrics_table.add_row(
                "  Recall", f"[{recall_style}]{agg_recall:.3f}[/{recall_style}]"
            )
        else:
            metrics_table.add_row("  Recall", "[dim]N/A[/dim]")

        if agg_f1 is not None:
            f1_style = (
                "green" if agg_f1 >= 0.8 else "yellow" if agg_f1 >= 0.5 else "red"  # noqa: PLR2004
            )
            metrics_table.add_row(
                "  F1 Score", f"[{f1_style}]{agg_f1:.3f}[/{f1_style}]"
            )
        else:
            metrics_table.add_row("  F1 Score", "[dim]N/A[/dim]")

        metrics_table.add_row("", "")  # Spacer row

        # Numeric RMSE
        if pd.notna(aggregate_row["avg_numeric_rmse"]):
            metrics_table.add_row(
                "Avg Numeric RMSE", f"{aggregate_row['avg_numeric_rmse']:.4f}"
            )
        else:
            metrics_table.add_row("Avg Numeric RMSE", "[dim]N/A[/dim]")

        # Numeric Correlation
        if pd.notna(aggregate_row["avg_numeric_corr"]):
            corr = aggregate_row["avg_numeric_corr"]
            corr_style = "green" if corr >= 0.8 else "yellow" if corr >= 0.5 else "red"  # noqa: PLR2004
            metrics_table.add_row(
                "Avg Numeric Correlation", f"[{corr_style}]{corr:.4f}[/{corr_style}]"
            )
        else:
            metrics_table.add_row("Avg Numeric Correlation", "[dim]N/A[/dim]")

        # Categorical exact match
        if pd.notna(aggregate_row["avg_categorical_exact_match"]):
            exact_match = aggregate_row["avg_categorical_exact_match"]
            exact_style = (
                "green"
                if exact_match >= 0.9  # noqa: PLR2004
                else "yellow"
                if exact_match >= 0.7  # noqa: PLR2004
                else "red"
            )
            metrics_table.add_row(
                "Avg Categorical Exact Match",
                f"[{exact_style}]{exact_match:.1%}[/{exact_style}]",
            )
        else:
            metrics_table.add_row("Avg Categorical Exact Match", "[dim]N/A[/dim]")

        # Categorical overlap
        if pd.notna(aggregate_row["avg_categorical_overlap"]):
            overlap = aggregate_row["avg_categorical_overlap"]
            overlap_style = (
                "green" if overlap >= 0.8 else "yellow" if overlap >= 0.5 else "red"  # noqa: PLR2004
            )
            metrics_table.add_row(
                "Avg Categorical Overlap",
                f"[{overlap_style}]{overlap:.3f}[/{overlap_style}]",
            )
        else:
            metrics_table.add_row("Avg Categorical Overlap", "[dim]N/A[/dim]")

        metrics_table.add_row("", "")  # Spacer row
        completion_style = (
            "green"
            if aggregate_row["avg_task_completion_percentage"] >= 90.0  # noqa: PLR2004
            else "yellow"
            if aggregate_row["avg_task_completion_percentage"] >= 60.0  # noqa: PLR2004
            else "red"
        )
        metrics_table.add_row(
            "Avg task completion percentage",
            f"[{completion_style}]{aggregate_row['avg_task_completion_percentage']:.1f}%[/{completion_style}]",
        )
        metrics_table.add_row(
            "Number of fully complete tasks",
            f"{int(aggregate_row['number_task_complete'])}",
        )

        console.print(metrics_table)
        console.rule("[bold blue]END AGGREGATE METRICS[/bold blue]")
        console.print()

    return summary_df
