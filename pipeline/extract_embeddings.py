"""Extract text embeddings from medical procedures and diagnoses using Sentence Transformers"""

from argparse import (
    ArgumentParser,
    MetavarTypeHelpFormatter,
    ArgumentDefaultsHelpFormatter,
)

import polars as pl

try:
    import torch

    assert torch.cuda.is_available()
    print(f"Running torch v{torch.__version__}")
    n_devices = torch.cuda.device_count()
    print(f"Devices: {n_devices} CUDA device(s) available.")
except (ImportError, AssertionError) as err:
    raise ValueError("torch should be installed with CUDA enabled.") from err

from sentence_transformers import SentenceTransformer
from sklearn.decomposition import PCA


class ArgumentDefaultsMetavarTypeHelpFormatter(
    ArgumentDefaultsHelpFormatter, MetavarTypeHelpFormatter
):
    """Simple formatter with default values and metavar types."""


def parse_args():
    """Parse args"""
    parser = ArgumentParser(formatter_class=ArgumentDefaultsMetavarTypeHelpFormatter)
    parser.add_argument(
        "-i",
        dest="input_file",
        type=str,
        required=True,
        help="Path to the input parquet file without embeddings.",
    )
    parser.add_argument(
        "-o",
        dest="output_file",
        type=str,
        required=True,
        help="Path to the output parquet file with embeddings.",
    )
    parser.add_argument(
        "-n",
        dest="n_components",
        type=int,
        default=60,
        help="Number of PCA components to reduce the embeddings to.",
    )
    parser.add_argument(
        "-m",
        dest="model_name",
        type=str,
        default="minishlab/potion-multilingual-128M",
        help="Name of the sentence transformer model to use.",
    )
    parser.add_argument(
        "--batch_size",
        type=int,
        default=128,
        help="Batch size for embedding extraction.",
    )
    parser.add_argument(
        "--chunk_size",
        type=int,
        default=8192,
        help="Chunk size for embedding extraction.",
    )
    parser.add_argument(
        "--devices",
        type=str,
        nargs="+",
        default=None,
        help="List of devices to use (use all available CUDA devices if None).",
    )
    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()

    print("Loading data...")
    df = pl.scan_parquet(args.input_file)

    print(f"Number of samples: {df.select(pl.len()).collect().item():,}")

    print(df.head().collect())

    # Create the sentences for the medical procedures and previous diagnoses
    df = df.with_columns(
        [
            pl.concat_str(
                [
                    pl.lit("Tidligere medicinske procedurer: "),
                    pl.col("Aggregated_Procedures").fill_null("Ingen"),
                    pl.lit(" Tidligere diagnoser: "),
                    pl.col("Aggregated_Diagnoses").fill_null("Ingen"),
                ],
                separator="",
            ).alias("Aggregated_Information")
        ]
    ).drop(["Aggregated_Procedures", "Aggregated_Diagnoses"])

    # Select the column of aggregated information
    agg_info = df.select("Aggregated_Information").collect().to_series().to_list()

    print(f"Example sentences\n{agg_info[:5]}")

    # Determine devices to use
    if args.devices is not None:
        devices = args.devices
    else:
        devices = [f"cuda:{i}" for i in range(n_devices)]

    # Extract embeddings
    print(f"Loading model {args.model_name}...")
    model = SentenceTransformer(args.model_name, device=devices[0])

    print("Extracting embeddings...")
    embeddings = model.encode(
        agg_info,
        show_progress_bar=True,
        device=devices,
        batch_size=args.batch_size,
        chunk_size=args.chunk_size,
    )
    print(f"Embeddings shape: {embeddings.shape}")

    # Principle Component Analysis (PCA) for dimensionality reduction
    print("Performing PCA...")
    pca = PCA(n_components=args.n_components)
    embeddings_pca = pca.fit_transform(embeddings)
    print(f"Reduced embeddings shape: {embeddings_pca.shape}")

    total_variance_explained = pca.explained_variance_ratio_.sum()
    print(
        (
            f"Explained variance ratio for {args.n_components} components: "
            f"{total_variance_explained*100:.2f}%)"
        )
    )
    print("First 10 explained variance ratios:")
    print(pca.explained_variance_ratio_[:10])

    # Merge data with reduced embeddings
    print("Merging data with reduced embeddings...")
    embedding_pca_df = pl.LazyFrame(embeddings_pca).rename(
        lambda x: x.replace("column", "pca")
    )
    df_full = pl.concat([df, embedding_pca_df], how="horizontal")

    print("Writing output...")
    df_full.sink_parquet(args.output_file)
