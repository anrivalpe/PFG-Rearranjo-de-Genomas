from pathlib import Path
import csv
import re
import shutil


ROOT = Path("/home/c-ec2022/ra244185/PFG-Rearranjo-de-Genomas")

INPUT_DIRS = {
    "SREV": ROOT / "SREV",
    "SREV_TRANS": ROOT / "SREV_TRANS",
}

OUTPUT_ROOT = ROOT / "data" / "processed"
MANIFEST_PATH = OUTPUT_ROOT / "manifest.csv"


# Exemplo:
# N500_SREV50_I10_L100_F0.txt
# N500_SREV_TRANS50_I10_L100_F0.txt
FILE_PATTERN = re.compile(
    r"^N(?P<N>\d+)_"
    r"(?P<dataset>SREV_TRANS|SREV)"
    r"(?P<R>\d+)_"
    r"I(?P<I>\d+)_"
    r"L(?P<L>\d+)_"
    r"F(?P<F>\d+)\.txt$"
)

def parse_filename(filename: str) -> dict:
    """
    Extrai os parâmetros do nome do arquivo.
    """

    match = FILE_PATTERN.match(filename)

    if not match:
        raise ValueError(f"Nome de arquivo inesperado: {filename}")

    data = match.groupdict()

    return {
        "N": int(data["N"]),
        "dataset": data["dataset"],
        "R": int(data["R"]),
        "I": int(data["I"]),
        "L": int(data["L"]),
        "F": int(data["F"]),
    }


def parse_genome_line(line: str, line_number: int, filepath: Path) -> list[int]:
    """
    Converte uma linha como:

        L 1 -2 3 -4 ... ;

    em:

        [1, -2, 3, -4, ...]

    """

    tokens = line.replace(";", " ").split()

    if not tokens:
        raise ValueError(
            f"{filepath}: linha {line_number} está vazia."
        )

    if tokens[0] != "L":
        raise ValueError(
            f"{filepath}: linha {line_number} deveria começar com 'L'. "
            f"Encontrado: {tokens[0]}"
        )

    try:
        genome = [int(x) for x in tokens[1:]]
    except ValueError as exc:
        raise ValueError(
            f"{filepath}: valor não inteiro na linha {line_number}."
        ) from exc

    if not genome:
        raise ValueError(
            f"{filepath}: nenhum gene encontrado na linha {line_number}."
        )

    return genome


# Processamento dos arquivo

def process_file(
    filepath: Path,
    output_dir: Path,
    global_instance_id: int,
):
    """
    Lê um arquivo F0.

    Linhas originais:
        1 -> genoma A1
        2 -> região intergênica
        3 -> genoma B1
        4 -> região intergênica

        5 -> genoma A2
        6 -> região intergênica
        7 -> genoma B2
        8 -> região intergênica
        ...

    Retorna:
        linhas do manifest
        próximo ID global
    """

    metadata = parse_filename(filepath.name)

    # Segurança adicional:
    # este programa não deveria processar F diferente de zero.
    if metadata["F"] != 0:
        return [], global_instance_id

    with filepath.open("r", encoding="utf-8") as f:
        raw_lines = f.read().splitlines()

    if len(raw_lines) % 4 != 0:
        raise ValueError(
            f"{filepath}: esperado número de linhas múltiplo de 4, "
            f"mas encontrei {len(raw_lines)}."
        )

    # Ignorar linhas pares (contagem começa em 1)

    genome_lines = []

    for line_number, line in enumerate(raw_lines, start=1):

        # Regiões intergênicas
        if line_number % 2 == 0:
            continue

        genome = parse_genome_line(
            line=line,
            line_number=line_number,
            filepath=filepath,
        )

        genome_lines.append(
            (line_number, genome)
        )

    # Depois de eliminar linhas pares precisamos ter:
    #
    # A1, B1, A2, B2, ...
    #
    if len(genome_lines) % 2 != 0:
        raise ValueError(
            f"{filepath}: quantidade ímpar de genomas após "
            f"remover regiões intergênicas."
        )

    # Salva o arquivo limpado limpo

    output_dir.mkdir(parents=True, exist_ok=True)

    output_file = output_dir / filepath.name

    manifest_rows = []

    with output_file.open("w", encoding="utf-8") as out:

        pair_index = 0

        for i in range(0, len(genome_lines), 2):

            pair_index += 1

            line_a, genome_a = genome_lines[i]
            line_b, genome_b = genome_lines[i + 1]

            # Validação do tamanho.
            #
            # Nos arquivos verificados:
            # N200 -> 200 genes
            # N500 -> 500 genes
            #
            if len(genome_a) != metadata["N"]:
                raise ValueError(
                    f"{filepath}: instância {pair_index}: "
                    f"genoma A possui {len(genome_a)} genes, "
                    f"mas N={metadata['N']}."
                )

            if len(genome_b) != metadata["N"]:
                raise ValueError(
                    f"{filepath}: instância {pair_index}: "
                    f"genoma B possui {len(genome_b)} genes, "
                    f"mas N={metadata['N']}."
                )

            # Formato canônico:
            #
            # uma linha = um genoma
            #
            # linha 1 -> A1
            # linha 2 -> B1
            # linha 3 -> A2
            # linha 4 -> B2
            #
            out.write(" ".join(map(str, genome_a)) + "\n")
            out.write(" ".join(map(str, genome_b)) + "\n")

            instance_id = f"{metadata['dataset']}_{global_instance_id:06d}"

            manifest_rows.append(
                {
                    "instance_id": instance_id,
                    "dataset": metadata["dataset"],
                    "source_file": filepath.name,
                    "pair_index": pair_index,

                    "N": metadata["N"],
                    "R": metadata["R"],
                    "I": metadata["I"],
                    "L": metadata["L"],
                    "F": metadata["F"],

                    "original_line_A": line_a,
                    "original_line_B": line_b,

                    "len_A": len(genome_a),
                    "len_B": len(genome_b),

                    "unique_labels_A": len(
                        {abs(x) for x in genome_a}
                    ),
                    "unique_labels_B": len(
                        {abs(x) for x in genome_b}
                    ),

                    "max_label_A": max(abs(x) for x in genome_a),
                    "max_label_B": max(abs(x) for x in genome_b),

                    "processed_file": str(
                        output_file.relative_to(ROOT)
                    ),
                }
            )

            global_instance_id += 1

    return manifest_rows, global_instance_id


# MAIN

def main():

    print("=" * 70)
    print("PRÉ-PROCESSAMENTO DAS INSTÂNCIAS")
    print("=" * 70)

    # Validar diretórios

    for name, directory in INPUT_DIRS.items():

        if not directory.exists():
            raise FileNotFoundError(
                f"Pasta não encontrada: {directory}"
            )

        print(f"[OK] {name}: {directory}")

    # Recriar processed/

    if OUTPUT_ROOT.exists():
        print(f"\nRemovendo processamento anterior: {OUTPUT_ROOT}")
        shutil.rmtree(OUTPUT_ROOT)

    OUTPUT_ROOT.mkdir(parents=True)

    all_manifest_rows = []

    global_instance_id = 1

    # ========================================================
    # DATASETS
    # ========================================================

    for dataset_name, input_dir in INPUT_DIRS.items():

        output_dir = OUTPUT_ROOT / dataset_name

        # Somente F0
        files = sorted(input_dir.glob("*_F0.txt"))

        print()
        print("-" * 70)
        print(dataset_name)
        print("-" * 70)

        print(f"Arquivos F0 encontrados: {len(files)}")

        dataset_instances = 0

        for index, filepath in enumerate(files, start=1):

            rows, global_instance_id = process_file(
                filepath=filepath,
                output_dir=output_dir,
                global_instance_id=global_instance_id,
            )

            all_manifest_rows.extend(rows)

            dataset_instances += len(rows)

            if index % 25 == 0 or index == len(files):
                print(
                    f"  {index:3d}/{len(files)} arquivos processados "
                    f"({dataset_instances} instâncias)"
                )

        print(
            f"[OK] {dataset_name}: "
            f"{dataset_instances} pares de genomas"
        )

    # Manifest

    fieldnames = [
        "instance_id",
        "dataset",
        "source_file",
        "pair_index",
        "N",
        "R",
        "I",
        "L",
        "F",
        "original_line_A",
        "original_line_B",
        "len_A",
        "len_B",
        "unique_labels_A",
        "unique_labels_B",
        "max_label_A",
        "max_label_B",
        "processed_file",
    ]

    with MANIFEST_PATH.open(
        "w",
        newline="",
        encoding="utf-8",
    ) as csvfile:

        writer = csv.DictWriter(
            csvfile,
            fieldnames=fieldnames,
        )

        writer.writeheader()
        writer.writerows(all_manifest_rows)


    # Resumo e confirmação das etapas pra ver se deu certo 

    print()
    print("=" * 70)
    print("PROCESSAMENTO CONCLUÍDO")
    print("=" * 70)

    print(f"Total de instâncias: {len(all_manifest_rows)}")
    print(f"Manifest: {MANIFEST_PATH}")
    print(f"Dados processados: {OUTPUT_ROOT}")


if __name__ == "__main__":
    main()