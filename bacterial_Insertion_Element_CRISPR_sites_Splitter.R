library(Biostrings)
library(stringr)
library(parallel)
bacterial_Insertion_Element_CRISPR_sites_Splitter <- function(fasta_files, pattern) {
    # a bacterial insertion element scannner based on the string search
    # algorithm, it will check for the mentioned insertion element pattern
    # and then clip that pattern and will stitch the genome devoid of the
    # insertion elements.It will give you directly the fasta, you can also
    # write the fasta. I am making a insertion element package and associating
    # that to the microbiomes. You can also mention the CRISPR sites and to remove
    # those from the genome
    # to use you can use the function as
    # bacterial_Insertion_Element_Splitter(
    # "/Users/gauravsablok/Desktop/CodeBase/check.fasta",
    #     "GCAGCGTACGATATATAT")
      fasta_read <- readDNAStringSet(fasta_files)
    insertionelement <- pattern
    fasta_names <- names(fasta_read)
    fasta_sequences <- vector(length = length(fasta_names))
    fasta_insertion_element_remove <- vector(length = length(fasta_names))
    for (i in seq_along(fasta_read)) {
        fasta_sequences[i] <- str_split(
            as.vector(fasta_read[i]),
            insertionelement
        )
    }
    for (i in seq_along(fasta_sequences)) {
        fasta_insertion_element_remove[i] <- str_flatten(as.character(
            unlist(fasta_sequences[i],
                use.names = FALSE
            )
        ))
    }
    for (i in seq_along(fasta_names)) {
        print(paste0(">", fasta_names[i]))
        print(fasta_insertion_element_remove)
    }
}
# to apply across a large number of genomes or fasta files
# numCores <- detectCores()
# system.time(results <- mclapply(fasta_files,
#             bacterial_Insertion_Element_CRISPR_sites_Splitter(fasta_file, pattern), 
#                                                            mc.cores = numCores))
