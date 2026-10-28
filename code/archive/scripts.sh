# SLURM submission (JHPCE Rnosave wrapper) for 04_fui.R; run from code/2011_2014/.
# The second line is the same job with less memory.
Rnosave 04_fui.R -J FUI --mem=150G --time=7-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err
Rnosave 04_fui.R -J FUI --mem=30G --time=7-00 -o eofiles/%x_%A.out -e eofiles/%x_%A.err
