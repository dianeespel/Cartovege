#!/bin/bash
#SBATCH --job-name=NbPlots
#SBATCH --output=resultat.NbInitialPlots.txt
#SBATCH --chdir=/home/genouest/cnrs_umr6553/despel/CARTOVEGE/Gollum/
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=8G
#SBATCH --time=2-00:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=diane.espel@univ-rennes.fr


# ============ ENVIRONNEMENT ============


# active conda (active deja l'environnement R 4.2)
. /local/env/envconda.sh

#active l'environnement conda  cartovege_env_principal
conda activate cartovege_env_principal


# ============ LANCEMENT + LOGGING ============

echo "Début du script : $(date)" #affiche la date (debut de script)
echo "Nœud : $HOSTNAME"


echo "==========================================="

# run un script R du home en creant un fichier temporaire dans le scratch
TMPDIR=/scratch/despel/Tempo/ Rscript /home/genouest/cnrs_umr6553/despel/CARTOVEGE/scripts/ALL/1.4_Nb_initial_plots_per_class.R 


echo "==================================="
echo "Fin du script : $(date)" # re affiche la date pour savoir la duree du script (fin du script)

echo "------------------------------------"
echo "Résumé efficacité du job avec seff :"
seff $SLURM_JOB_ID