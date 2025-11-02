# Cartovege

The SCO-Cartovege project developed a decision support tool for the conservation of flora and habitats of subantarctic islands, by combining vegetation surveys, satellite imagery and artificial intelligence modeling. The developed remote sensing R pipeline can be transferred to others territories. 

### **Cartovege** 🌿
_Automated pipeline for very high-resolution habitat mapping in remote environments_

Cartovege is an end-to-end, fully scripted pipeline developed in R to produce high-resolution habitat and vegetation maps from remote sensing data, with minimal human intervention.

Originally developed to address the challenges of mapping in isolated, cloud-prone insular regions (e.g., French subantarctic islands), the pipeline integrates:
- Orthorectified multispectral imagery
- Digital elevation model
- Vector mask of region of interest
- Field habitat data (in situ and/or photo-interpreted)

It uses a **Random Forest** supervised classification framework and supports hierarchical habitat typologies (up to _N_ levels) and the calculation of landscape metrics and diversity indices for ecological monitoring.

🛠️ Key features

- Fully automated and reproducible workflow
- Adaptable to various datasets, spatial resolutions, and ecological contexts
- Transparent script-based implementation (no GUI required)
- RMarkdown tutorials


This pipeline is open source but protected by a MIT Licence.

Please cite this pipeline using the following DOI: https://doi.org/10.5281/zenodo.15827741

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------

⚙️ Prerequisites

Make sure you have R 3.2 (or later) installed.

Clone the repository: 
```
git clone https://github.com/dianeespel/Cartovege.git
cd Cartovege
```

Set up your environment:
The _Requirements/_ folder contains the .yml files for the different environments used in the project.
Choose the one that fits your needs and create the environment using conda:
```
conda env create -f Requirements/environment_name.yml
conda activate environment_name
```


🚀 Usage

1) Data preparation:
Place your satellite image(s) (GeoTIFF) and reference polygons (Shapefile) in the respective folders.

2) Configuration:
Before each script edit the file to specify input and output paths

3) Run the pipeline:
Execute the main script from the terminal:
```
Rscript R_scripts/Cartovege_main_script.R
```



📂 Project structure
```
Cartovege/
│
├── data/                # Input satellite images and reference data
├── Requirements/        # Conda environments (.yml files)
├── R_scripts/           # Processing and analysis scripts
├── Rmarkdown_tutorials/ # R markdown files
├── LICENCE.txt          # MIT Licence file
└── README.md            # Project documentation
```




