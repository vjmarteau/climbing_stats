nextflow.enable.dsl=2

out_dir = file(params.resDir)
mode = params.publish_dir_mode

process Webscrape_KI_Routendatenbank {
    publishDir "${out_dir}", mode: "$mode"

    input:

    output:
        path("*-KI_Routendatenbank.csv"), emit: KI_Routendatenbank


	script:
	"""
    singularity exec \
    --no-home \
    -B in:/input_data \
    -B out:/results \
    -B $PWD \
    ./envs/selenium-firefox-standalone.sif \
    Webscrape_KI_Routendatenbank.R \
    /opt/Rscripts/Webscrape_KI_Routendatenbank.R \
    --results_dir=/results
	"""
}
