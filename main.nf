#!/usr/bin/env nextflow

nextflow.enable.dsl=2

include { Webscrape_KI_Routendatenbank } from "./modules/Webscrape_KI_Routendatenbank"

workflow {
    // start workflow
    Webscrape_KI_Routendatenbank()
}