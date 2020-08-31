#!/bin/bash -l
set -e
Rscript -e "cranscraper::cran_registry_update_json()"
echo "Action complete!"
