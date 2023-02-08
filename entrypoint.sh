#!/bin/bash -l
set -e
Rscript -e "cranscraper::ci_update_all()"
echo "Action complete!"
