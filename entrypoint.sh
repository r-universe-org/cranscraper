#!/bin/bash -l
set -e
UPDATE="${1}"

case $UPDATE in
  crantogit)
    echo "Running action to update crantogit.csv"
    Rscript -e "cranscraper::update_crantogit_csv()"
    ;;
  maintainers)
    echo "Running action to update maintainers.csv"
    Rscript -e "cranscraper::update_maintainers_csv()"
    ;;
  *)
    echo "Unknown action specified: $UPDATE"
    exit 1
    ;;
esac
echo "Action complete!"
