# Action: cranscraper

This action is used to update the [cran-to-git](https://github.com/r-universe-org/cran-to-git) registries. It scrapes all CRAN packages looking for a Git URLs in the package descriptions, and then saves json files grouped by owner.

This action is invoked every night in a cronjob in the [control-room](https://github.com/r-universe-org/control-room)
