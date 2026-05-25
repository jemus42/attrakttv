.PHONY: launch launch-dev install document check

PORT ?= 7842
DEV_DB ?= /tmp/attrakttv-smoke-db

# Launch the currently installed attrakttv against the default user db.
launch:
	Rscript -e 'attrakttv::attrakttv_app(launch.browser = TRUE)'

# Dev launch: load_all the source tree, use a throwaway db.
# Use after edits when you don't want to reinstall.
launch-dev:
	mkdir -p $(DEV_DB)
	trakt_db_path=$(DEV_DB) Rscript -e 'devtools::load_all("."); attrakttv_app(port = $(PORT), launch.browser = TRUE)'

# Install the local source as a package.
install:
	Rscript -e 'pak::local_install(".", ask = FALSE, upgrade = FALSE)'

# Re-roxygenize NAMESPACE + man/.
document:
	Rscript -e 'devtools::document(".")'

# R CMD check.
check:
	Rscript -e 'devtools::check(".")'
