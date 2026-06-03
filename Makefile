.PHONY: launch launch-dev refresh-tick redeploy \
	format doc document install deps check test clean

PORT ?= 7842
DEV_DB ?= /tmp/attrakttv-smoke-db

# ---- attrakttv-specific ----

# Launch the currently installed attrakttv against the default user db.
launch:
	Rscript -e 'attrakttv::attrakttv_app(launch.browser = TRUE)'

# Dev launch: load_all the source tree, use a throwaway db.
# Use after edits when you don't want to reinstall.
launch-dev:
	mkdir -p $(DEV_DB)
	trakt_db_path=$(DEV_DB) Rscript -e 'devtools::load_all("."); attrakttv_app(port = $(PORT), launch.browser = TRUE)'

# One-shot refresh tick (mirrors what the sidecar runs on a loop).
# Reads ATTRAKTTV_CONFIG or the bundled defaults.
refresh-tick:
	Rscript -e 'attrakttv::cache_refresh_tick()'

# ---- Deploy (run on horst) ----

# Pull image layers as needed, rebuild app + sidecar, and bring them up.
# This is the one compose invocation that doesn't have a shell alias.
redeploy:
	docker compose up -d --build

# ---- Standard R-package targets (mirrors $SYNCBIN/R/Makefile) ----

format:
	air format .

doc: format
	Rscript -e 'devtools::document(".")'

# Alias for muscle memory.
document: doc

install:
	Rscript -e 'pak::local_install(".", ask = FALSE, upgrade = FALSE)'

deps:
	Rscript -e 'pak::local_install_dev_deps()'

check:
	Rscript -e 'devtools::check(".")'

test:
	Rscript -e 'devtools::test(reporter = "summary")'

clean:
	-rm -f man/*.Rd NAMESPACE
	$(MAKE) doc
