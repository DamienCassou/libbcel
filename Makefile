ELPA_DEPENDENCIES=package-lint request

ELPA_ARCHIVES=melpa gnu

TEST_ERT_FILES=$(wildcard test/*.el)
LINT_CHECKDOC_FILES=$(wildcard *.el) $(wildcard test/*.el)
LINT_PACKAGE_LINT_FILES=$(wildcard *.el) $(wildcard test/*.el)
LINT_COMPILE_FILES=$(wildcard *.el) $(wildcard test/*.el)

makel.mk:
	# Download makel
	@if [ -f ../makel/makel.mk ]; then \
		ln -s ../makel/makel.mk .; \
	else \
		curl \
		--fail --silent --show-error --insecure --location \
		--retry 9 --retry-delay 9 \
		-O https://raw.githubusercontent.com/DamienCassou/makel/v0.8.0/makel.mk; \
	fi

# Include makel.mk if present
-include makel.mk
