.PHONY: all type-coverage coverage format check mypy pytype test merge-pyi clean

all: passfail

passfail: mypy pytype format check

test: pytest

qualitative: type-coverage coverage

SOURCES := $(shell find nri -name '*.py')

coverage:
	coverage run tests/test_*.py

type-coverage:
	mypy -p nri --strict --html-report coverage --no-incremental
	$(BROWSER) coverage/index.html

format:
	black nri

check:
	flake8

mypy:
	mypy -p nri --strict
	! rg "Any" -t py
	! rg "TODO" -t py
	! rg "  print\(" nri -t py
	! rg "^print\(" nri -t py

pytype:
	pytype nri -o .

pytest:
	py.test

merge-pyi: $(SOURCES)

nri/*.py: pyi
	merge-pyi -i $@ pyi/$@i

clean:
	rm -rf imports
	rm -f .ninja_*
	rm -f build.ninja
	rm -rf .pytype
	rm -rf .pytest_cache
	rm -rf .mypy_cache
	rm -rf nri.egg-info
