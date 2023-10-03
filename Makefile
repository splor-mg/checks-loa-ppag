.PHONY: all check check-r check-py

check: check-py check-r

check-r: 
	Rscript checks/rstats/testthat.R 2> logs/logs-rstats.txt

check-py:
	python -m pytest --log-file=logs/logs-python.txt --log-format='%(asctime)s %(levelname)-5.5s [%(name)s] %(message)s' --log-date-format='%Y-%m-%dT%H:%M:%S%z'
