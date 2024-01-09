#!/usr/bin/env python3

import os
import sys
from lcov_cobertura import LcovCobertura

LCOV_DATA_FILENAME = "lcov.info"

if not os.path.isfile(LCOV_DATA_FILENAME):
    sys.stderr.write("No coverage data in file: lcov.info\n")
    sys.exit(1)

lcov_data = open("lcov.info").read()
converter = LcovCobertura(lcov_data)
cobertura_xml = converter.convert()
print(cobertura_xml)
