# SPDX-FileCopyrightText: 2021 The ipkg-idr developers
#
# SPDX-License-Identifier: CC0-1.0

.PHONY: install-ipkg
install-ipkg:
	idris2 --install ipkg.ipkg

.PHONY: build-testrunner
build-testrunner: install-ipkg
	cd tests; idris2 --install tests.ipkg