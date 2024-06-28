#!/bin/bash

#update submodule to latest release
git submodule update --remote --merge

#copy functions from submodule to package folder
cp ODCdqa/src/functions.R R/functions.R