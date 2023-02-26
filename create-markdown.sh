#!/bin/env bash

# Create markdown file linking to figure for easier display on github

# execute this file within respective project folder!

# save name of single visualization
file=$(find *.png)

touch readme.md
echo "![figure](${file})" > readme.md
