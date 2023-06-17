#!/bin/bash
#
# File: daily-tests.sh
# Author: Dudu, Gugz
# Created on: Mon 20 Jun 2022 02:14
# Copyright (C) 2022, Dudu, Gugz
#

RED='\033[0;31m'
GREEN='\033[1;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

i = 0
j = 0

mkdir -p run
for file in auto-tests/*; do
  if [ ! -d "$file" ]; then
        filename=$(basename -- "$file")
        extension="${filename##*.}"
        filename="${filename%.*}"
        ./mml --target asm auto-tests/$filename.mml > /dev/null 2>&1
        yasm -felf32 auto-tests/$filename.asm
        ld -m elf_i386 -o test $filename.o -lrts
        ./test > run/$filename.log
        rm auto-tests/$filename.asm
        rm $filename.o
        diff <( tr -d ' \n\t' <auto-tests/expected/$filename.out) <( tr -d ' \n\t' <run/$filename.log)
        rv_diff=$?
        if [ ${rv_diff} == 0 ]; then
            echo -e "${GREEN}Test $filename PASSED!\n${NC}"
            ((i++))
        else
            echo -e "${RED}Test $filename FAILURE!\n${NC}"
            ((j++))
        fi
  fi
done
echo -e "${GREEN}Passed $i tests!${NC}"
echo -e "${RED}Failed $j tests!${NC}"
echo -e "${YELLOW}PROGRESS: ${i}/$((i+j))${NC}"
rm -rf run