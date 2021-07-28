# This msut be the first rule
C_FILES = $(shell find leetcode -type f -name *.c)
CPP_FILES = $(shell find leetcode -type f -name *.cpp)

usage:
	@echo "Please choose a rule:"
	@echo "  index - foobar"

index: 
	@echo "$(C_FILES)"

.PHONY: usage index