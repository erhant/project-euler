#!make

C_FILES := $(shell find -type f -name "*.c")
CPP_FILES := $(shell find -type f -name "*.cpp")
ALL_FILES := $(C_FILES)
ALL_FILES += $(CPP_FILES)

usage:
	@echo "Please choose a rule:"
	@echo "  index - foobar"

# sed -e '/^$/,$d' $$f  | head -n -1 | tail -n +2 | awk '{printf "\t%s\n", $0}' >> INDEX.MD
index: $(ALL_FILES)
	rm -f INDEX.MD
	touch INDEX.MD 
	printf $< >> INDEX.MD
	echo ":" >> INDEX.MD
	sed -e '/^$$/,$$d' $<  | head -n -1 | tail -n +2 | awk '{printf "  %s\n", $$0}' >> INDEX.MD; 
	

.PHONY: usage cleanup

# sed -e '/^$/,$d' <filename> will get the comment above

## sed -e '/^$/,$d' leetcode/add-two-numbers.c | head -n -1 | tail -n +2 | awk '{printf "\t%s\n", $0}'