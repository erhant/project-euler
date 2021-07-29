#!make

C_FILES := $(shell find -type f -name "*.c")
CPP_FILES := $(shell find -type f -name "*.cpp")
TS_FILES := $(shell find -type f -name "*.ts")
ALL_FILES := $(C_FILES)
ALL_FILES += $(CPP_FILES)
ALL_FILES += $(TS_FILES)
INDEX_FILE := index.yaml

# sed -e '/^$/,$d' $$f  | head -n -1 | tail -n +2 | awk '{printf "\t%s\n", $0}' >> $(INDEX_FILE)
# gets the first lines until blank, trims first and last lines, adds 2 spaces before, append to index
index: $(ALL_FILES)
	@rm -f $(INDEX_FILE) 
	@echo "---" >> $(INDEX_FILE) 
	@for f in $^; do \
		printf $$f >> $(INDEX_FILE); \
		echo ":" >> $(INDEX_FILE); \
		sed -e '/^$$/,$$d' $$f  | head -n -1 | tail -n +2 | awk '{printf "  %s\n", $$0}' >> $(INDEX_FILE); \
	done
	@echo "$(INDEX_FILE) created!"
	@yamllint $(INDEX_FILE)
	