#!make

FOLDERS := leetcode
INDEX_FILE := index.yaml

index: $(FOLDERS)
	@rm -f $(INDEX_FILE) 
	@echo "---" >> $(INDEX_FILE) 
	@for f in $^; do \
	  echo "$$f:" >> $(INDEX_FILE); \
		cd $$f; \
		make; \
		cd ..; \
		cat $$f/$(INDEX_FILE) | tail -n +2 | awk '{printf "  %s\n", $$0}' >> $(INDEX_FILE); \
	done
	@echo "main $(INDEX_FILE) created!"
	@yamllint $(INDEX_FILE)

.PHONY: index