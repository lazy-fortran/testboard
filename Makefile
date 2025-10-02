.PHONY: all templates clean

GENERATED_DIR := src/generated
GENERATED_FILE := $(GENERATED_DIR)/embedded_templates.f90

all: templates

templates: $(GENERATED_FILE)

$(GENERATED_FILE): templates/*.html tools/embed_templates.py
	@mkdir -p $(GENERATED_DIR)
	@command -v python >/dev/null 2>&1 && PYTHON=python || PYTHON=python3; \
	$$PYTHON tools/embed_templates.py templates $(GENERATED_FILE)

clean:
	rm -f $(GENERATED_FILE)
